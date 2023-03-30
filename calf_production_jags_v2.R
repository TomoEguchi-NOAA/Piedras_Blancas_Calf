
# This script is a modified version of "Running Individual Year Model.R"
# I inherited it from Josh Stewart in early 2022.

# In this version (v2), I added years into the model.
# Strangely, the Total.Calves tally increase as the number of 
# datasets increases... running n.years = 1 has the lowest
# total counts. As n.years increases, the numbers increase for 
# all years. I can't figure out what is going on here because
# the years are independent of each other, as far as I can tell
# in this model.

# Decided to use v1 for each dataset. I have to get
# to the bottom of this. 2022-05-04

rm(list=ls())
library(jagsUI)
library(tidyverse)
library(lubridate)
library(bayesplot)

source("Piedras_Blancas_fcns.R")

#FILES <- list.files(pattern = ".csv$")
data.path <- "data/Formatted Annual Data/"
FILES <- list.files(path = data.path, 
                    pattern = "Formatted.csv")

MCMC.params <- list(n.samples = 80000,
                    n.thin = 80,
                    n.burnin = 40000,
                    n.chains = 3)

n.samples <- MCMC.params$n.chains * ((MCMC.params$n.samples - MCMC.params$n.burnin)/MCMC.params$n.thin)

jags.params <- c("count.true",
                "lambda",
                "p.obs.corr",
                "p.obs",
                "Total.Calves",
                "loglik")
# get data
count.obs <- effort <- week <- n.obs <- n.weeks <- list()

for(i in 1:length(FILES)){
  
  data <- read.csv(paste0(data.path, FILES[i]))
  data$Effort[is.na(data$Effort)] <- 0
  
  count.obs[[i]] <- data$Sightings
  effort[[i]] <- data$Effort
  week[[i]] <- data$Week
  n.obs[[i]] <- length(data$Sightings)
  n.weeks[[i]] <- max(data$Week)
  
}

n.obs.vec <- unlist(n.obs)

# prepare data matrices
week.mat <- effort.mat <- count.obs.mat <- matrix(data = NA, 
                                                  nrow = length(FILES),
                                                  ncol = max(n.obs.vec))

years <- vector(mode = "numeric", length = length(FILES))
for (k in 1:length(FILES)){
  years[k] <- as.numeric(str_split(FILES[k], " Formatted.csv")[[1]][1])
  count.obs.mat[k, 1:n.obs[[k]]] <- count.obs[[k]]
  effort.mat[k, 1:n.obs[[k]]] <- effort[[k]]
  week.mat[k, 1:n.obs[[k]]] <- week[[k]]
  
}#k

n.weeks.vec <- unlist(n.weeks)

cumsum.count <- apply(count.obs.mat, MARGIN = 1, FUN = cumsum) %>% t()

count.obs.long.df <- data.frame(count.obs.mat ) %>%
  pivot_longer(everything(), names_to = "name", values_to = "counts") %>%
  select(-name) %>%
  mutate(year = rep(years, each = ncol(count.obs.mat)),
         day = rep(1:ncol(count.obs.mat), nrow(count.obs.mat)))

cumsum.count.obs.long.df <- data.frame(cumsum.count) %>%
  pivot_longer(everything(), names_to = "name", values_to = "counts") %>%
  select(-name) %>%
  mutate(year = rep(years, each = ncol(count.obs.mat)),
         day = rep(1:ncol(count.obs.mat), nrow(count.obs.mat)))

jags.data <- list(count.obs = count.obs.mat,
                  effort = effort.mat,
                  week = week.mat,
                  n.obs = n.obs.vec,
                  n.weeks = n.weeks.vec,
                  n.years = nrow(count.obs.mat))

if (!file.exists("RData/calf_estimates_v2.rds")){
  jm <- jags(jags.data,
             inits = NULL,
             parameters.to.save= jags.params,
             "models/GWCalfCount_v2.jags", 
             n.chains = MCMC.params$n.chains,
             n.burnin = MCMC.params$n.burnin,
             n.thin = MCMC.params$n.thin,
             n.iter = MCMC.params$n.samples,
             DIC = T, parallel=T)
  
  # the numbers of data points are not equal among years so
  # I have to make a matrix of log likelihood values without
  # NAs and provide array index to relative_eff()
  
  # samples from jags are stored in here:
  # it is a list of length = the number of chains
  samples.jags <- jm$samples
  dim.names <- lapply(samples.jags,
                      FUN = function(x) attr(x, "dimnames")[[2]])

  loglik.idx <- lapply(dim.names, 
                       FUN = function(x) grep("loglik", x))
  
  loglik.samples.list <- lapply(samples.jags, 
                                FUN = function(x) x[, loglik.idx[[1]]])
  
  loglik.samples.mat <- do.call("rbind", loglik.samples.list)  
  
  chain_ID <- rep(1:MCMC.params$n.chains,
                  each = nrow(samples.jags[[1]]))
  
  # The following (loglik) has the dimension of 
  # the number of total samples x
  # the number of years x
  # the maximum number of data points
  # loglik <- jm$sims.list$loglik
  # First chain is from 1:n.samples, 
  # second chains is (n.samples+1):(2*n.samples), etc
  # This can be seen from doing something like:
  # samples.jags[[1]][1, grep("loglik\\[1,100", dim.names[[1]])]
  # loglik[1,1,100]
  # samples.jags[[2]][1, grep("loglik\\[1,321", dim.names[[2]])]
  # loglik[501,1,321]

  Reff <- relative_eff(exp(loglik.samples.mat),
                       chain_id = chain_ID,
                       cores = MCMC.params$n.chains)
  
  loo.out <- loo(loglik.samples.mat, 
                 r_eff = Reff, 
                 cores = MCMC.params$n.chains)
  
  jm.MCMC <- list(DIC = jm$DIC,
                  loglik.obs = loglik.samples.mat,
                  Reff = Reff,
                  Rhat = Rhat,
                  loo.out = loo.out)
  
  jm.out <- list(jm = jm,
                 jags.data = jags.data,
                 MCMC.params = MCMC.params,
                 MCMC.diag = jm.MCMC,
                 run.date = Sys.Date(),
                 env = Sys.getenv())
  saveRDS(jm.out,
          file = "RData/calf_estimates_v2.rds")
  
} else {
  jm.out <- readRDS("RData/calf_estimates_v2.rds")
  jm <- jm.out$jm
}

Estimates <- data.frame(Mean = jm.out$jm$mean$Total.Calves,
                        Median = jm.out$jm$q50$Total.Calves,
                        LCL =  jm.out$jm$q2.5$Total.Calves,
                        UCL = jm.out$jm$q97.5$Total.Calves)

Estimates$Year <- years
Estimates$Method <- "v2"

write.csv(Estimates,
          "data/Calf Estimates v2.csv",
          row.names = F)





# This script is a modified version of "Running Individual Year Model.R"
# I inherited it from Josh Stewart in early 2022.


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

n.samples <- MCMC.params$n.chains * 
  ((MCMC.params$n.samples - MCMC.params$n.burnin)/MCMC.params$n.thin)

jags.params <- c("count.true",
                "lambda",
                "p.obs.corr",
                "p.obs",
                "Total.Calves",
                "loglik")

# get data
count.obs <- effort <- week <- n.obs <- n.weeks <- list()
years <- vector(mode = "numeric", length = length(FILES))
jm.out <- list()
for(i in 1:length(FILES)){
  
  
  data <- read.csv(paste0(data.path, FILES[i]))
  data$Effort[is.na(data$Effort)] <- 0
  years[i] <- as.numeric(str_split(FILES[i], " Formatted.csv")[[1]][1])
  
  jags.data <- list(count.obs = data$Sightings,
                    effort = data$Effort,
                    week = data$Week,
                    n.obs = length(data$Sightings),
                    n.weeks = max(data$Week))
  
  if (!file.exists(paste0("RData/calf_estimates_v1_", years[i], "_Aug2022b.rds"))){
    jm <- jags(jags.data,
               inits = NULL,
               parameters.to.save= jags.params,
               "models/GWCalfCount_v1.jags", 
               n.chains = MCMC.params$n.chains,
               n.burnin = MCMC.params$n.burnin,
               n.thin = MCMC.params$n.thin,
               n.iter = MCMC.params$n.samples,
               DIC = T, parallel=T)

    # This function is in Piedras_Blancas_fcns.R
    jm.MCMC <- MCMC.diag(jm = jm, MCMC.params = MCMC.params)
    
    jm.out[[i]] <- list(jm = jm,
                        MCMC.diag = jm.MCMC,
                        jags.data = jags.data,
                        MCMC.params = MCMC.params,
                        run.date = Sys.Date())     
    
    saveRDS(jm.out[[i]],
            file = paste0("RData/calf_estimates_v1_", years[i], "_Aug2022b.rds"))
    
  } else {
    jm.out[[i]] <- readRDS(paste0("RData/calf_estimates_v1_", years[i], "_Aug2022b.rds"))
    #jm <- jm.out$jm
  }
}

stats.total.calves <- lapply(jm.out, 
                             FUN = function(x){
                               Mean <- x$jm$mean$Total.Calves
                               Median <- x$jm$q50$Total.Calves
                               SD <- x$jm$sd$Total.Calves
                               LCL <- x$jm$q2.5$Total.Calves
                               UCL <- x$jm$q97.5$Total.Calves
                               
                               return(data.frame(Mean = Mean,
                                                 Median = Median,
                                                 SD = SD,
                                                 LCL = LCL,
                                                 UCL = UCL))
                             })

Estimates <- do.call(rbind, stats.total.calves)
Estimates$Year <- years
Estimates$Method <- "v1"

write.csv(Estimates,
          "data/Calf Estimates v1 Aug2022b.csv",
          row.names = F)

Estimates_1994_2021 <- read.csv(file = "data/Updated Calf Estimates 1994-2019.csv")
#Estimates_2021 <- read.csv(file = "data/Updated Calf Estimates 2021.csv")

stats.1994.2021 <- data.frame(Mean = apply(Estimates_1994_2019, 
                                           FUN = mean, MARGIN = 2),
                              Median = apply(Estimates_1994_2019,
                                             FUN = median, MARGIN = 2),
                            Var = apply(Estimates_1994_2019,
                                         FUN = var, MARGIN = 2),
                            LCL = apply(Estimates_1994_2019,
                                        FUN = quantile, MARGIN = 2, 0.025),
                            UCL = apply(Estimates_1994_2019,
                                        FUN = quantile, MARGIN = 2, 0.975),
                            Year = years[1:27]) %>%
  mutate(SD = sqrt(Var))


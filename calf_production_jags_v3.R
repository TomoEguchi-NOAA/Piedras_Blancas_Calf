
# This script is a modified version of "Running Individual Year Model.R"
# I inherited it from Josh Stewart in early 2022.

# In this version (v3), I try Poisson likelihood with weekly sum 
# instead of binomial. 

rm(list=ls())
library(jagsUI)
library(tidyverse)
library(lubridate)
library(bayesplot)

source("Piedras_Blancas_fcns.R")

save.figs <- T
#FILES <- list.files(pattern = ".csv$")
data.path <- "data/Formatted Annual Data/"
FILES <- list.files(path = data.path, 
                    pattern = "Formatted.csv")

MCMC.params <- list(n.samples = 500000,
                    n.thin = 100,
                    n.burnin = 300000,
                    n.chains = 3)

n.samples <- MCMC.params$n.chains * ((MCMC.params$n.samples - MCMC.params$n.burnin)/MCMC.params$n.thin)

jags.params <- c("count.true",
                "lambda",
                "p.obs.corr",
                "p.obs",
                "Total.Calves",
                "psi",
                #"beta0", "beta1", "beta2",
                "loglik")

# get data
count.obs <- effort <- week <- n.obs <- n.weeks <- list()
years <- vector(mode = "numeric", length = length(FILES))
jm.out <- list()
i <- 3
for(i in 1:length(FILES)){
  
  data <- read.csv(paste0(data.path, FILES[i]))
  data$Effort[is.na(data$Effort)] <- 0
  data$MaxEffort <- 3
  years[i] <- as.numeric(str_split(FILES[i], " Formatted.csv")[[1]][1])
  
  effort <- sum.counts <- vector(mode = "numeric", length = max(data$Week))
  for (k in 1:max(data$Week)){
    sum.counts[k] <- data %>% filter(Week == k) %>%
      summarize(counts = sum(Sightings)) %>% pull(counts)
    effort[k] <- data %>% filter(Week == k) %>%
      summarize(effort = sum(Effort),
                max.effort = sum(MaxEffort),
                prop.effort = effort/max.effort) %>% pull(prop.effort)
  }
  
  jags.data <- list(count.obs = sum.counts,
                    prop.effort = effort,
                    n.weeks = max(data$Week))
  
  if (!file.exists(paste0("RData/calf_estimates_v3_", years[i], ".rds"))){
    jm <- jags(jags.data,
               inits = NULL,
               parameters.to.save= jags.params,
               "models/GWCalfCount_v3.jags", 
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
            file = paste0("RData/calf_estimates_v3_", years[i], ".rds"))
    
  } else {
    jm.out[[i]] <- readRDS(paste0("RData/calf_estimates_v3_", years[i], ".rds"))
    #jm <- jm.out$jm
  }
}


Mean <- Median <- LCL <- UCL <- vector(mode = "numeric", length = length(FILES))
stats.total.calves <- lapply(jm.out, FUN = function(x){
  Mean <- x$jm$mean$Total.Calves
  Median <- x$jm$q50$Total.Calves
  LCL <- x$jm$q2.5$Total.Calves
  UCL <- x$jm$q97.5$Total.Calves
  
  return(data.frame(Mean = Mean,
                    Median = Median,
                    LCL = LCL,
                    UCL = UCL))
})

Estimates <- do.call(rbind, stats.total.calves)
Estimates$Year <- years
Estimates$Method <- "v3"

write.csv(Estimates,
          "data/Calf Estimates v3.csv",
          row.names = F)

#Compare Wayne's estimates and new estimates
WayneAll <- read.csv("data/Calf Production Wayne Estimates.csv")
WayneAll %>% filter(!is.na(Effort)) %>%
  mutate(Method = "Perryman",
         LCL = Estimate - SE * 1.96,
         UCL = Estimate + SE * 1.96,
         Mean = Estimate) -> WayneShort

estimates.PandV3 <- rbind(Estimates %>% select(Year, Mean, LCL, UCL, Method),
                         WayneShort %>% select(Year, Mean, LCL, UCL, Method))

p.PvsV3 <- ggplot(data = estimates.PandV3) + 
  geom_point(aes(x = Year, 
                 y = Mean, 
                 color = Method)) +
  geom_ribbon(aes(x = Year, 
                  ymin = LCL, ymax = UCL, fill = Method),
              alpha = 0.4)+
  labs(title = "Perryman vs V3 (weekly + Poisson)")

if (save.figs)
  ggsave(p.PvsV3, filename = "figures/PVsV3.png",
         device = "png", dpi = 600)

# compare to V1
# Estimates.samples.V1 <- read.csv("data/Updated Calf Estimates 1994-2019.csv")
# 
# Estimates.V1 <- apply(Estimates.samples.V1, MARGIN = 2,
#                       FUN = function(x) {
#                         qtiles <- quantile(x, c(0.025, 0.5, 0.975))
#                         mean <- mean(x)
#                         return(c(mean, qtiles))}) %>%
#   t() %>% data.frame() %>%
#   mutate(Year = years,
#          Method = "V1")

Estimates.V1 <- read.csv("data/Calf Estimates v1.csv")
# colnames(Estimates.V1) <- c("Mean", "LCL", "Median", "UCL", "Year", "Method")

estimates.V1andV3 <- rbind(Estimates %>% select(Year, Mean, LCL, UCL, Method),
                           Estimates.V1 %>% select(Year, Mean, LCL, UCL, Method))

p.V1vsV3 <- ggplot(data = estimates.V1andV3) + 
  geom_point(aes(x = Year, 
                 y = Mean, 
                 color = Method)) +
  geom_ribbon(aes(x = Year, 
                  ymin = LCL, ymax = UCL, fill = Method),
              alpha = 0.4) +
  labs(title = "V1 (Stewart) vs V3 (weekly + Poisson)")

if (save.figs)
  ggsave(p.V1vsV3, filename = "figures/V1VsV3.png",
         device = "png", dpi = 600)

# They are identical (almost)

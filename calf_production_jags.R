
# This script is a modified version of "Running Individual Year Model.R"
# I inherited it from Josh Stewart in early 2022.

# In this version, all models can be run by specifying the model name (v1 - vy) at the beginning.

V7 IS INCOMPLETE. NEEDS TO BE FIXED. 2023-05-19

rm(list=ls())
library(jagsUI)
library(tidyverse)
library(lubridate)
library(bayesplot)

source("Piedras_Blancas_fcns.R")

save.file <- T
model <- "v7"

#FILES <- list.files(pattern = ".csv$")
#data.path <- "data/Formatted Annual Data/"
data.path <- "data/Formatted Annual Data v2/"
#data.path <- "data/Formatted Annual Data/"
FILES <- list.files(path = data.path, 
                    pattern = "Formatted.csv")

MCMC.params <- list(n.samples = 80000,
                    n.thin = 80,
                    n.burnin = 40000,
                    n.chains = 3)

n.samples <- MCMC.params$n.chains * ((MCMC.params$n.samples - MCMC.params$n.burnin)/MCMC.params$n.thin)

jags.params <- c("count.true",
                "lambda",
                "beta1", "beta2", "eps",
                "p.obs.corr",
                "p.obs",
                "Total.Calves",
                "S1", "S2", "P", "K", "Max",
                "loglik")

if (model != "V7"){
  # get data
  count.obs <- effort <- week <- n.obs <- n.weeks <- list()
  years <- vector(mode = "numeric", length = length(FILES))
  jm.out <- list()
  for(i in 1:length(FILES)){
    
    data <- read.csv(paste0(data.path, FILES[i]))
    data$Effort[is.na(data$Effort)] <- 0
    data$Sightings[data$Effort == 0] <- 0  # no effort, no sightings - error in 1997-04-14, shift 4 - check for errors
    years[i] <- as.numeric(str_split(FILES[i], " Formatted.csv")[[1]][1])
    
    jags.data <- list(count.obs = data$Sightings,
                      effort = data$Effort,
                      week = data$Week,
                      n.obs = length(data$Sightings),
                      n.weeks = max(data$Week),
                      weekly.max = data %>% 
                        group_by(Week) %>% 
                        summarize(weekly.max = max(Sightings)) %>% 
                        select (weekly.max) %>% 
                        as.vector() %>%
                        unlist() %>% 
                        unname())
    
    if (!file.exists(paste0("RData/calf_estimates_", model, "_", years[i], ".rds"))){
      jm <- jags(jags.data,
                 inits = NULL,
                 parameters.to.save= jags.params,
                 paste0("models/GWCalfCount_", model, ".jags"), 
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
              file = paste0("RData/calf_estimates_", model, "_", years[i], ".rds"))
      
    } else {
      jm.out[[i]] <- readRDS(paste0("RData/calf_estimates_", model, "_", years[i], ".rds"))
      #jm <- jm.out$jm
    }
  
  }
} else {
  #count.obs <- effort <- week <- n.obs <- n.weeks <- list()
  years <- vector(mode = "numeric", length = length(FILES))
  data <- vector(mode = "list", length = length(FILES))
  jm.out <- list()
  for(i in 1:length(FILES)){
    
    data[[i]] <- read.csv(paste0(data.path, FILES[i]))
    data[[i]]$Effort[is.na(data[[i]]$Effort)] <- 0
    data[[i]]$Sightings[data[[i]]$Effort == 0] <- 0  
    years[i] <- as.numeric(str_split(FILES[i], " Formatted.csv")[[1]][1])
    
    # Need to add a sequential day variable for each year
    data[[i]] %>%
      mutate(Date.date = as.Date(Date),
             Date.dif = Date.date - min(Date.date) + 1,
             Date.num = as.numeric(Date.dif)) -> data[[i]] 
      
  }
  
  n.obs <- lapply(data, FUN = nrow) %>% unlist()
  n.days <- lapply(data, FUN = function(x) { max(x$Date.num)}) %>%
    unlist()
  
  # maximum number of sighting counts
  max.length <- max(n.obs)
  
  # Need to make all years the same number of rows 
  sightings.list <- lapply(data, FUN = function(x) x$Sightings)
  effort.list <- lapply(data, FUN = function(x) x$Effort)
  day.list <- lapply(data, FUN = function(x) x$Date.num)
  
  for (k in 1:length(sightings.list)){
    sightings.list[[k]] <- c(sightings.list[[k]], 
                             rep(0, max.length - length(sightings.list[[k]])))
    effort.list[[k]] <- c(effort.list[[k]], 
                             rep(0, max.length - length(effort.list[[k]])))
    day.list[[k]] <- c(day.list[[k]], 
                       rep(0, max.length - length(day.list[[k]])))
    
  }
  
  # convert data into matrices
  jags.data <- list(count.obs = array(unlist(sightings.list), 
                                      dim = c(max.length, 
                                              length(sightings.list))),
                    effort = array(unlist(effort.list), 
                                   dim = c(max.length, 
                                           length(effort.list))),
                    day = array(unlist(day.list), 
                                dim = c(max.length, 
                                        length(effort.list))),
                    n.obs = n.obs,
                    n.years = length(effort.list),
                    n.days = n.days)
  
  if (!file.exists(paste0("RData/calf_estimates_", model, ".rds"))){
    jm <- jags(jags.data,
               inits = NULL,
               parameters.to.save= jags.params,
               paste0("models/GWCalfCount_", model, ".jags"), 
               n.chains = MCMC.params$n.chains,
               n.burnin = MCMC.params$n.burnin,
               n.thin = MCMC.params$n.thin,
               n.iter = MCMC.params$n.samples,
               DIC = T, parallel=T)
    
    # This function is in Piedras_Blancas_fcns.R
    jm.v7.MCMC <- MCMC.diag(jm = jm, MCMC.params = MCMC.params)
    
    jm.out.v7 <- list(jm = jm,
                      MCMC.diag = jm.MCMC,
                      jags.data = jags.data,
                      MCMC.params = MCMC.params,
                      run.date = Sys.Date(),
                      Sys.info = Sys.getenv())     
    
    saveRDS(jm.out.v7,
            file = paste0("RData/calf_estimates_", model, ".rds"))
    
  } else {
    jm.out.v7 <- readRDS(paste0("RData/calf_estimates_", model, ".rds"))
    #jm <- jm.out$jm
  }
  
}

stats.total.calves <- lapply(jm.out, 
                             FUN = function(x){
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
Estimates$Method <- model
#Estimates$Sys_env <- Sys.getenv()

if (save.file)
  write.csv(Estimates,
            paste0("data/Calf Estimates ", model, ".csv"),
            row.names = F)




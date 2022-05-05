library(tidyr)
library(dplyr)
library(lubridate)



#########################################################
# FEB 2020 FINAL FORMATTING
# WRITE FILES WITH FILLED 0s AND WEEK STARTING AT 1
# FOR INDIVIDUAL RUNS OF EACH YEAR
#########################################################


FILES <- list.files(pattern = "[.csv]$")

for(i in 1:length(FILES)){
  data <- read.csv(FILES[i],na.strings="")
  data <- data %>% fill(Date)
  data$Sightings[is.na(data$Sightings)] <- 0
  
  #Formatted_Date <- dmy(paste0(data$Date,"-",substr(FILES[i],1,4)))
  #data$Week <- week(Formatted_Date)
  
  # Surveys from 1994-2005 had 1 weekend day in between 6-day weeks
  
  if(substr(FILES[i],1,4) %in% c(1993:2003,2005)){
    FROMS <- seq(from=1,to=dim(data)[1],by=24)
    TOS <- seq(from=24,to=dim(data)[1],by=24)
    if(length(TOS) < length(FROMS)){TOS[length(FROMS)] <- length(data$Week)} #Not every survey is a multiple of 6 days...
    
    interm_data <- rbind(data[FROMS[1]:TOS[1],],
                         data.frame(Week=rep((data$Week[TOS[1]]),4),
                                    Date=rep(NA,4),
                                    Effort=rep(0,4),
                                    Sightings=rep(0,4)))
    
    for(j in 2:length(FROMS)){
      
      interm_data <- rbind(interm_data,
                           data[FROMS[j]:TOS[j],],
                           data.frame(Week=rep((data$Week[TOS[j]]),4),
                                      Date=rep(NA,4),
                                      Effort=rep(0,4),
                                      Sightings=rep(0,4)))
      
    }#j
  }#if 1994:2005
  
  # Surveys from 2006 onwards had 2 weekend days in between 5-day weeks
  
  if(substr(FILES[i],1,4) %in% c(2004,2006:2021)){
    FROMS <- seq(from=1,to=dim(data)[1],by=20)
    TOS <- seq(from=20,to=dim(data)[1],by=20)
    if(length(TOS) < length(FROMS)){TOS[length(FROMS)] <- length(data$Week)} #Not every survey is a multiple of 5 days...
    
    
    interm_data <- rbind(data[FROMS[1]:TOS[1],],
                         data.frame(Week=rep((data$Week[TOS[1]]),8),
                                    Date=rep(NA,8),
                                    Effort=rep(0,8),
                                    Sightings=rep(0,8)))
    
    for(j in 2:length(FROMS)){
      
      interm_data <- rbind(interm_data,
                           data[FROMS[j]:TOS[j],],
                           data.frame(Week=rep((data$Week[TOS[j]]),8),
                                      Date=rep(NA,8),
                                      Effort=rep(0,8),
                                      Sightings=rep(0,8)))
      
    }#j
  }#if 2006:2019
  
  if(length(interm_data$Week)/28 < ceiling(length(interm_data$Week)/28)){
    objective <- ceiling(length(interm_data$Week)/28)*28
    addedrows <- objective - length(interm_data$Week)
    interm_data <- rbind(interm_data,
                         data.frame(Week=rep(data$Week[length(data$Week)],addedrows),
                                    Date=rep(NA,addedrows),
                                    Effort=rep(0,addedrows),
                                    Sightings=rep(0,addedrows)))
    
  }#if not full 7 days in final week
  
  wknd_data <- interm_data
  
  #Add in 4 nighttime 3 hr periods in between each 4 daytime periods
  W_FROMS <- seq(from=1,to=dim(wknd_data)[1],by=4)
  W_TOS <- seq(from=4,to=dim(wknd_data)[1],by=4)
  
  w_interm_data <- rbind(wknd_data[W_FROMS[1]:W_TOS[1],],
                         data.frame(Week=rep(data[W_FROMS[1],1],4),
                                    Date=rep(data[W_FROMS[1],2],4),
                                    Effort=rep(0,4),
                                    Sightings=rep(0,4)))
  
  for(k in 2:length(W_FROMS)){
    
    w_interm_data <- rbind(w_interm_data,
                           wknd_data[W_FROMS[k]:W_TOS[k],],
                           data.frame(Week=rep(wknd_data[W_FROMS[k],1],4),
                                      Date=rep(wknd_data[W_FROMS[k],2],4),
                                      Effort=rep(0,4),
                                      Sightings=rep(0,4)))
  }#k
  
  if(mean(hist(w_interm_data$Week,breaks=0:max(w_interm_data$Week),plot=F)$counts) == 56){  
    write.csv(w_interm_data,paste0(substr(FILES[i],1,4)," Formatted.csv"),row.names = F)
  }else(stop("Not All Weeks Have 7 Days"))
  
}#i


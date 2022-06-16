# Modified Josh Stewart's version of Collating and Formatting Passdown.R
# Some issues with dropped years for recent year datasets. 

# Changed to a function so that I can just run on one file at a time for 2022.

# More major changes are found in Extract Excel data.Rmd, which uses the original
# Excel files to extract data rather than converting them into a text file.

# TE

rm(list=ls())
library(tidyr)
library(dplyr)
library(lubridate)

fill.data.pre2006 <- function(data){
  # 24 = 6 days x 4 shifts per day
  FROMS <- seq(from=1, to=dim(data)[1], by=24)
  TOS <- seq(from=24, to=dim(data)[1], by=24)
  if(length(TOS) < length(FROMS)){
    TOS[length(FROMS)] <- length(data$Week)
  } #Not every survey is a multiple of 6 days...
  
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
  return(interm_data)
}

fill.data.post2006 <- function(data){
  # 20 = 5 days x 4 shifts per day
  FROMS <- seq(from=1,to=dim(data)[1],by=20)
  TOS <- seq(from=20,to=dim(data)[1],by=20)
  if(length(TOS) < length(FROMS)){
    TOS[length(FROMS)] <- length(data$Week)} #Not every survey is a multiple of 5 days...
  
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
  return(interm_data)  
}

#########################################################
# FEB 2020 FINAL FORMATTING
# WRITE FILES WITH FILLED 0s AND WEEK STARTING AT 1
# FOR INDIVIDUAL RUNS OF EACH YEAR
#########################################################

path <- "data/Raw data"
FILES <- list.files(path = path, pattern = "[.csv]$")
i <- 28
for(i in 1:length(FILES)){
  data <- read.csv(paste0(path, "/", FILES[i]), na.strings="")
  data <- data %>% fill(Date)
  data$Sightings[is.na(data$Sightings)] <- 0
  data$Effort[is.na(data$Effort)] <- 0
  
  # if there is no "week" column (e.g., 2022), needs to be filled in
  # This has been fixed - data are extracted in Extract Excel data.Rmd
  # and saved as "raw data" which is used here. So, commenting these lines
  # out
  # if (sum(str_detect(names(data), "Week")) == 0){
  #   # The format of the date column is assumed to be dd-mmm-yy... this works for
  #   # 2022. This may have to be modified to accommodate other format in the
  #   # future. For now, go with this. 
  #   week.since.Jan <- week(as.Date(data$Date, format = "%d-%b-%y"))
  #   data$Week <- week.since.Jan - min(week.since.Jan) + 1
  # }
  #Formatted_Date <- dmy(paste0(data$Date,"-",substr(FILES[i],1,4)))
  #data$Week <- week(Formatted_Date)
  
  # Surveys from 1994-2005 had 1 weekend day in between 6-day weeks
  if(substr(FILES[i],1,4) %in% c(1993:2003,2005)){
    interm_data <- fill.data.pre2006(data)

  }#if 1994:2005
  
  # Surveys from 2006 onwards had 2 weekend days in between 5-day weeks
  if(substr(FILES[i],1,4) %in% c(2004,2006:2021)){
    interm_data <- fill.data.post2006(data)
    
  }#if 2006:2019
  
  # for 2022 and beyond, I add all dates in Extract Excel data.Rmd so no
  # need to add extra dates. 
  
  interm_data <- data
  
  # Why 28? There are maximum of 4 shifts per day. 4 x 7 days = 28
  # If there were 7 days in the final week, length() and ceiling(length())
  # would result in the same number.  
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
  W_FROMS <- seq(from=1, to=dim(wknd_data)[1], by=4)
  W_TOS <- seq(from=4, to=dim(wknd_data)[1], by=4)
  
  w_interm_data <- rbind(wknd_data[W_FROMS[1]:W_TOS[1],],
                         data.frame(Week=rep(data[W_FROMS[1], "Week"],4),
                                    Date=rep(data[W_FROMS[1], "Date"],4),
                                    Effort=rep(0,4),
                                    Sightings=rep(0,4)))
  
  for(k in 2:length(W_FROMS)){
    
    w_interm_data <- rbind(w_interm_data,
                           wknd_data[W_FROMS[k]:W_TOS[k],],
                           data.frame(Week=rep(wknd_data[W_FROMS[k], "Week"],4),
                                      Date=rep(wknd_data[W_FROMS[k], "Date"],4),
                                      Effort=rep(0,4),
                                      Sightings=rep(0,4)))
  }#k
  
  if(mean(hist(w_interm_data$Week,
               breaks=0:max(w_interm_data$Week),
               plot=F)$counts) == 56){  
    write.csv(w_interm_data, 
              paste0("data/Formatted Annual Data/",
                     substr(FILES[i],1,4),
                     " Formatted.csv"),
              row.names = F)
  } else {
    stop("Not All Weeks Have 7 Days")
  }
}#i


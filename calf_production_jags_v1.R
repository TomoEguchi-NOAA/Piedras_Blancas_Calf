
# This script is a modified version of "Running Individual Year Model.R"
# I inherited it from Josh Stewart in early 2022. 



library(R2jags)
library(tidyr)
library(dplyr)
library(lubridate)

#FILES <- list.files(pattern = ".csv$")
data.path <- "data/Formatted Annual Data/"
FILES <- list.files(path = data.path, 
                    pattern = "Formatted.csv")

Estimates <- matrix(nrow=1500,ncol=length(FILES))
Lambdas <- list()

ni <- 80000
nt <- 80
nb <- 40000
nc <- 3

parameters <- c("count.true",
                "lambda",
                "p.obs.corr",
                "p.obs",
                "Total.Calves")


for(i in 1:length(FILES)){
  
  data <- read.csv(paste0(data.path, FILES[i]))
  data$Effort[is.na(data$Effort)] <- 0
  
#  No.Weeks <- floor(length(data$Week)/56)
#  Weeks <- rep(1,56)
#  for(w in 2:No.Weeks){
#    Weeks <- c(Weeks,rep(w,56))
#  }
#  Extras <- length(data$Week) - length(Weeks)
#  Weeks <- c(Weeks,rep(No.Weeks+1,Extras))
  
  jags.data <- list(count.obs = data$Sightings,
                    effort = data$Effort,
#                    week = Weeks,
                    week = data$Week,
                    n.obs = length(data$Sightings),
#                    n.weeks = max(Weeks))
                    n.weeks = max(data$Week))
  
  GWCalves <- jags(jags.data, 
                   inits=NULL, 
                   parameters, 
                   "models/GWCalfCount_v1.jags", 
                   n.chains = nc, 
                   n.thin = nt, 
                   n.iter = ni, 
                   n.burnin = nb)
  
  attach.jags(GWCalves)
  Estimates[,i] <- Total.Calves
  Lambdas[[i]] <- lambda
  detach.jags()

}

write.csv(Estimates,
          "Updated Calf Estimates 1994-2019.csv",
          row.names = F)
write.csv(Estimates[,27],
          "Updated Calf Estimates 2021.csv",
          row.names = F)


save.image("RData/Annual Calf Estimates and Weekly Lambdas 1994-2019.RData")
save.image("RData/Annual Calf Estimates and Weekly Lambdas 2021.RData")


#Compare Wayne's estimates and new estimates

WayneAll <- read.csv("data/Calf Production Wayne Estimates.csv")

#Estimates <- read.csv("/Users/joshuastewart/NOAA/Research/Gray Whale Calf Counts/Updated Calf Estimates Sept 2019.csv")
#Filter out 80-81
Wayne <- dplyr::filter(WayneAll,Year %in% 1994:2019)

plot(x=Wayne$Year,
     y=Wayne$Estimate,
     pch=19,
     ylim=c(0,2000),type='b',xlab="Year",ylab="Estimate",main="")

segments(x0=Wayne$Year,
         y0=Wayne$Estimate-Wayne$SE*1.96,
         y1=Wayne$Estimate+Wayne$SE*1.96)
end.yr <- 2020
points(x=(1994:end.yr)+0.2,
       y=apply(Estimates,2,median),
       type='b',pch=19,col="red")

segments(x0=(1994:end.yr)+0.2,
         y0=apply(Estimates,2,quantile,0.025),
         y1=apply(Estimates,2,quantile,0.975),col="red")
  
#Histogram of differences:
hist(apply(Estimates,2,median) - Wayne$Estimate[6:length(Wayne$Estimate)],
     col="lightgrey",
     xlab="Difference",
     main="New Estimates - Original Estimates",
     breaks=10,
     xlim=c(0,200))


#Boxplots of passage rates by year
for(i in 1:length(Lambdas)){
  boxplot(Lambdas[[i]],main=paste0(i+1993," Weekly Passage Rates"),xlab="Week",ylab="Cow-Calf Pairs / 3hr Watch")
}

#1997,2000,2016,2019

UpdatedEstimates <- data.frame(Year=1994:end.yr,
                               Median=apply(Estimates,2,median),
                               LCI=apply(Estimates,2,quantile,0.025),
                               UCI=apply(Estimates,2,quantile,0.975))

Wayne$SEUCI <- Wayne$Estimate+Wayne$SE*1.96
Wayne$SELCI <- Wayne$Estimate-Wayne$SE*1.96

AllEst <- dplyr::left_join(UpdatedEstimates,Wayne)

hist((AllEst$Median-AllEst$Estimate)/AllEst$Estimate,breaks=10,
     col="grey",main="",xlab= "Proportion Increase New vs Old Estimate",xlim=c(0,0.2))
hist((AllEst$Median-AllEst$Estimate),breaks=10,col="grey",
     main="",xlab= "New Median - Old Best Estimate")
hist((AllEst$LCI-AllEst$SELCI),breaks=10,col="grey",
     main="",xlab= "New Lower CI - Old Lower CI")
hist((AllEst$UCI-AllEst$SEUCI),breaks=10,col="grey",
     main="",xlab= "New Upper CI - Old Upper CI")


ReportTable <- AllEst[,c(1,7,2,9,4,10,3)]

write.csv(ReportTable,"Output for Table 1.csv",row.names = F)

# Testing different sampling regimes

SamplingTests <- matrix(nrow=1500,ncol=4)

# This is not good programming. We should specify "2017" while looking for this file.
# Fix it. 
data <- read.csv(paste0(data.path, FILES[24])) #2017 since it's a middle-of-the-road year
data$Effort[is.na(data$Effort)] <- 0


LO25 <- sample(which(data$Effort>0), 38) #randomly sample 25% of on-effort survey periods to remove
LO50 <- sample(which(data$Effort>0), 75) #50%
LO75 <- sample(which(data$Effort>0), 113) #75%

dataLO25 <- data
dataLO25$Effort[LO25] <- 0 #set those effort & sightings to 0
dataLO25$Sightings[LO25] <- 0

dataLO50 <- data
dataLO50$Effort[LO50] <- 0
dataLO50$Sightings[LO50] <- 0

dataLO75 <- data
dataLO75$Effort[LO75] <- 0
dataLO75$Sightings[LO75] <- 0


DATA <- dataLO75

jags.data <- list(count.obs = DATA$Sightings,
                  effort = DATA$Effort,
                  week = DATA$Week,
                  n.obs = length(DATA$Sightings),
                  n.weeks = max(DATA$Week))



GWCalves75 <- jags(jags.data, 
                   inits=NULL, 
                   parameters, 
                   "models/GWCalfCount_v1.jags", 
                   n.chains = nc, 
                   n.thin = nt, 
                   n.iter = ni, 
                   n.burnin = nb, working.directory = getwd())

attach.jags(GWCalves)

SamplingTests[,4] <- Total.Calves

boxplot(SamplingTests, main="2017 Calf Estimate (Varying Effort)", 
        names=c("Full","75%","50%","25%"), ylim=c(0,3000))


# 2021 tech memo stuff

FILES <- list.files("data/Formatted Annual Data/",
                    pattern = "Formatted.csv")

load("RData/Annual Calf Estimates and Weekly Lambdas 2021.RData")

Estimates2021 <- Estimates
Lambdas2021 <- Lambdas

load("RData/Annual Calf Estimates and Weekly Lambdas 1994-2019.RData")

AllEstimates <- cbind(Estimates,Estimates2021[,27])
AllLambdas <- c(Lambdas,Lambdas2021)
EffortHrs <- NULL
TotalSightings <- NULL

for(i in 1:length(FILES)){
  
  data <- read.csv(paste0(data.path, FILES[i]))
  
  EffortHrs[i] <- sum(data$Effort,na.rm=T)
  TotalSightings[i] <- sum(data$Sightings)
  
}

EffSightDF <- data.frame(Year=c(1994:2019,2021),
                         Effort=round(EffortHrs,0),
                         Sightings=TotalSightings)



#Compare Wayne's estimates and new estimates

WayneAll <- read.csv("data/Calf Production Wayne Estimates.csv")

WayneAll <- rbind(WayneAll[1:28,],NA,WayneAll[29,])
WayneAll[29,1] <- 2020

#Estimates <- read.csv("/Users/joshuastewart/NOAA/Research/Gray Whale Calf Counts/Updated Calf Estimates Sept 2019.csv")
#Filter out 80-81
Wayne <- dplyr::filter(WayneAll,Year %in% 1994:2021)

plot(x=Wayne$Year,y=Wayne$Estimate,pch=19,
     ylim=c(0,2000),type='b',
     xlab="Year",ylab="Estimate",main="")
segments(x0=Wayne$Year,y0=Wayne$Estimate-Wayne$SE*1.96,
         y1=Wayne$Estimate+Wayne$SE*1.96)
points(x=(1994:end.yr)+0.2,
       y=apply(Estimates,2,median),
       type='b',pch=19,col="red")
segments(x0=(1994:end.yr)+0.2,
         y0=apply(Estimates,2,quantile,0.025),
         y1=apply(Estimates,2,quantile,0.975),col="red")

points(x=2021.2,y=median(AllEstimates[,27]),pch=19,col='red')
segments(x0=2021.2,
         y0=quantile(AllEstimates[,27],0.025),
         y1=quantile(AllEstimates[,27],0.975),col='red')

#Histogram of differences:
hist(apply(AllEstimates,2,median) - Wayne$Estimate,
     col="lightgrey",xlab="Difference",
     main="New Estimates - Original Estimates",
     breaks=10,xlim=c(0,200))


#Boxplots of passage rates by year
for(i in 1:length(Lambdas)){
  boxplot(Lambdas[[i]],
          main=paste0(i+1993," Weekly Passage Rates"),
          xlab="Week",
          ylab="Cow-Calf Pairs / 3hr Watch")
}


UpdatedEstimates <- data.frame(Year=c(1994:end.yr,2021),
                               Median=apply(AllEstimates,2,median),
                               LCI=apply(AllEstimates,2,quantile,0.025),
                               UCI=apply(AllEstimates,2,quantile,0.975))

Wayne$SEUCI <- Wayne$Estimate+Wayne$SE*1.96
Wayne$SELCI <- Wayne$Estimate-Wayne$SE*1.96

AllEst <- dplyr::left_join(UpdatedEstimates,filter(Wayne, Year %in% c(1994:end.yr,2021)))

par(mfrow=c(2,2))
hist((AllEst$Median-AllEst$Estimate)/AllEst$Estimate,
     breaks=10,col="grey",main="",
     xlab= "Proportion Increase New vs Old Estimate",
     xlim=c(0,0.2))
hist((AllEst$Median-AllEst$Estimate),
     breaks=10,col="grey",main="",
     xlab= "New Median - Old Best Estimate",
     xlim=c(0,200))
hist((AllEst$LCI-AllEst$SELCI),
     breaks=10,col="grey",main="",
     xlab= "New Lower CI - Old Lower CI",
     xlim=c(-50,200))
hist((AllEst$UCI-AllEst$SEUCI),
     breaks=10,col="grey",main="",
     xlab= "New Upper CI - Old Upper CI")


#1997,2000,2016,2019
boxplot(Lambdas[[4]],main="",xlab="Week",ylab="Cow-Calf Pairs / 3hr Watch")
boxplot(Lambdas[[7]],main="",xlab="Week",ylab="Cow-Calf Pairs / 3hr Watch")
boxplot(Lambdas[[23]],main="",xlab="Week",ylab="Cow-Calf Pairs / 3hr Watch")
boxplot(Lambdas2021[[27]],main="",xlab="Week",ylab="Cow-Calf Pairs / 3hr Watch")


plot(x=c(1994:2019,2021),
     y=apply(Estimates,2,median),
     pch=21,bg="#F8CBAD",
     ylim=c(0,0.105),
     ylab="Calf Production Per Capita",
     xlab="Year", xlim=c(1994,2021),
     type='b')
arrows(x0=c(1994:2019,2021),
       y0=apply(Estimates,2,quantile,0.025),
       y1=apply(Estimates,2,quantile,0.975),
       code=3,angle=90,length=0.05)
points(x=c(1994:2019,2021),
       y=apply(Estimates,2,median),
       pch=21,bg="#F8CBAD")

# Stranding data don't exist
# plot(x=c(1990:2021),
#      y=apply(StrandingsPerCap,2,median),
#      pch=21,bg="#DEEBF7",ylim=c(0,0.009),
#      ylab="Strandings Per Capita",xlab="Year", 
#      xlim=c(1990,2021),type='b')
# 
# arrows(x0=c(1990:2021),
#        y0=apply(StrandingsPerCap,2,quantile,0.025),
#        y1=apply(StrandingsPerCap,2,quantile,0.975),
#        code=3,angle=90,length=0.05)
# points(x=c(1990:2021),
#        y=apply(StrandingsPerCap,2,median),
#        pch=21,bg="#DEEBF7")
# 


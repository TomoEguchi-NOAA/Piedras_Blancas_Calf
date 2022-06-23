
library(rstan)
library(loo)

get.all.data <- function(sheet.name.inshore, sheet.name.offshore,
                         col.types.inshore, col.types.offshore,
                         col.names.inshore, col.names.offshore,
                         Year, dir.name, 
                         xls.file.name.inshore, xls.file.name.offshore,
                         start.time){
  
  data.inshore <- get.data(Year = Year, 
                           dir.name = dir.name, 
                           xls.file.name = xls.file.name.inshore, 
                           sheet.name = sheet.name.inshore,
                           col.types = col.types.inshore, 
                           col.names = col.names.inshore, 
                           area.name = "inshore",
                           start.time = start.time)
  
  data.offshore <- get.data(Year = Year, 
                            dir.name = dir.name, 
                            xls.file.name = xls.file.name.offshore, 
                            sheet.name = sheet.name.offshore,
                            col.types = col.types.offshore, 
                            col.names = col.names.offshore, 
                            area.name = "offshore",
                            start.time = start.time)
  
  data.all <- rbind(data.inshore, data.offshore) %>% 
    filter(!is.na(Date)) %>%
    filter(!is.na(Event)) %>%
    filter(!is.na(Shift)) %>%
    arrange(Date, Minutes_since_T0)
  
  data.all %>% 
    group_by(Date, Shift) %>%
    summarise(Date = min(Date),
              Sea_State = max(SeaState, na.rm = T),
              Vis = max(Vis, na.rm = T),
              Mother_Calf = sum(Mother_Calf, na.rm = T),
              Duration = max(Minutes_since_0000, na.rm = T) - 
                min(Minutes_since_0000, na.rm = T),
              Time_T0 = min(Minutes_since_T0, na.rm = T),
              Time_0000 = min(Minutes_since_0000, na.rm = T)) -> data.shift
  
  return(list(all = data.all,
              shift = data.shift))
}


# col.names has to be in the same order of:
# c("Date", "Event", "Time", "Obs. Code", 
# "Sea State", "Vis. IN", "Cow  / Calf")
# The exact names may be different in each file. Make sure to 
# match them. 

# T0 is the beginning of the first shift (start.time)

get.data <- function(Year, dir.name, xls.file.name, sheet.name,
                     col.types, col.names, area.name, start.time,
                     max.shift){
  # col.names <- c("Date", "Event", "Time", "Obs. Code", "Sea State", 
  #                "Vis. IN", "Cow  / Calf")
  
  data.out <- read_excel(xls.file.name,
                         sheet = sheet.name,
                         col_types = col.types) %>% 
    select(col.names) %>%
    transmute(Date = .data[[col.names[[1]]]],
              Event = .data[[col.names[[2]]]],
              Time = .data[[col.names[[3]]]],
              Minutes_since_T0 = sapply(Time, FUN = char_time2min, start.time),
              Minutes_since_0000 = sapply(Time, FUN = char_time2min),
              Shift = sapply(Minutes_since_0000, FUN = find.shift, max.shift = max.shift),
              Obs = .data[[col.names[[4]]]],
              SeaState = .data[[col.names[[5]]]],
              Vis = .data[[col.names[[6]]]],
              Mother_Calf = .data[[col.names[[7]]]],
              Year = Year,
              Area = area.name)
  
  # Some files contain wrong years...
  years <- year(data.out$Date)
  dif.years <- sum(years - Year, na.rm = T)
  if (dif.years != 0){
    year(data.out$Date) <- Year
  } 

  return(data.out)  
}

# extract MCMC diagnostic statistics, including Rhat, loglikelihood, DIC, and LOOIC
MCMC.diag <- function(jm, MCMC.params){
  n.per.chain <- (MCMC.params$n.samples - MCMC.params$n.burnin)/MCMC.params$n.thin
  
  Rhat <- unlist(lapply(jm$Rhat, FUN = max, na.rm = T))
  
  loglik <- jm$sims.list$loglik
  
  Reff <- relative_eff(exp(loglik), 
                       chain_id = rep(1:MCMC.params$n.chains, 
                                      each = n.per.chain),
                       cores = 1)
  loo.out <- loo(loglik, r_eff = Reff, cores = 1)
  
  return(list(DIC = jm$DIC,
              loglik.obs = loglik,
              Reff = Reff,
              Rhat = Rhat,
              loo.out = loo.out))
  
}


# Function to convert character time (e.g., 1349) to minutes from 
# a particular start time of the day (e.g., 0700). Default is midnight (0000)
# Use as char_time2min(x, origin = "0000"). 
char_time2min <- function(x, origin = "0000"){
  chars.origin <- strsplit(origin, split = "") %>% unlist
  if (length(chars.origin) == 3){
    hr <- as.numeric(chars.origin[1])
    m <- as.numeric(c(chars.origin[2:3]))
    M.0 <- hr * 60 + m[1] * 10 + m[2]
  } else {
    hr <- as.numeric(chars.origin[1:2])
    m <- as.numeric(c(chars.origin[3:4]))
    M.0 <- (hr[1] * 10 + hr[2]) * 60 + m[1] * 10 + m[2]
  }
  
  if (!is.na(x)){
    chars <- strsplit(x, split = "") %>% unlist()
    if (length(chars) == 3){
      hr <- as.numeric(chars[1])
      m <- as.numeric(c(chars[2:3]))
      M.1 <- (hr * 60 + m[1] * 10 + m[2])
    } else {
      hr <- as.numeric(chars[1:2])
      m <- as.numeric(c(chars[3:4]))
      M.1 <- (hr[1] * 10 + hr[2]) * 60 + m[1] * 10 + m[2]
    }
  } else {
    M.1 <- NA
  }
  
  if (!is.na(M.1)){
    if (M.1 >= M.0){
      out <- M.1 - M.0
    } else {
      out <- 1440 - (M.0 - M.1)
    }
    
  } else {
    out <- NA
  }
  
  return(out)
}

# change max.shift if needed. max.shift can be up to 5
find.shift <- function(x0, max.shift = 4){
  x <- as.numeric(x0)
  if (!is.na(x)){
    if (x < 600){
      shift <- "1"
    } else if (x == 600) {
      shift <- "1/2"
    } else if (x > 600 & x < 780) {
      shift <- "2"
    } else if (x == 780){
      shift <- "2/3"
    } else if (x > 780 & x < 960){
      shift <- "3"
    } else if (x == 960){
      shift <- "3/4"
    } else if (x > 960 & x < 1140){
      shift <- "4"
    } else if (x == 1140){
      if (max.shift > 4){
        shift <- "4/5"        
      } else {
        shift <- "4"
      }
    } else if (x > 1140 & x <= 1320){ 
      shift <- "5"
    } else {
      shift <- NA
    }
  } else {
    shift <- NA
  }
  return(shift)
}

# x is a data.frame with at least two fields: Minutes_since_0000
# and Shift - find.shift is used to come up with this
# 1 = START EFFORT
#2 = CHANGE OBSERVERS
#3 = CHANGE SIGHTING CONDITIONS
#4 = GRAY WHALE SIGHTING
#5 = END EFFORT
#6 = OTHER SPECIES SIGHTING

find.effort <- function(x, max.shift){
  all.dates <- unique(x$Date) %>% as.character()
  
  if (max.shift == 4){
    shifts <- c("1", "2", "3", "4")
    shifts.1 <- c("1/2", "2/3", "3/4", NA)
  } else if (max.shift == 5){
    shifts <- c("1", "2", "3", "4", "5")
    shifts.1 <- c("1/2", "2/3", "3/4", "4/5", NA)
  }
  
  out.df <- data.frame(Date = rep(all.dates,
                                  each = length(shifts)),
                       Shift = rep(shifts, 
                                   times = length(unique(data.shift$Date))),
                       Effort = NA)
  
  for (d in 1:length(all.dates)){
    one.day <- filter(x, as.character(Date) == all.dates[d])
    shifts.one.day <- unique(one.day$Shift)
    for (k in 1:length(shifts.one.day)){
      shifts.1.one.day <- shifts.1[str_detect(shifts.1, shifts.one.day[k])]
      if (shifts.one.day[k] < max.shift){
        if (as.numeric(shifts.one.day[k]) > 1){
          one.shift <- one.day %>% filter(Shift == shifts.one.day[k] | 
                                            Shift == shifts.1.one.day[1] | 
                                            Shift == shifts.1.one.day[2])
        } else{
          one.shift <- one.day %>% filter(Shift == shifts.one.day[k] | 
                                            Shift == shifts.1.one.day[1])
        }
      } else {
        one.shift <- one.day %>% filter(Shift == shifts.one.day[k])
      }
      
      if (nrow(one.shift) != 0){
        out.df[as.character(out.df$Date) == all.dates[d] & 
               out.df$Shift == shifts.one.day[k], "Effort"] <- max(one.shift$Minutes_since_0000, na.rm = T) - 
          min(one.shift$Minutes_since_0000, na.rm = T)
        
      }
      
    }
    
  }
  return(out.df)
}

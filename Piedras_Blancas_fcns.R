
library(rstan)
library(loo)

get.all.data <- function(sheet.name.inshore, sheet.name.offshore,
                         col.types.inshore, col.types.offshore,
                         col.names.inshore, col.names.offshore,
                         Year, 
                         xls.file.name.inshore, xls.file.name.offshore,
                         start.time, max.shift){
  
  data.inshore <- get.data(Year = Year, 
                           xls.file.name = xls.file.name.inshore, 
                           sheet.name = sheet.name.inshore,
                           col.types = col.types.inshore, 
                           col.names = col.names.inshore, 
                           start.time = start.time,
                           max.shift = max.shift)
  
  data.offshore <- get.data(Year = Year, 
                            xls.file.name = xls.file.name.offshore, 
                            sheet.name = sheet.name.offshore,
                            col.types = col.types.offshore, 
                            col.names = col.names.offshore, 
                            start.time = start.time,
                            max.shift = max.shift)
  
  data.all <- rbind(data.inshore, data.offshore) %>% 
    arrange(Date.date, Minutes_since_0000)
  return(data.all)
}


# col.names has to be in the same order of:
# c("Date", "Event", "Time", "Obs. Code", 
# "Sea State", "Vis. IN", "Cow  / Calf")
# The exact names may be different in each file. Make sure to 
# match them. 

# T0 is the beginning of the first shift (start.time)

get.data <- function(Year, xls.file.name, sheet.name,
                     col.types, col.names, start.time,
                     max.shift){
  # col.names <- c("Date", "Event", "Time", "Obs. Code", "Sea State", 
  #                "Vis. IN", "Cow  / Calf")
  if (length(col.names) < 8){
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
                Area = "off")
     
  } else {
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
                Area =  .data[[col.names[[8]]]])
  }
  # Some files contain wrong years...
  years <- year(data.out$Date)
  dif.years <- sum(years - Year, na.rm = T)
  if (dif.years != 0){
    year(data.out$Date) <- Year
  } 

  # filter out rows with NAs in certain fields, then order them using
  # Date and time since T0 (i.e., start.time defined above).
  # Create Date fields with character and date format - may not be necessary?
  data.out %>% 
    filter(!is.na(Date)) %>%
    filter(!is.na(Event)) %>%
    filter(!is.na(Shift)) %>%
    arrange(Date, Minutes_since_0000) %>%
    mutate(Date.date = as.Date(Date),
           Date.char = as.character(Date)) %>%
    select(-Date) %>%
    relocate(Date.date) -> data.out
  
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

# x is a data.frame with at least three fields: Date (character), 
# Minutes_since_0000
# and Shift - find.shift is used to come up with this
# 1 = START EFFORT
#2 = CHANGE OBSERVERS
#3 = CHANGE SIGHTING CONDITIONS
#4 = GRAY WHALE SIGHTING
#5 = END EFFORT
#6 = OTHER SPECIES SIGHTING
# provide a data frame that came back from get.data function, hrs for 
# start and end of each day (default = 7 and 19, correspond to 0700
# and 1900), and the maximum number of shift per day (default to 4).
find.effort <- function(x, start.hr = 7, end.hr = 19, max.shift = 4){
  # this turns dates in to character
  all.dates <- unique(x$Date.date)
  
  if (max.shift == 4){
    shifts <- c("1", "2", "3", "4")

  } else if (max.shift == 5){
    shifts <- c("1", "2", "3", "4", "5")
  }
  
  start.shift <- seq(start.hr*60, (end.hr*60-180), by = 180)
  end.shift <- seq((start.hr*60 + 180), (end.hr*60), by = 180)
  
  out.df <- data.frame(Date.char = rep(all.dates,
                                       each = length(shifts)),
                       Shift = rep(shifts, 
                                   times = length(all.dates)),
                       Effort = NA,
                       Mother_Calf = 0,
                       Sea_State = NA,
                       Vis = NA,
                       Time_T0 = NA,
                       Time_0000 = NA)
  
  for (d in 1:length(all.dates)){
    # pick just one day's worth of data
    one.day <- filter(x, Date.char == all.dates[d])
    
    for (k1 in 1:length(shifts)){
      one.day %>%
        filter(Minutes_since_0000 >= start.shift[k1] &
                 Minutes_since_0000 <= end.shift[k1]) -> one.shift
      
      if (nrow(one.shift) != 0){
        # add one row at the top and end of one.shift, so that it has
        # Event 1 at the top, and 5 at the bottom if they are not there:
        if (one.shift[1,"Event"] != 1){
          one.shift.eft <- rbind(one.shift[1,], one.shift)
          one.shift.eft[1, "Event"] <- 1
        } else {
          one.shift.eft <- one.shift
        }
        
        if (one.shift[nrow(one.shift), "Event"] != 5){
          one.shift.eft <- rbind(one.shift.eft, one.shift[nrow(one.shift),])
          one.shift.eft[nrow(one.shift.eft), "Event"] <- 5
        }

        # find out how many on/off effort existed
        row.1 <- which(one.shift.eft$Event == 1)
        row.5 <- which(one.shift.eft$Event == 5)
        
        # calculate effort for each on period per shift
        tmp.eft <- 0
        for (k2 in 1:length(row.1)){
          tmp <- one.shift.eft[row.1[k2]:row.5[k2],] 
          tmp.eft <- tmp.eft + (max(tmp$Minutes_since_0000) - min(tmp$Minutes_since_0000))  
        }

        out.df[out.df$Date.char == all.dates[d] & 
                 out.df$Shift == shifts[k1], "Effort"] <- tmp.eft
        
        out.df[out.df$Date.char == all.dates[d] & 
                 out.df$Shift == shifts[k1], "Mother_Calf"] <- sum(one.shift$Mother_Calf, 
                                                                  na.rm = T) 
        
        out.df[out.df$Date.char == all.dates[d] & 
                 out.df$Shift == shifts[k1], "Sea_State"] <- max(one.shift$SeaState, 
                                                                na.rm = T) 
        
        out.df[out.df$Date.char == all.dates[d] & 
                 out.df$Shift == shifts[k1], "Vis"] <- max(one.shift$Vis, na.rm = T) 
        
        out.df[out.df$Date.char == all.dates[d] & 
                 out.df$Shift == shifts[k1], "Time_T0"] <- min(one.shift$Minutes_since_T0, 
                                                              na.rm = T) 
        out.df[out.df$Date.char == all.dates[d] & 
                 out.df$Shift == shifts[k1], "Time_0000"] <- min(one.shift$Minutes_since_0000, 
                                                                na.rm = T) 
      }
    }
    
  }
  
  out.df %>% 
    mutate(Date.date = as.Date(Date.char, 
                               format = "%Y-%m-%d")) -> out.df.1
  return(out.df.1)
}

format.output <- function(data.shift, max.shift){
  # create a data frame with the full set of date/shift for dates with observations
  day.shifts <- data.frame(Date.char = rep(unique(data.shift$Date.char),
                                           each = max.shift),
                           Shift = ifelse(max.shift == 4, 
                                          rep(c("1", "2", "3", "4"), 
                                              times = length(unique(data.shift$Date.char))),
                                          rep(c("1", "2", "3", "4", "5"), 
                                              times = length(unique(data.shift$Date.char)))))
  # Combine the full set of date/shift with the observed - some shifts are NAs because
  # they were not in the dataset
  day.shifts %>% 
    left_join(data.shift, by = c("Date.char", "Shift")) -> all.day.shifts
  
  
  # create a vector with sequential weeks
  all.weeks <- seq.Date(as.Date(min(data.shift$Date.char)),
                        as.Date(max(data.shift$Date.char)),
                        by = "week")
  
  # select necessary data from per-shift data frame, mutate the column names
  # the create a new data frame
  data.shift %>% 
    select(Shift, Date.char, Date.date, Effort, Mother_Calf) %>%
    mutate(Date.char = Date.char,
           Effort = Effort/60,
           Sightings = Mother_Calf) %>%
    select(Date.char, Shift, Effort, Sightings) %>%
    mutate(Date.date = as.Date(Date.char)) %>%
    data.frame() -> raw.data
  
  # Create all dates, including weekends
  all.dates <- seq.Date(as.Date(min(data.shift$Date.char)),
                        as.Date(max(data.shift$Date.char)),
                        by = "day")
  
  # create all shists, incluyding nights
  all.shifts <- data.frame(Date.date = rep(all.dates,
                                           each = 8),
                           Shift = rep(c("1", "2", "3", "4",
                                         "5", "6", "7", "8"), 
                                       times = length(all.dates),
                                       by = "day"))
  
  all.shifts %>% 
    left_join(raw.data, by = c("Date.date", "Shift")) %>%
    mutate(Week = ceiling(difftime(Date.date, min(all.dates), 
                                   units = "weeks")) %>%
             as.numeric(),
           Date = Date.date) %>%
    select(Week, Date, Shift, Effort, Sightings) -> formatted.all.data
  
  # need to change week = 0 to week = 1...
  formatted.all.data[formatted.all.data$Week == 0, "Week"] <- 1
  
  # Change NAs to 0s
  formatted.all.data[is.na(formatted.all.data)] <- 0
  
  return(formatted.all.data)
}

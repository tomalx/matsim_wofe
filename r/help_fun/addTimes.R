
# add time

library(hms)
library(VGAM) # for skew norm
library(lubridate)


addEndTime <- function(meanTime = "08.00", sd_in_minutes = 30, skew = 5){  ## skew: -early, +late
 
  h <- 0:23
  m <- 0:59
  s <- 0:59
  times24h <- tibble(hour=rep(0:23, each = 3600), mins=rep(rep(m, each = 60),24),secs=rep(rep(s, times = 60), times=24))
  times24h <- times24h %>% mutate(lubritime = lubridate::hms(paste(times24h$hour,times24h$mins,times24h$secs)))
  times24h <- times24h %>% mutate(fromMidnight = 0:86399) %>% mutate(time = hms::as_hms(fromMidnight))
  rm(h,m,s)  
  sd <- sd_in_minutes*60
  mean <- hm(meanTime)
  mean <- filter(times24h, lubritime == mean)
  mean <- mean$fromMidnight
  time <- as.integer(rskewnorm(1,mean,sd,skew))

}


#end_time <- addEndTime("07.00")

minutesTOsecs <- function(minutes){
  minutes * 60
}

addMinutes <- function(time, minutes){
  hms::as_hms(time + minutesTOsecs(minutes))
}

minusMinutes <- function(time, minutes){
  hms::as_hms(time - minutesTOsecs(minutes))
}

## function to generate activity end times randomly
## drawn from a multimodal distribution - twin peaks



# nn <- 10000


#sims <- c(round( rnorm(nn*4, mean=5, sd=3), 0),
#          round( rnorm(nn*3, mean=20, sd=5), 0) )

#sample(sims,5)

#hist(sims, breaks=100)

addTimeBiModal <- function(time1 = "08.00",
                              time2 = "09.00",
                              sd1 = 30, 
                              sd2 = 15,
                              skew1 = 5,
                              skew2 = 5){  ## skew: -early, +late
  
  h <- 0:23
  m <- 0:59
  s <- 0:59
  times24h <- tibble(hour=rep(0:23, each = 3600), mins=rep(rep(m, each = 60),24),secs=rep(rep(s, times = 60), times=24))
  times24h <- times24h %>% mutate(lubritime = lubridate::hms(paste(times24h$hour,times24h$mins,times24h$secs)))
  times24h <- times24h %>% mutate(fromMidnight = 0:86399) %>% mutate(time = hms::as_hms(fromMidnight))
  rm(h,m,s)
  nn <- 10000
  sd1 <- sd1*60
  sd2 <- sd2*60
  mean1 <- hm(time1)
  mean1 <- filter(times24h, lubritime == mean1)
  mean1 <- mean1$fromMidnight
  mean2 <- hm(time2)
  mean2 <- filter(times24h, lubritime == mean2)
  mean2 <- mean2$fromMidnight
  
  sims <- c(as.integer(rskewnorm(nn,mean1,sd1,skew1)),
            as.integer(rskewnorm(nn,mean2,sd2,skew2)) )
  
  sample(sims,1)
}

#as_hms(addTimeBiModal())


#hist(sims)








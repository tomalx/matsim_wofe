
HLTH_1_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="HLTH_1"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.1.End <- addTimeBiModal(time1 = "14:00", time2 = "18:00", sd1 = 90, sd2 = 90, skew1 = 0, skew2 = 0)
    agentDF$Act.2.End <- addMinutes( as_hms(agentDF$Act.1.End),rnorm(1,mean = 300, sd = 60) )
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2,sec = "HLTH") #Act.2 X and Y
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF
    
  }else{
    #agentDF$Act.1 <- NA
    #agentDF$Act.2 <- NA
    #agentDF$Act.3 <- NA
    #agentDF$Act.1.End <- NA
    agentDF
  }
  
}

HLTH_2_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="HLTH_2"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "escort"
    agentDF$Act.3 <- "work"
    agentDF$Act.4 <- "escort"
    agentDF$Act.5 <- "home"
    agentDF$Act.2.End <- addEndTime(meanTime = "08:40",sd_in_minutes = 5, skew = -1)
    agentDF$Act.1.End <- minusMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,15,5,3))
    agentDF$Act.4.End <- addEndTime(meanTime = "15:30",sd_in_minutes = 20, skew = 4)
    agentDF$Act.3.End <- minusMinutes(as_hms(agentDF$Act.4.End),rskewnorm(1,30,5,5))
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.3.X, -Act.3.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 3,sec = "HLTH") #Act.3 X and Y
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 2,radius =5000, sec = "EDUC") #Act.2 X and Y
    agentDF$Act.4.X <- agentDF$Act.2.X
    agentDF$Act.4.Y <- agentDF$Act.2.Y
    agentDF$Act.5.X <- agentDF$home.X
    agentDF$Act.5.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

HLTH_3_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="HLTH_3"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "shop"
    agentDF$Act.4 <- "home"
    
    
    agentDF$Act.1.End <- addTimeBiModal(time1 = "07:00", time2 = "08:00", sd1 = 60, sd2 = 90, skew1 = 3, skew2 = 1)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rskewnorm(1,450,60,-2))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,90,45,0))
    
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.3.X, -Act.3.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2, sec = "HLTH")
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 3,radius =5000, centreX = "Act.2.X", centreY = "Act.2.Y", sec = "HIGH")
    
    agentDF$Act.4.X <- agentDF$home.X
    agentDF$Act.4.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

HLTH_4_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="HLTH_4"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.1.End <- addTimeBiModal(time1 = "06:00", time2 = "12:00", sd1 = 90, sd2 = 90, skew1 = 0, skew2 = 0)
    agentDF$Act.2.End <- addMinutes( as_hms(agentDF$Act.1.End),rnorm(1,mean = 500, sd = 120) )
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2,sec = "HLTH") #Act.2 X and Y
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF
    
  }else{
    #agentDF$Act.1 <- NA
    #agentDF$Act.2 <- NA
    #agentDF$Act.3 <- NA
    #agentDF$Act.1.End <- NA
    agentDF
  }
  
}
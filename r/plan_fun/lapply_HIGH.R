
HIGH_1_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="HIGH_1"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.4 <- "leisure"
    agentDF$Act.5 <- "home"
    
    agentDF$Act.1.End <- addTimeBiModal(time1 = "06:00", time2 = "08:00", sd1 = 30, sd2 = 30, skew1 = 2, skew2 = 0)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rskewnorm(1,520,30,-2))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,120,45, 1))
    agentDF$Act.4.End <- addMinutes(as_hms(agentDF$Act.3.End),rskewnorm(1,120,45,1))
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.4.X, -Act.4.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2, sec = "HIGH")
    agentDF <- addXY_leisureActs(agentDF,refNumber = 4,radius =5000, pointsDF = LEISURE_points)
    
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF$Act.5.X <- agentDF$home.X
    agentDF$Act.5.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

HIGH_2_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="HIGH_2"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "escort"
    agentDF$Act.3 <- "work"
    agentDF$Act.4 <- "escort"
    agentDF$Act.5 <- "home"
    agentDF$Act.2.End <- addEndTime(meanTime = "08:40",sd_in_minutes = 3, skew = 2)
    agentDF$Act.1.End <- minusMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,15,5,3))
    agentDF$Act.4.End <- addEndTime(meanTime = "15:30",sd_in_minutes = 3, skew = 2)
    agentDF$Act.3.End <- minusMinutes(as_hms(agentDF$Act.4.End),rskewnorm(1,30,5,5))
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.3.X, -Act.3.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 3,sec = "HIGH") #Act.3 X and Y
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

HIGH_3_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="HIGH_3"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "shop"
    agentDF$Act.4 <- "home"
  
    
    agentDF$Act.1.End <- addTimeBiModal(time1 = "08:45", time2 = "12:00", sd1 = 60, sd2 = 90, skew1 = 3, skew2 = 1)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rskewnorm(1,270,30,3))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,90,45,0))
    
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.3.X, -Act.3.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2, sec = "HIGH")
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 3,radius =5000, centreX = "Act.2.X", centreY = "Act.2.Y", sec = "HIGH")
    
    agentDF$Act.4.X <- agentDF$home.X
    agentDF$Act.4.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

HIGH_4_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="HIGH_4"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.1.End <- addTimeBiModal(time1 = "08:00", time2 = "10:00", sd1 = 15, sd2 = 30, skew1 = -2, skew2 = 3)
    agentDF$Act.2.End <- addMinutes( as_hms(agentDF$Act.1.End),rnorm(1,mean = 540, sd = 30) )
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2,sec = "HIGH") #Act.2 X and Y
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
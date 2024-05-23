
EDUC_1_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="EDUC_1"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.1.End <- addEndTime(meanTime = "07:00", sd_in_minutes = 30, skew = 0)
    agentDF$Act.2.End <- addMinutes( as_hms(agentDF$Act.1.End),rskewnorm(1,540,60, 2) )
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2,sec = "EDUC") #Act.2 X and Y
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

EDUC_2_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="EDUC_2"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.1.End <- addEndTime(meanTime = "08:00", sd_in_minutes = 30, skew = 0)
    agentDF$Act.2.End <- addMinutes( as_hms(agentDF$Act.1.End),rskewnorm(1, 500, 60, -4) )
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2,sec = "EDUC") #Act.2 X and Y
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

EDUC_3_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="EDUC_3"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "shop"
    agentDF$Act.4 <- "home"
    
    
    agentDF$Act.1.End <- addEndTime(meanTime = "07:00", sd_in_minutes = 30, skew = 0)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rskewnorm(1,450,60,-2))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,90,45,0))
    
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.3.X, -Act.3.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2, sec = "EDUC")
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 3,radius =5000, centreX = "Act.2.X", centreY = "Act.2.Y", sec = "HIGH")
    
    agentDF$Act.4.X <- agentDF$home.X
    agentDF$Act.4.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

EDUC_4_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="EDUC_4"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.1.End <- addTimeBiModal(time1 = "07:00", time2 = "08:00", sd1 = 90, sd2 = 45, skew1 = 2, skew2 = 0)
    agentDF$Act.2.End <- addMinutes( as_hms(agentDF$Act.1.End),rnorm(1,mean = 500, sd = 120) )
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2,sec = "EDUC") #Act.2 X and Y
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
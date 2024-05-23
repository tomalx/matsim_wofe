
CONS_1_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  
  
  if(agentDF$planType=="CONS_1"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.1.End <- addTimeBiModal(time1 = "07:00", time2 = "08:00", sd1 = 30, sd2 = 30, skew1 = -2, skew2 = 0)
    agentDF$Act.2.End <- addMinutes( as_hms(agentDF$Act.1.End),rnorm(1,mean = 600, sd = 30) )
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2,sec = "OFFC") #Act.2 X and Y
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

CONS_2_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="CONS_2"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "shop"
    agentDF$Act.4 <- "work"
    agentDF$Act.5 <- "home"
    
    agentDF$Act.1.End <- addEndTime("08:15", 45, 1)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rskewnorm(1,120,120,2))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,75,15,0))
    agentDF$Act.4.End <- addMinutes(as_hms(agentDF$Act.3.End),rskewnorm(1,180,80,1))
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.3.X, -Act.3.Y)
    agentDF <- addXY_radiusMethod_home(agentDF,refNumber = 2, radius = 10000, pointsDF = HOME_points)
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 3,radius =5000, centreX = "Act.2.X", centreY = "Act.2.Y",  sec = "HIGH") # centreX = agentDF$Act.2.X, centreY = agentDF$Act.2.Y,
    
    agentDF$Act.4.X <- agentDF$Act.2.X
    agentDF$Act.4.Y <- agentDF$Act.2.Y
    agentDF$Act.5.X <- agentDF$home.X
    agentDF$Act.5.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

CONS_3_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="CONS_3"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "work"
    agentDF$Act.3 <- "home"
    agentDF$Act.4 <- "leisure"
    agentDF$Act.5 <- "home"
    
    agentDF$Act.1.End <- addTimeBiModal(time1 = "07:00", time2 = "08:00", sd1 = 45, sd2 = 45, skew1 = 1, skew2 = 1)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rskewnorm(1,520,90,-1))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,120,60, 1))
    agentDF$Act.4.End <- addMinutes(as_hms(agentDF$Act.3.End),rskewnorm(1,120,45,2))
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.4.X, -Act.4.Y)
    agentDF <- addXY_ODmethod(agentDF,refNumber = 2, sec = "INDY")
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




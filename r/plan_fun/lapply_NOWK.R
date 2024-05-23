
NOWK_1_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  
  
  if(agentDF$planType=="NOWK_1"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "education"
    agentDF$Act.3 <- "home"
    agentDF$Act.1.End <- addTimeBiModal(time1 = "08:00", time2 = "09:30", sd1 = 45, sd2 = 45, skew1 = 0, skew2 = 0)
    agentDF$Act.2.End <- addMinutes( as_hms(agentDF$Act.1.End),rnorm(1,mean = 360, sd = 90) )
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 2, radius = 10000, sec = "EDUC") #Act.2 X and Y
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

NOWK_2_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="NOWK_2"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "education"
    agentDF$Act.3 <- "home"
    agentDF$Act.4 <- "shop"
    agentDF$Act.5 <- "home"
    
    agentDF$Act.1.End <- addTimeBiModal(time1 = "07:45", time2 = "08:45", sd1 = 45, sd2 = 45, skew1 = 1, skew2 = 0)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rnorm(1,300,30))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rnorm(1,300,45))
    agentDF$Act.4.End <- addMinutes(as_hms(agentDF$Act.3.End),rnorm(1,90,30))
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.3.X, -Act.3.Y,-Act.4.X, -Act.4.Y)
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 2, sec = "EDUC")
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 4,radius =7500, sec = "HIGH")
   
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF$Act.5.X <- agentDF$home.X
    agentDF$Act.5.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

NOWK_3_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="NOWK_3"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "education"
    agentDF$Act.3 <- "home"
    agentDF$Act.4 <- "leisure"
    agentDF$Act.5 <- "home"
    
    agentDF$Act.1.End <- addTimeBiModal(time1 = "07:45", time2 = "08:45", sd1 = 45, sd2 = 45, skew1 = 2, skew2 = 1)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rnorm(1,300,30))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rnorm(1,360,60))
    agentDF$Act.4.End <- addMinutes(as_hms(agentDF$Act.3.End),rnorm(1,120,45))
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.4.X, -Act.4.Y)
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 2, radius = 5000, sec = "EDUC")
    agentDF <- addXY_leisureActs(agentDF,refNumber = 4,radius = 5000, pointsDF = LEISURE_points)
    
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF$Act.5.X <- agentDF$home.X
    agentDF$Act.5.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

NOWK_4_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="NOWK_4"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "business"
    agentDF$Act.3 <- "home"
    
    agentDF$Act.1.End <- addTimeBiModal(time1 = "09:15", time2 = "14:00", sd1 = 120, sd2 = 120, skew1 = 1, skew2 = 1)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rskewnorm(1,120,30,3))
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 2,radius =10000, sec = c("HLTH", "OFFC")) # centreX = agentDF$Act.2.X, centreY = agentDF$Act.2.Y,
    
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

NOWK_5_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="NOWK_5"){
    
    # add variables to the "everything else" DF,
    # so that we can rbind at the end.
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "leisure"
    agentDF$Act.3 <- "home"
    
    agentDF$Act.1.End <- addTimeBiModal(time1 = "09:30", time2 = "12:00", sd1 = 60, sd2 = 120, skew1 = 1, skew2 = 0)
    agentDF$Act.2.End <- addMinutes(as_hms(agentDF$Act.1.End),rskewnorm(1,240,120,1))
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y)
    agentDF <- addXY_leisureActs(agentDF,refNumber = 2,radius =20000, pointsDF = LEISURE_points)
    
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}

NOWK_6_fun <- function(agent){
  
  agentDF <- data.frame(as.list(agent))
  
  if(agentDF$planType=="NOWK_6"){
    
    agentDF$Act.1 <- "home"
    agentDF$Act.2 <- "escort"
    agentDF$Act.3 <- "home"
    agentDF$Act.4 <- "shop"
    agentDF$Act.5 <- "home"
    agentDF$Act.6 <- "escort"
    agentDF$Act.7 <- "home"
    
    
    agentDF$Act.2.End <- addEndTime(meanTime = "08:30",sd_in_minutes = 5, skew = 2)
    agentDF$Act.1.End <- minusMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,15,5,3))
    agentDF$Act.3.End <- addMinutes(as_hms(agentDF$Act.2.End),rskewnorm(1,90,30,1))
    agentDF$Act.4.End <- addMinutes(as_hms(agentDF$Act.3.End),rskewnorm(1,90,30,2))
    agentDF$Act.6.End <- addEndTime(meanTime = "15:30",sd_in_minutes = 5, skew = 1)
    agentDF$Act.5.End <- minusMinutes(as_hms(agentDF$Act.6.End),rskewnorm(1,30,5,5))
    
    agentDF <- agentDF %>% select(-Act.2.X, -Act.2.Y, -Act.4.X, -Act.4.Y)
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 2,radius =5000, sec = "EDUC") #Act.2 X and Y
    agentDF <- addXY_radiusMethod(agentDF,refNumber = 4,radius =10000, sec = "HIGH") #Act.2 X and Y
   
    agentDF$Act.3.X <- agentDF$home.X
    agentDF$Act.3.Y <- agentDF$home.Y
    agentDF$Act.5.X <- agentDF$home.X
    agentDF$Act.5.Y <- agentDF$home.Y
    agentDF$Act.6.X <- agentDF$Act.2.X
    agentDF$Act.6.Y <- agentDF$Act.2.Y
    agentDF$Act.7.X <- agentDF$home.X
    agentDF$Act.7.Y <- agentDF$home.Y
    agentDF
    
  }else{
    agentDF
  }
  
}
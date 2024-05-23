#################################################################
# import residential points data with OA attribute + format     #
#################################################################

# assumes HOME_points data loaded
# if not run...
# source("rScript/routines/helperFunctions/loadPointData.R")

#################################################################
# function to add home location (X & Y grid ref) for each agent #
#################################################################

addHomeLoop <- function(agent.row){
  agent <- agent.row #%>% select(-c(home.X,home.Y))
  agentOApoints <- filter(HOME_points, homeOA == agent$OA) # add another filter here for different agent plan routines
  agentHome <- sample(agentOApoints$pointRef, 1) 
  agentHome <- HOME_points %>% filter(pointRef == agentHome)
  agentHome <- cbind(agent,agentHome)
  agentHome <- agentHome %>% select(-c("pointRef","homeOA"))
  agentHome <- agentHome %>% rename("home.X"=X , "home.Y"=Y) 
  }


#################################################################
# function to add home location (X & Y grid ref) for each agent #
#################################################################
#apply version

#myPopList <- split(myPop, seq(nrow(myPop))) 

#agent.row <- myPop[5,]

addHomeList <- function(agent.row){
  
  myOA <- agent.row[["OA"]]
  myHomePoint <- HOME_points[HOME_points$homeOA == myOA,]
  myHomePoint <- sample_n(myHomePoint,1)
  agent.row <- cbind(agent.row,myHomePoint)
  agent.row <- agent.row %>% select("agentName","OA","planType","X","Y",starts_with("attr."))
  #agent.row <- agent.row[,c("agentName","OA","planType","X","Y",starts_with("attr."))]
  agent.row <- agent.row %>% rename("home.X"=X , "home.Y"=Y)
}

#myPopTest <- lapply(myPopList, addHome)
#myPopTest[1]
#########################################
# FAKE (obsolete) - keep for reference? #
#########################################
# Function that creates a X and Y grid ref for a row of a data frame.

## read work locations csv file
#homePoints <- read.csv("csv/fakeHomePoints.csv")
#homePoints <- homePoints %>% mutate(homePointRef = paste0("hp_",row_number()))
#homePoints <- homePoints %>% select(homePointRef,homeOA,X,Y)
#homePoints$X <- round(homePoints$X,0)
#homePoints$Y <- round(homePoints$Y,0)
#homePoints <- homePoints %>% rename("home.X"=X , "home.Y"=Y)

## workerCount <- flowData %>% group_by(OA) %>% summarise(count = sum(count))
## agent <- myPop[2,]


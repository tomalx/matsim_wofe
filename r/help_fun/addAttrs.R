#################################################################
# function to add other Attributes for each agent #
# BME
# Car Availability
#################################################################

addAttrsList <- function(agent.row){
  
  myOA <- agent.row[["OA"]]
  myAttrs <- profileTypeDF[profileTypeDF$OA == myOA,]
  probNoCar <- myAttrs$dem.noCars/myAttrs$dem.pop.total
  probBME <- myAttrs$dem.BME/myAttrs$dem.pop.total
  myAttrs <- myAttrs %>% mutate(attr.carAvailable = sample(c(FALSE,TRUE),1, prob = c(probNoCar,1-probNoCar)) )
  myAttrs <- myAttrs %>% mutate(attr.BME = sample(c(TRUE,FALSE),1, prob = c(probBME,1-probBME)) )
  agent.row <- cbind(agent.row,myAttrs)
  agent.row <- agent.row[,c("agentName","OA","planType","attr.carAvailable","attr.BME")]
}


###################################################################

addAttrsLoop <- function(agent.row){
  
  agent_row_OA <- agent.row$OA
  agent_attrs <- profileTypeDF %>% filter(OA == agent_row_OA)
  probNoCar <- agent_attrs$dem.noCars/agent_attrs$dem.pop.total
  probBME <- agent_attrs$dem.BME/agent_attrs$dem.pop.total
  agent_attrs <- agent_attrs %>% mutate(attr.carAvailable = sample(c(FALSE,TRUE),1, prob = c(probNoCar,1-probNoCar)) )
  agent_attrs <- agent_attrs %>% mutate(attr.BME = sample(c(TRUE,FALSE),1, prob = c(probBME,1-probBME)) )
  ## bind_cols (only planType,attr.carAvailable,attr.BME)
  agent.row <- bind_cols(agent.row,agent_attrs %>% select(attr.carAvailable,attr.BME))
  
}


addHomeLoop <- function(agent.row){
  agent <- agent.row #%>% select(-c(home.X,home.Y))
  agentOApoints <- filter(HOME_points, homeOA == agent$OA) # add another filter here for different agent plan routines
  agentHome <- sample(agentOApoints$pointRef, 1) 
  agentHome <- HOME_points %>% filter(pointRef == agentHome)
  agentHome <- cbind(agent,agentHome)
  agentHome <- agentHome %>% select(-c("pointRef","homeOA"))
  agentHome <- agentHome %>% rename("home.X"=X , "home.Y"=Y) 
}
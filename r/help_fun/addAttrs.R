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



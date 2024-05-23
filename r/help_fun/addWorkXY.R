


library(sf)

options(digits = 3)


######### FUNCTION: addMultiWork_ODmethod
###########Function: adds work.X and work.Y coordinates to a row of agent data ###


addXY_ODmethod <- function(agent.row, refNumber, sec= c("INDY","OFFC","HIGH","EDUC","HLTH")){
  
  agent <- agent.row #%>% select(-c(home.X,home.Y))
  thisAgentOA <- filter(flowData, OA == agent$OA)
  selectedMSOA <- sample(thisAgentOA$workMSOA, 1, prob = thisAgentOA$workMSOAfreq)
  thisAgentWorkMSOA <- WORK_points %>%  filter(MSOA == selectedMSOA)
  thisAgentWorkMSOA <- thisAgentWorkMSOA %>% filter(sector %in% sec)
  if(nrow(thisAgentWorkMSOA)==0){thisAgentWorkMSOA <- WORK_points %>% filter(MSOA == selectedMSOA)}
  thisAgentWorkPoint <- sample_n(thisAgentWorkMSOA,1)
  
  names(thisAgentWorkPoint)[names(thisAgentWorkPoint)=="X"] <- paste0("Act.",refNumber,".X")
  names(thisAgentWorkPoint)[names(thisAgentWorkPoint)=="Y"] <- paste0("Act.",refNumber,".Y")
  
  agent.row.with.work.points <- cbind(agent,thisAgentWorkPoint)
  agent.row.with.work.points <- agent.row.with.work.points %>% select(-MSOA, -sector, -pointRef)
}

addXY_radiusMethod <- function(agent.row, refNumber, 
                                radius = 2000, sec = c("INDY","OFFC","HIGH","EDUC","HLTH"), 
                                pointsDF = WORK_points,
                                centreX = "home.X",
                                centreY = "home.Y"
                               ){
  
  
  agent <- agent.row
  X <- agent[,names(agent) == centreX]
  Y <- agent[,names(agent) == centreY]
  
  centre <- st_point(c(X,Y),  dim = "XY")
  centre <- centre %>% st_sfc(crs = 27700, precision = 1)
  circle <- st_buffer(centre, radius)
  
  # intersection of circle and workPoints (or LeisurePoints, HomePoints, etc)
  ### ?? set up code so that points selected changes dynamically with activity argument.
  
  pointsSF <- st_as_sf(pointsDF, coords = c("X","Y") ,crs = 27700)
  #pointsSF <- st_as_sf(pointsDF, coords = c(paste0(X, Y)) ,crs = 27700)
  circlePoints <- st_intersection(pointsSF, circle)
  circlePoints <- circlePoints %>% filter(sector %in% sec)
  if(nrow(circlePoints)==0){circlePoints <- st_intersection(pointsSF, circle)}
  ##### add Filter here ####
  ##### filter for sector type ###
  ## e.g.
  #if(sector == NULL){circlePoints <- circlePoints %>% filter(sector == sector)}
  ## if circlePoints == 0L, don't do it.
  
  # pick one of the points in this intersection.
  
  selected_circlePoint <- sample_n(circlePoints,1) %>% select(-MSOA, -sector)   ## could use weighting here!!!
  st_geometry(selected_circlePoint) <- NULL
  selected_circlePoint <- left_join(selected_circlePoint, pointsDF, by = "pointRef") %>% select(-MSOA, -sector)
  
  # add X and Y coordinate onto agent (use code from addMultiWork_ODmethod)
  
  names(selected_circlePoint)[names(selected_circlePoint)== "X"] <- paste0("Act.",refNumber,".X")
  names(selected_circlePoint)[names(selected_circlePoint)== "Y"] <- paste0("Act.",refNumber,".Y")
  agent.row.with.new.points <- cbind(agent,selected_circlePoint)
  agent.row.with.new.points <- agent.row.with.new.points %>% select(-pointRef)
  
}



addXY_radiusMethod_home <- function(agent.row, refNumber, 
                               radius = 2000, 
                               pointsDF = HOME_points,
                               centreX = "home.X",
                               centreY = "home.Y"
){
  
  
  agent <- agent.row
  X <- agent[,names(agent) == centreX]
  Y <- agent[,names(agent) == centreY]
  
  centre <- st_point(c(X,Y),  dim = "XY")
  centre <- centre %>% st_sfc(crs = 27700, precision = 1)
  circle <- st_buffer(centre, radius)
  
  # intersection of circle and workPoints (or LeisurePoints, HomePoints, etc)
  ### ?? set up code so that points selected changes dynamically with activity argument.
  
  pointsSF <- st_as_sf(pointsDF, coords = c("X","Y") ,crs = 27700)
  #pointsSF <- st_as_sf(pointsDF, coords = c(paste0(X, Y)) ,crs = 27700)
  circlePoints <- st_intersection(pointsSF, circle)
  if(nrow(circlePoints)==0){circlePoints <- st_intersection(pointsSF, circle)}
  ##### add Filter here ####
  ##### filter for sector type ###
  ## e.g.
  #if(sector == NULL){circlePoints <- circlePoints %>% filter(sector == sector)}
  ## if circlePoints == 0L, don't do it.
  
  # pick one of the points in this intersection.
  
  selected_circlePoint <- sample_n(circlePoints,1) %>% select(-homeOA)   ## could use weighting here!!!
  st_geometry(selected_circlePoint) <- NULL
  selected_circlePoint <- left_join(selected_circlePoint, pointsDF, by = "pointRef") %>% select(-homeOA)
  
  # add X and Y coordinate onto agent (use code from addMultiWork_ODmethod)
  
  names(selected_circlePoint)[names(selected_circlePoint)== "X"] <- paste0("Act.",refNumber,".X")
  names(selected_circlePoint)[names(selected_circlePoint)== "Y"] <- paste0("Act.",refNumber,".Y")
  agent.row.with.new.points <- cbind(agent,selected_circlePoint)
  agent.row.with.new.points <- agent.row.with.new.points %>% select(-pointRef)
  
}


addXY_leisureActs <- function(agent.row, refNumber, radius = 5000,
                               pointsDF = LEISURE_points, typ = c("outdoor","indoor")){
  
  centreX <- "home.X"
  centreY <- "home.Y"
  agent <- agent.row
  X <- agent[,names(agent) == centreX]
  Y <- agent[,names(agent) == centreY]
  
  centre <- st_point(c(X,Y),  dim = "XY")
  centre <- centre %>% st_sfc(crs = 27700, precision = 1)
  circle <- st_buffer(centre, radius)
  
  # intersection of circle and workPoints (or LeisurePoints, HomePoints, etc)
  ### ?? set up code so that points selected changes dynamically with activity argument.
  
  pointsSF <- st_as_sf(pointsDF, coords = c("X","Y") ,crs = 27700)
  #pointsSF <- st_as_sf(pointsDF, coords = c(paste0(X, Y)) ,crs = 27700)
  circlePoints <- st_intersection(pointsSF, circle)
  circlePoints <- circlePoints %>% filter(type %in% typ)
  if(nrow(circlePoints)==0){circlePoints <- st_intersection(pointsSF, circle)}
  ##### add Filter here ####
  ##### filter for sector type ###
  ## e.g.
  #if(sector == NULL){circlePoints <- circlePoints %>% filter(sector == sector)}
  ## if circlePoints == 0L, don't do it.
  
  # pick one of the points in this intersection.
  
  selected_circlePoint <- sample_n(circlePoints,1) %>% select(-type)   ## could use weighting here!!!
  st_geometry(selected_circlePoint) <- NULL
  selected_circlePoint <- left_join(selected_circlePoint, pointsDF, by = "pointRef") %>% select(-type)
  
  # add X and Y coordinate onto agent (use code from addMultiWork_ODmethod)
  
  names(selected_circlePoint)[names(selected_circlePoint)== "X"] <- paste0("Act.",refNumber,".X")
  names(selected_circlePoint)[names(selected_circlePoint)== "Y"] <- paste0("Act.",refNumber,".Y")
  agent.row.with.new.points <- cbind(agent,selected_circlePoint)
  agent.row.with.new.points <- agent.row.with.new.points %>% select(-pointRef)
  
}



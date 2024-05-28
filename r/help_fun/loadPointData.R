# import shapefiles for activity locations
# and convert to DF with X and Y

library(sf)

EDUC_points <- read_sf("shp/EDUC_points.shp")
HLTH_points <- read_sf("shp/HLTH_points.shp")
OFFC_points <- read_sf("shp/OFFC_points.shp")
INDY_points <- read_sf("shp/INDY_points.shp")
HIGH_points <- read_sf("shp/HIGH_points.shp")

#function to import and format shp file
#for work points

formatPoints <- function(sf_obj){
  data <- st_set_geometry(sf_obj, NULL) %>% select(MSOA = msoa11cd)
  as_tibble(st_coordinates(sf_obj)) %>% cbind(data)
}

EDUC_points <- formatPoints(EDUC_points) %>% mutate(sector = "EDUC")
HLTH_points <- formatPoints(HLTH_points) %>% mutate(sector = "HLTH")
OFFC_points <- formatPoints(OFFC_points) %>% mutate(sector = "OFFC")
INDY_points <- formatPoints(INDY_points) %>% mutate(sector = "INDY")
HIGH_points <- formatPoints(HIGH_points) %>% mutate(sector = "HIGH")

WORK_points <- rbind(EDUC_points, HLTH_points, OFFC_points, INDY_points, HIGH_points)
rm(formatPoints, EDUC_points, HIGH_points, HLTH_points, INDY_points, OFFC_points)
#WORK_points <- WORK_points %>% rename("work.X"=X , "work.Y"=Y)
WORK_points <- WORK_points %>% mutate(pointRef = paste0("wp_",row_number()))
WORK_points <- WORK_points %>% mutate(X = round(X), Y = round(Y))

HOME_points <- read.csv("csv/HOME_points.csv")


# HOME_points <- read_sf("shp/homePoints.shp")
# data <- st_set_geometry(HOME_points, NULL) %>% select(homeOA = OA11CD)
# HOME_points <- as_tibble(st_coordinates(HOME_points)) %>% cbind(data)
# rm(data)
# HOME_points <- HOME_points %>% mutate(pointRef = paste0("hp_",row_number()))
# HOME_points <- HOME_points %>% select(pointRef,homeOA,X,Y)
# HOME_points$X <- round(HOME_points$X,0)
# HOME_points$Y <- round(HOME_points$Y,0)
# HOME_points <- sample_n(HOME_points, 100000)
#HOME_points <- HOME_points %>% rename("home.X"=X , "home.Y"=Y)
#HOME_points <- st_as_sf(HOME_points, coords = c("X","Y"), crs=27700)
#mapview(HOME_points)

LEISURE_points <- read_sf("shp/LEISURE_points.shp")
data <- st_set_geometry(LEISURE_points, NULL) %>% select(type)
LEISURE_points <- as_tibble(st_coordinates(LEISURE_points)) %>% cbind(data)
LEISURE_points <- LEISURE_points %>% mutate(pointRef = paste0("lp_",row_number()))
LEISURE_points <- LEISURE_points %>% mutate(X = round(X), Y = round(Y))
rm(data)

############################################
# load flow data
##################

flowData <- read.csv("csv/flow/OD_OAhome_MSOAwork_withinWofEonly.csv")
flowData <- flowData %>% pivot_longer(!OA, names_to = "workMSOA", values_to = "count")
OAtotalFlow <- flowData %>% group_by(OA) %>% summarise(total = sum(count))
flowData <- left_join(flowData, OAtotalFlow, by = "OA")
rm(OAtotalFlow)
flowData <- flowData %>% mutate(workMSOAfreq = round(count/total,3))













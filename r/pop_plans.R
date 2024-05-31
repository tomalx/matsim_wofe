library(tidyverse)
library(tictoc)
library(parallel)
library(VGAM)
library(hms)
library(sf)



#setwd("~/matsim/on_github/matsim_wofe")
#setwd("C:/Users/tom.alexander1/OneDrive - West Of England Combined Authority/Transport/7.0 Data/MATsim/matsim_wofe")


#source("r/pop_profile.R")   # if not already run
#source("r/pop_expander.R")  # if not already run

#load("sample_yate_pop.RData")
#load("sample_bus6or7_pop.RData")

sampleSize  <- 1000     #for testing set smaller pop sample size
#pop <- sample_oa_pop     # or use an Output Area sample (e.g. Yate OAs or OAs on bus route 6 or 7)
myPop <- sample_n(pop, sampleSize)  # get a sample pop
#myPop <- myPop %>% mutate(Act.1 = NA, Act.2 = NA, Act.3 = NA, Act.4 = NA, Act.5 = NA, Act.6 = NA, Act.7= NA, Act.8= NA, Act.9= NA, Act.10= NA)


#source("rScript/routines/planFunctions/plans_INDY.R")
#source("rScript/routines/planFunctions/plans_OFFC.R")

source("r/help_fun/addHomeXY.R")
source("r/help_fun/addWorkXY.R")
source("r/help_fun/addTimes.R")
source("r/help_fun/addAttrs.R")
source("r/help_fun/loadPointData.R")

source('r/plan_fun/lapply_INDY.R')
source('r/plan_fun/lapply_OFFC.R')
source('r/plan_fun/lapply_HIGH.R')
source('r/plan_fun/lapply_HLTH.R')
source('r/plan_fun/lapply_EDUC.R')
source('r/plan_fun/lapply_CONS.R')
source('r/plan_fun/lapply_NOWK.R')


# NB will take approx. 3 hours to run this loop for 500k population



################# OPTION1: using LAPPLY ##############################

#myPopList <- split(myPop, seq(nrow(myPop))) 

#tic("just lapply")
#myPopTest <- lapply(myPopList,addHomeList)
#rm(HOME_points, myPopList)
#myPopTest <- lapply(myPopTest,INDY_1_fun)
#myPopTest <- lapply(myPopTest,NOWK_3_fun)
#myPopTest <- lapply(myPopTest,INDY_3_fun)
#myPopTest <- lapply(myPopTest,INDY_4_fun)
#toc()
tic("just lapply")
myPopList <- split(myPop, seq(nrow(myPop)))
myPop_with_acts <- lapply(myPopList,addAttrsLoop)
myPop_with_acts <- lapply(myPop_with_acts,addHomeLoop)
myPop_with_acts <- lapply(myPop_with_acts,INDY_1_fun)
myPop_with_acts <- lapply(myPop_with_acts,INDY_2_fun)
myPop_with_acts <- lapply(myPop_with_acts,INDY_3_fun)
myPop_with_acts <- lapply(myPop_with_acts,INDY_4_fun)
myPop_with_acts <- lapply(myPop_with_acts,OFFC_1_fun)
myPop_with_acts <- lapply(myPop_with_acts,OFFC_2_fun)
myPop_with_acts <- lapply(myPop_with_acts,OFFC_3_fun)
myPop_with_acts <- lapply(myPop_with_acts,OFFC_4_fun)
myPop_with_acts <- lapply(myPop_with_acts,OFFC_5_fun)
myPop_with_acts <- lapply(myPop_with_acts,HIGH_1_fun)
myPop_with_acts <- lapply(myPop_with_acts,HIGH_2_fun)
myPop_with_acts <- lapply(myPop_with_acts,HIGH_3_fun)
myPop_with_acts <- lapply(myPop_with_acts,HIGH_4_fun)
myPop_with_acts <- lapply(myPop_with_acts,HLTH_1_fun)
myPop_with_acts <- lapply(myPop_with_acts,HLTH_2_fun)
myPop_with_acts <- lapply(myPop_with_acts,HLTH_3_fun)
myPop_with_acts <- lapply(myPop_with_acts,HLTH_4_fun)
myPop_with_acts <- lapply(myPop_with_acts,EDUC_1_fun)
myPop_with_acts <- lapply(myPop_with_acts,EDUC_2_fun)
myPop_with_acts <- lapply(myPop_with_acts,EDUC_3_fun)
myPop_with_acts <- lapply(myPop_with_acts,EDUC_4_fun)
myPop_with_acts <- lapply(myPop_with_acts,CONS_1_fun)
myPop_with_acts <- lapply(myPop_with_acts,CONS_2_fun)
myPop_with_acts <- lapply(myPop_with_acts,CONS_3_fun)
myPop_with_acts <- lapply(myPop_with_acts,NOWK_1_fun)
myPop_with_acts <- lapply(myPop_with_acts,NOWK_2_fun)
myPop_with_acts <- lapply(myPop_with_acts,NOWK_3_fun)
myPop_with_acts <- lapply(myPop_with_acts,NOWK_4_fun)
myPop_with_acts <- lapply(myPop_with_acts,NOWK_5_fun)
myPop_with_acts <- lapply(myPop_with_acts,NOWK_6_fun)


myPop_with_acts <- as.data.frame(do.call(rbind, myPop_with_acts))
toc()

# 250 - 89.3 secs
# 1000 - 322.3 secs



################# OPTION2: PARALLEL ###############################

myPopList <- split(myPop, seq(nrow(myPop)))

cl <- makeCluster (4)
clusterExport (cl, varlist = c("myPopList", "addHomeList", "flowData", "HOME_points", 
                               "addEndTime", "addMinutes", "addTimeBiModal", "addXY_leisureActs",
                               "addXY_ODmethod", "addXY_radiusMethod", "addXY_radiusMethod_home",
                               "INDY_1_fun", "INDY_2_fun","INDY_3_fun", 
                               "minusMinutes", "minutesTOsecs", "WORK_points",
                               "LEISURE_points") )
clusterEvalQ(cl, library("tidyverse"))
clusterEvalQ(cl, library("sf"))
clusterEvalQ(cl, library("hms"))
clusterEvalQ(cl, library("VGAM"))
clusterEvalQ(cl, library("lubridate"))

tic("in parallel")
# parPop <- parLapply (cl, myPopList, addHomeList)
parPop <- lapply(myPopList,addAttrsList)
parPop <- lapply(parPop,addHomeList)

parPop <- parLapply (cl, parPop, INDY_1_fun)
parPop <- parLapply (cl, parPop, INDY_2_fun)
parPop <- parLapply (cl, parPop, INDY_3_fun)
parPop <- parLapply (cl, parPop, INDY_4_fun)

parPop <- parLapply (cl, parPop, OFFC_1_fun)
parPop <- parLapply (cl, parPop, OFFC_2_fun)
parPop <- parLapply (cl, parPop, OFFC_3_fun)
parPop <- parLapply (cl, parPop, OFFC_4_fun)
parPop <- parLapply (cl, parPop, OFFC_5_fun)

parPop <- parLapply (cl, parPop, HIGH_1_fun)
parPop <- parLapply (cl, parPop, HIGH_2_fun)
parPop <- parLapply (cl, parPop, HIGH_3_fun)
parPop <- parLapply (cl, parPop, HIGH_4_fun)

parPop <- parLapply (cl, parPop, HLTH_1_fun)
parPop <- parLapply (cl, parPop, HLTH_2_fun)
parPop <- parLapply (cl, parPop, HLTH_3_fun)
parPop <- parLapply (cl, parPop, HLTH_4_fun)

parPop <- parLapply (cl, parPop, EDUC_1_fun)
parPop <- parLapply (cl, parPop, EDUC_2_fun)
parPop <- parLapply (cl, parPop, EDUC_3_fun)
parPop <- parLapply (cl, parPop, EDUC_4_fun)

parPop <- parLapply (cl, parPop, CONS_1_fun)
parPop <- parLapply (cl, parPop, CONS_2_fun)
parPop <- parLapply (cl, parPop, CONS_3_fun)

parPop <- parLapply (cl, parPop, NOWK_1_fun)
parPop <- parLapply (cl, parPop, NOWK_2_fun)
parPop <- parLapply (cl, parPop, NOWK_3_fun)
parPop <- parLapply (cl, parPop, NOWK_4_fun)
parPop <- parLapply (cl, parPop, NOWK_5_fun)
parPop <- parLapply (cl, parPop, NOWK_6_fun)

toc()

# 100 - 61 secs
# 5k  - 2682 secs
# 10k - 3262 secs

stopCluster (cl)

myPop2 <- as.data.frame(do.call(rbind, parPop))

myPop2 <- mutate(myPop2, 
                 Act.1.End = as_hms(as.integer(Act.1.End)),
                 Act.2.End = as_hms(round(as.integer(Act.2.End),2)),
                 Act.3.End = as_hms(as.integer(Act.3.End)),
                 Act.4.End = as_hms(as.integer(Act.4.End)),
                 Act.5.End = as_hms(as.integer(Act.5.End)),
                 Act.6.End = as_hms(as.integer(Act.6.End)))

#myPop2 <- myPop2 %>% select(!ends_with("End"),ends_with("End"))


# Save an object to a file
saveRDS(myPop2, file = "pop_data_frame/pop_5000.rds")



##############################################################

#hex6 <- c("#c65a79","#c479ba","#a59fed","#74c4ff","#56e4ff","#7bfffc")
#scales::show_col(hex6)

#myPop2 %>%  
  #filter(planType %in% c("OFFC_2","HLTH_2","OFFC_3","EDUC_4","HIGH_2","INDY_1")) %>% 
  #ggplot(aes(x = Act.1.End, fill = planType)) + 
  #geom_histogram(bins = 60, color = "#ababab") + 
  #facet_wrap(~planType) + 
 # scale_fill_manual(values = hex6) + 
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 90), legend.position = "none") 



######## create one agent for testing #########

#agent <- myPop %>% select(-starts_with("Act"))
#agent <- agent %>% filter(planType == "INDY_2")
#agent <- sample_n(agent,1)

#agents <- sample_n(myPop,100)
#agentWorkXY <- tibble()
#for (i in seq_len(nrow(agents))) {
#  agent <- addXY_ODmethod(agents[i,],refNumber = 99, sec = "INDY")
#  agentWorkXY <- rbind(agentWorkXY,agent)
#}



####obsolete##########
#numberOfAgents <- 500

#myPop <- tibble(agents = rep("agent", numberOfAgents))
#myPop <- mutate(myPop, agents = paste0(agents,1:numberOfAgents))
#myPop <- mutate(myPop, Act.1 = NA, Act.2 = NA, Act.3 = NA, Act.4 = NA, Act.5 = NA, Act.6 = NA)
#myPop <- mutate(myPop, planType = sample(c("FIX1","FIX2","VAR1","SCH1","FAL1"),nrow(myPop), replace=T, c(2,3,3,3,3)))
#myPop <- mutate(myPop, OA = paste0("OA000",rep(1:50,numberOfAgents/50)))
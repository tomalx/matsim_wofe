##############
## PROFILER ##
##############

# For every OA in the region, this script creates a table of census data,
# which is then used to create the appropriate count of agents for each
# 'planType'. 

# This script should be run prior to the popExpander script.


setwd("~/matsim/on_github/matsim_wofe")

###################
# load nomis data #
###################

industry <- read.csv("csv/industry.csv") %>% rename(OA = X2011.output.area)
age <- read.csv("csv/age.csv") %>% rename(OA = X2011.output.area)
ecoAct <- read.csv("csv/economicActivity.csv") %>% rename(OA = X2011.output.area)
#occupation <- read.csv("nomis/occupations.csv") %>% rename(OA = X2011.output.area)
carAvail <- read.csv("csv/carAvailability.csv") %>% rename(OA = homeOA)
ethnicity <- read.csv("csv/ethnicity.csv")

#industry.num <- industry[,-1]
#colSums(industry.num)

##################################
# combine nomis data into one DF #
##################################

profileTypeDF <- age %>% select(OA)

age <- age %>% mutate(dem.pop.total = All.usual.residents, 
                      dem.pop.preSch = Age.0.to.4, 
                      dem.pop.primary = Age.5.to.7 + Age.8.to.9,
                      dem.pop.secondary = Age.10.to.14 + Age.15,
                      dem.pop.65plus = Age.65.to.74 + Age.75.to.84 + Age.85.to.89 + Age.90.and.over) %>% 
                select(OA, dem.pop.total, dem.pop.preSch, dem.pop.primary, dem.pop.secondary, dem.pop.65plus)

industry <- industry %>% mutate(workplace.ind = A.Agriculture..forestry.and.fishing + B.Mining.and.quarrying + C.Manufacturing + D.Electricity..gas..steam.and.air.conditioning.supply + E.Water.supply..sewerage..waste.management.and.remediation.activities + H.Transport.and.storage,
                                 workplace.highStreet = G.Wholesale.and.retail.trade..repair.of.motor.vehicles.and.motor.cycles + I.Accommodation.and.food.service.activities + L.Real.estate.activities + R..S..T..U.Other,
                                 workplace.education = P.Education,
                                 workplace.health = Q.Human.health.and.social.work.activities,
                                 workplace.office = K.Financial.and.insurance.activities + M.Professional..scientific.and.technical.activities + N.Administrative.and.support.service.activities + O.Public.administration.and.defence..compulsory.social.security + J.Information.and.communication,
                                 workplace.construction = F.Construction ) %>% 
                 select(OA, workplace.ind, workplace.highStreet, workplace.education, workplace.health, workplace.office, workplace.construction)

ecoAct <- ecoAct %>%  mutate(dem.inWork = Economically.active - Economically.active..Unemployed,
                             dem.notInWork = Economically.Inactive + Economically.active..Unemployed,
                              dem.partTime = Economically.active..Employee..Part.time,
                              dem.studentNoJob = Economically.inactive..Student..including.full.time.students.,
                              dem.studentPlusJob = Economically.active..Full.time.student,
                              dem.lookAfterFamilyFT = Economically.inactive..Looking.after.home.or.family) %>% 
                        select(OA, dem.inWork, dem.notInWork, dem.partTime, dem.studentNoJob, dem.studentPlusJob, dem.lookAfterFamilyFT)

carAvail <- carAvail %>% mutate(dem.noCars = No.cars.or.vans.in.household) %>%
                          select(OA, dem.noCars)

ethnicity <- ethnicity %>% mutate(dem.white = White, 
                                  dem.BME = Black.African.Caribbean.Black.British + Mixed.multiple.ethnic.groups + Asian.Asian.British + Other.ethnic.group) %>% 
                           select(OA,dem.white,dem.BME)

profileTypeDF <- profileTypeDF %>% left_join(age, by="OA") %>% 
                                    left_join(ecoAct, by="OA") %>%
                                    left_join(industry, by="OA") %>%
                                    left_join(carAvail, by="OA") %>% 
                                    left_join(ethnicity, by="OA")
                                    
rm(age,carAvail,ecoAct,ethnicity,industry) 

  
#######################
## Add Plans         ##
#######################

## function to calculate profile counts from demographic data
## planCountWorkFunx3/4/5 assume some educational escort plans based on preSch / primary age population
## 
## planCountIdleFun assigns plan counts for retirees, unemployed students

planCountWorkFunx3 <- function(df = profileTypeDF, varName = "FOO", landuse){
  
  var1 <- 0.2  # % of agents (excl. sch escorters) who incorporate shopping activities 
  var2 <- 0.3 # % of agents (excl. sch escorters) who incorporate leisure activities
  var3 <- 0.5 # % of agents (excl. sch escorters) with no other activities

  varName1 <- paste0(varName,"_1")
  varName2 <- paste0(varName,"_2")
  varName3 <- paste0(varName,"_3")
  
  df2 <- df %>% 
    mutate( !!varName1 := round( var1 *
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% 
    mutate( !!varName2 := round( var2 *
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% # school escorter
    mutate( !!varName3 := round( var3 *
                                   df[[paste0("workplace.",landuse)]]   ) )
  
}

planCountWorkFunx4 <- function(df = profileTypeDF, varName = "FOO", landuse){
  
  var1 <- 0.1  # % of agents (excl. sch escorters) who incorporate shopping activities 
  var3 <- 0.4 # % of agents (excl. sch escorters) who incorporate leisure activities
  var4 <- 0.25 # % of agents (excl. sch escorters) with no other activities
  
  kids <- df$dem.pop.preSch+df$dem.pop.primary
  siblingRatio <- 0.6 # ratio of siblings in same household
  
  varName1 <- paste0(varName,"_1")
  varName2 <- paste0(varName,"_2")
  varName3 <- paste0(varName,"_3")
  varName4 <- paste0(varName,"_4")
  df2 <- df %>% 
    mutate( !!varName1 := round( ( 1 - (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * var1 *
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% 
    mutate( !!varName2 := round( ( (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * 
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% # school escorter
    mutate( !!varName3 := round( ( 1 - (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * var3 *
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% 
    mutate( !!varName4 := round( ( 1 - (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * var4 *
                                   df[[paste0("workplace.",landuse)]]   ) )  
  
}

planCountWorkFunx5 <- function(df = profileTypeDF, varName = "FOO", landuse){
  
  var1 <- 0.1  # % of agents (excl. sch escorters) who incorporate shopping activities 
  var3 <- 0.4 # % of agents (excl. sch escorters) who incorporate leisure activities
  var4 <- 0.25 # % of agents (excl. sch escorters) with no other activities
  var5 <- 0.25
  
  kids <- df$dem.pop.preSch+df$dem.pop.primary
  siblingRatio <- 0.6 # ratio of siblings in same household
  
  varName1 <- paste0(varName,"_1")
  varName2 <- paste0(varName,"_2")
  varName3 <- paste0(varName,"_3")
  varName4 <- paste0(varName,"_4")
  varName5 <- paste0(varName,"_5")
  
  df2 <- df %>% 
    mutate( !!varName1 := round( ( 1 - (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * var1 *
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% 
    mutate( !!varName2 := round( ( (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * 
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% # school escorter
    mutate( !!varName3 := round( ( 1 - (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * var3 *
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% 
    mutate( !!varName4 := round( ( 1 - (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * var4 *
                                   df[[paste0("workplace.",landuse)]]   ) ) %>% 
    mutate( !!varName5 := round( ( 1 - (kids*siblingRatio) / (dem.inWork + dem.lookAfterFamilyFT) ) * var5 *
                                   df[[paste0("workplace.",landuse)]]   ) )
  
}

planCountNoWorkFun <- function(df = profileTypeDF, varName = "NOWK"){
  
  sVar1 <- 0.3
  sVar2 <- 0.2
  sVar3 <- 0.5
  var3 <- 0.1 # % of agents (excl. sch escorters) who incorporate leisure activities
  var4 <- 0.3 # % of agents (excl. sch escorters) with no other activities
  var5 <- 0.6
  
  varName1 <- paste0(varName,"_1")
  varName2 <- paste0(varName,"_2")
  varName3 <- paste0(varName,"_3")
  varName4 <- paste0(varName,"_4")
  varName5 <- paste0(varName,"_5")
  varName6 <- paste0(varName,"_6")
  
  df2 <- df %>% 
    mutate( !!varName1 := round( (dem.studentNoJob + dem.studentPlusJob) * sVar1) ) %>%
    mutate( !!varName2 := round( (dem.studentNoJob + dem.studentPlusJob) * sVar2) ) %>%
    mutate( !!varName3 := round( (dem.studentNoJob + dem.studentPlusJob) * sVar3) ) %>%
    mutate( !!varName4 := round( (dem.notInWork - dem.studentNoJob) * var3 ) ) %>% 
    mutate( !!varName5 := round( (dem.notInWork - dem.studentNoJob) * var4 ) ) %>% 
    mutate( !!varName6 := round( (dem.notInWork - dem.studentNoJob) * var5 ) )
  
}



profileTypeDF <- planCountWorkFunx4(profileTypeDF,"INDY",landuse = "ind")
profileTypeDF <- planCountWorkFunx4(profileTypeDF,"HIGH",landuse = "highStreet")
profileTypeDF <- planCountWorkFunx4(profileTypeDF,"EDUC",landuse = "education")
profileTypeDF <- planCountWorkFunx4(profileTypeDF,"HLTH",landuse = "health")
profileTypeDF <- planCountWorkFunx5(profileTypeDF,"OFFC",landuse = "office")
profileTypeDF <- planCountWorkFunx3(profileTypeDF,"CONS",landuse = "construction")
profileTypeDF <- planCountNoWorkFun(profileTypeDF)


#profileTypeDF <- profileTypeDF %>% select(-starts_with("dem."), -starts_with("workplace.")) 
rm(planCountNoWorkFun,planCountWorkFunx3,planCountWorkFunx4,planCountWorkFunx5)

any_negative <- function(x){ any(x < 0)}
replace_with_zero <- function(x){ if_else(x < 0,0,x) }
profileTypeDF <- profileTypeDF %>% mutate_if(any_negative, replace_with_zero)
rm(any_negative,replace_with_zero)












##############
## EXPANDER ##
##############

library(splitstackshape)

# This script should be run after the PROFILER script.
# It takes the profileTypeDF - a df of counts for each profile type in each OA -
# and expands this out to create a longer df with each row corresponding to an agent.
# This script also requires a random name generator function to create
# unique agent names.

# create a list of all the profileTypes within the data frame...
profileTypes <- names(profileTypeDF)
profileTypes <- str_subset(profileTypes, "[:upper:]{4}_[1-9]")

# expand out profile counts table so correct number of agents for
# each profileType...


popExpander <- function(){
  pop <- tibble()
  for (i in profileTypes){
    newTibble <- profileTypeDF %>% select(1,i) %>% mutate(planType = print(i))
    newTibble <- expandRows(newTibble,i)
    pop <- rbind(pop, newTibble)
  }
  return(pop)
}

pop <- popExpander()


source("r/help_name_gen.R")
pop <- pop %>% mutate(agentName = suppressWarnings(nameGenX(nrow(pop))))
pop <- pop %>% select(agentName,OA,planType)

#just yate pop
# yateOAs <- read.csv("csv/yateOAs.csv")
# yateOAs <- yateOAs$OA11CD
# yatePop <- pop %>% filter(OA %in% yateOAs)
# rm(yateOAs)

#select OAs (filter out some of the 3490 OAs)
 myOAs <- read.csv("csv/bus_6and7_OAs.csv")
 myOAs <- myOAs$OA11CD
 bus6or7pop <- pop %>% filter(OA %in% myOAs)
 rm(myOAs)


rm(threeLetNames, alphaNumerics, profileTypes, agentNameGen, agentNameGen2, agentNameGenAlphaNum, popExpander, 
   alphaNumSmall, nameGenX, agentNameGenSimple1, agentNameGenSimple2, constonants, vowels)

###################
# profile sums df #
###################

profileSums <- profileTypeDF %>% select(-OA, -starts_with("dem."), -starts_with("workplace.")) %>% 
  mutate(total = rowSums(.))

profileSums <- summarise_all(profileSums, sum)

#ecoActSums <- ecoAct %>% select(All.usual.residents.aged.16.to.74, Economically.active, Economically.Inactive) #%>%  mutate(sum = rowSums(.))
#ecoActSums <- summarise_all(ecoActSums, sum)

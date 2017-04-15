# In 2010 to 2016, any team vs. any team, in the regular season, 
# play type is pass or rush or punt or field goal, 
# in the first or third quarter, goal to go, scoring margin is between -14 and 14

library(stringr)
library(dplyr)
library(lubridate)

nfl <- read.csv("nfl_data.csv")[-16]

nfl <- nfl %>% 
  mutate(touchdown = str_detect(Detail,"touchdown"),
         field_goal_good = str_detect(Detail,"field goal good"),
         field_goal_no_good = str_detect(Detail,"field goal no good"),
         interception = str_detect(Detail,"intercepted"),
         fumble = str_detect(Detail,"fumbles"),
         pass = str_detect(Detail, "pass"),
         complete = str_detect(Detail,"pass complete"),
         incomplete = str_detect(Detail, "pass incomplete"),
         sack = str_detect(Detail, "sack"),
         rush = (pass == F & sack == F & field_goal_good == F & field_goal_no_good == F))

nfl$direction <- ifelse(str_detect(nfl$Detail,"middle"),"middle",
                            ifelse(str_detect(nfl$Detail,"right"),"right",
                                   ifelse(str_detect(nfl$Detail,"left"),"left",NA)))

nfl$Yds <- ifelse(str_detect(nfl$Detail,"field goal"),NA,nfl$Yds)

nfl$Yds <- ifelse(str_detect(nfl$Detail,"incomplete"),0,nfl$Yds)

nfl <- nfl %>% 
  mutate(team_score = as.numeric(str_sub(Score,end = str_locate(Score,"-")[,1]-1)),
         opponent_score = as.numeric(str_sub(Score, start = str_locate(Score,"-")[,1]+1)))

nfl$Location <- str_sub(nfl$Location, start = str_locate(nfl$Location," ")+1) %>% 
  as.numeric()

nfl <- nfl %>% 
  filter(Location == ToGo)

nfl <- nfl[-c(9:10,16)]

nfl <- nfl[c(1:3,8,4:7,24:25,9:23)]

colnames(nfl) <- tolower(colnames(nfl))

nfl$time <- ms(nfl$time)

write.csv(nfl,file="nfl-data.csv",row.names = F)



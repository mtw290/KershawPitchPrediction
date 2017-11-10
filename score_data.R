


#need to join in score data
pitchDat3$date1 <- paste(substr(pitchDat3$date,1,4),
                         substr(pitchDat3$date,6,7),
                         substr(pitchDat3$date,9,10),
                         sep = "_")

#get at bat data from sqlite database again, filter for kershaw and right years
score_dat <- filter(tbl(db,"atbat"), pitcher == 477132) %>% 
  mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
  filter(year > 2015) 

scoreDat <- collect(score_dat)

scoreDat1 <- scoreDat %>% 
  select(num, date, home_team_runs, away_team_runs, inning_side, inning, batter) %>% 
  mutate(LAD = if_else(inning_side == "top", home_team_runs, away_team_runs),
         OPP = if_else(inning_side == "top", away_team_runs, home_team_runs)) %>% 
  mutate(scoreDiff = as.numeric(LAD) - as.numeric(OPP))

hold <- right_join(scoreDat1, pitchDat3, by = c("date" = "date1", "batter" = "batter", "inning" = "inning"))

#3 records are missing - why? not sure, but that at bat by Carlos Gonzalez is missing from the 
#sqlite db, nothing changes in the game so I'll fill with same value from before and after the at bat
pitchDat3 <- hold
pitchDat3$scoreDiff[is.na(pitchDat3$scoreDiff)] <- 0
















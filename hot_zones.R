library(tidyverse)
library(RSQLite)
library(lubridate)
library(pitchRx)
library(dplyr)


##### snap up data from earlier steps
  pitchDat <- read.csv("C://Users/Mike/Documents/kershaw/current_kershaw1.csv")
  pitchDat <- pitchDat %>% 
    mutate(year = year(as.Date(date)), month = month(as.Date(date)))

##### connect to database
db <- src_sqlite("D://pitchfx.sqlite3")

#####
##### 2016
# figure out which batters faced Kershaw in 2017
  playersINeed16 <- filter(pitchDat, year < 2017) %>% distinct(batter)

##### for early and late 2016 - for each batter, figure out what zone they hit best
#####   -number of hits from pitches in each zone/number of pitches in that zone
#####   -standardized by batter so we can tell where each batter's strengths and weaknesses are
#####   -at bats from April and May get last year's data, later at bats get data from 2016
#early 2016
  absINeed_e16 <- filter(tbl(db,"atbat"), batter %in% playersINeed16$batter) %>% 
    mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
    filter(year < 2016) %>% 
    left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) %>% 
    mutate(hit = ifelse(des == "In play, no out", 1, 0), counter = 1) %>% 
    group_by(batter, zone) %>% 
    summarize(heat = sum(hit)/sum(counter)) %>%
    filter(!is.na(zone)) 
  
  heat_early16 <- collect(absINeed_e16) %>% 
    spread(zone, heat)
  
  
  names(heat_early16) <- c("batter", "zone_1", "zone_11", "zone_12", "zone_13", "zone_14",
                       "zone_2", "zone_3", "zone_4", "zone_5", "zone_6", "zone_7",
                       "zone_8", "zone_9")
  heat_early16 <- select(heat_early16, "batter", "zone_1", "zone_2", "zone_3", "zone_4", "zone_5", 
                     "zone_6", "zone_7", "zone_8", "zone_9")
  
  heat_e16 <- heat_early16 %>% 
    gather(zone, heat, -batter) %>% 
    group_by(batter) %>% 
    mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
    select(-starts_with("heat")) %>% 
    spread(zone, new_heat)

#late 2016 - batting data up to end of 2016
  
  absINeed_l16 <- filter(tbl(db,"atbat"), batter %in% playersINeed16$batter) %>% 
    mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
    filter(year < 2017) %>% 
    left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) %>% 
    mutate(hit = ifelse(des == "In play, no out", 1, 0), counter = 1) %>% 
    group_by(batter, zone) %>% 
    summarize(heat = sum(hit)/sum(counter)) %>%
    filter(!is.na(zone)) 
  
  heat_late16 <- collect(absINeed_l16) %>% 
    spread(zone, heat)
  
  
  names(heat_late16) <- c("batter", "zone_1", "zone_11", "zone_12", "zone_13", "zone_14",
                       "zone_2", "zone_3", "zone_4", "zone_5", "zone_6", "zone_7",
                       "zone_8", "zone_9")
  heat_late16 <- select(heat_late16, "batter", "zone_1", "zone_2", "zone_3", "zone_4", "zone_5", 
                     "zone_6", "zone_7", "zone_8", "zone_9")
  
  heat_l16 <- heat_late16 %>% 
    gather(zone, heat, -batter) %>% 
    group_by(batter) %>% 
    mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
    select(-starts_with("heat")) %>% 
    spread(zone, new_heat)
#####
##### do the same thing for 2017
  playersINeed17 <- filter(pitchDat, year == 2017) %>% distinct(batter)
  
  # early 2017
  absINeed_e17 <- filter(tbl(db,"atbat"), batter %in% playersINeed17$batter) %>% 
    mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
    filter(year < 2017) %>% 
    left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) %>% 
    mutate(hit = ifelse(des == "In play, no out", 1, 0), counter = 1) %>% 
    group_by(batter, zone) %>% 
    summarize(heat = sum(hit)/sum(counter)) %>%
    filter(!is.na(zone)) 
  
  heat_early17 <- collect(absINeed_e17) %>% 
    spread(zone, heat)
  
  # %>% 
  #   gather(zone, heat, -batter) %>% 
  #   group_by(batter) %>% 
  #   mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
  #   select(-starts_with("heat")) %>% 
  #   spread(zone, new_heat)
  
  names(heat_early17) <- c("batter", "zone_1", "zone_2", "zone_3", "zone_4", "zone_5", 
                       "zone_6", "zone_7", "zone_8", "zone_9", "zone_11", "zone_12", 
                       "zone_13", "zone_14")
  heat_early17 <- select(heat_early17, "batter", "zone_1", "zone_2", "zone_3", "zone_4", "zone_5", 
                     "zone_6", "zone_7", "zone_8", "zone_9")
  
  heat_e17 <- heat_early17 %>% 
    gather(zone, heat, -batter) %>% 
    group_by(batter) %>% 
    mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
    select(-starts_with("heat")) %>% 
    spread(zone, new_heat)
  
  
  #late 2017
  absINeed_l17 <- filter(tbl(db,"atbat"), batter %in% playersINeed17$batter) %>% 
    mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
    #filter(year < 2016) %>% 
    left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) %>% 
    mutate(hit = ifelse(des == "In play, no out", 1, 0), counter = 1) %>% 
    group_by(batter, zone) %>% 
    summarize(heat = sum(hit)/sum(counter)) %>%
    filter(!is.na(zone)) 
  
  heat_late17 <- collect(absINeed_l17) %>% 
    spread(zone, heat)
  
  
  names(heat_late17) <- c("batter", "zone_1", "zone_11", "zone_12", "zone_13", "zone_14",
                       "zone_2", "zone_3", "zone_4", "zone_5", "zone_6", "zone_7",
                       "zone_8", "zone_9")
  heat_late17 <- select(heat_late17, "batter", "zone_1", "zone_2", "zone_3", "zone_4", "zone_5", 
                     "zone_6", "zone_7", "zone_8", "zone_9")
  
  heat_l17 <- heat_late17 %>% 
    gather(zone, heat, -batter) %>% 
    group_by(batter) %>% 
    mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
    select(-starts_with("heat")) %>% 
    spread(zone, new_heat)
  

  
  ##### for early and late 2016 - for each batter, figure out what pitches they hit best
  #####   -number of swinging strikes by pitch type/number of pitches of that type
  #####   -standardized by across batters so we can tell where each batter's strengths and weaknesses are
  #####   -at bats from April and May get last year's data, later at bats get data from same year
  #early 2016
  swingMiss_e16 <- filter(tbl(db,"atbat"), batter %in% playersINeed16$batter) %>% 
    mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
    filter(year < 2016) %>% 
    left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) 
  
  miss_e16 <- collect(swingMiss_e16)%>% 
    mutate(swingMiss = ifelse(des == "Swinging Strike", 1, 0), counter = 1) %>% 
    mutate(pitch_type = recode(pitch_type, "FA" = "FA", "CU" = "CU", "SL" = "SL", "FF" = "FA", 
                               "FT" = "FA", "FC" = "FA")) %>% 
    group_by(batter, pitch_type) %>% 
    summarize(swingStrike = sum(swingMiss)/sum(counter), pitch_type_count = sum(counter)) %>%
    filter(pitch_type %in% c("FA", "CU", "SL"))  %>% 
    group_by(pitch_type) %>% 
    mutate(swingMissStd = (swingStrike-mean(swingStrike))/sd(swingStrike)) %>% 
    select(batter, pitch_type, swingMissStd) %>% 
    spread(pitch_type, swingMissStd) %>% 
    replace_na(list(FA = 0, CU = 0, SL = 0))
  
  names(miss_e16) <- c("batter","CU_miss", "FA_miss", "SL_miss")
  
  #late 2016
  swingMiss_l16 <- filter(tbl(db,"atbat"), batter %in% playersINeed16$batter) %>% 
    mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
    filter(year < 2017) %>% 
    left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) 
  
  miss_l16 <- collect(swingMiss_l16)%>% 
    mutate(swingMiss = ifelse(des == "Swinging Strike", 1, 0), counter = 1) %>% 
    mutate(pitch_type = recode(pitch_type, "FA" = "FA", "CU" = "CU", "SL" = "SL", "FF" = "FA", 
                               "FT" = "FA", "FC" = "FA")) %>% 
    group_by(batter, pitch_type) %>% 
    summarize(swingStrike = sum(swingMiss)/sum(counter), pitch_type_count = sum(counter)) %>%
    filter(pitch_type %in% c("FA", "CU", "SL"))  %>% 
    group_by(pitch_type) %>% 
    mutate(swingMissStd = (swingStrike-mean(swingStrike))/sd(swingStrike)) %>% 
    select(batter, pitch_type, swingMissStd) %>% 
    spread(pitch_type, swingMissStd) %>% 
    replace_na(list(FA = 0, CU = 0, SL = 0))
  
  names(miss_l16) <- c("batter","CU_miss", "FA_miss", "SL_miss")
  
  
  #####
  ##### 2017
  # figure out which batters faced Kershaw in 2017
  playersINeed17 <- filter(pitchDat, year == 2017) %>% distinct(batter)
  
  #early 2017
  swingMiss_e17 <- filter(tbl(db,"atbat"), batter %in% playersINeed17$batter) %>% 
    mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
    filter(year < 2017) %>% 
    left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) 
  
  miss_e17 <- collect(swingMiss_e17)%>% 
    mutate(swingMiss = ifelse(des == "Swinging Strike", 1, 0), counter = 1) %>% 
    mutate(pitch_type = recode(pitch_type, "FA" = "FA", "CU" = "CU", "SL" = "SL", "FF" = "FA", 
                               "FT" = "FA", "FC" = "FA")) %>% 
    group_by(batter, pitch_type) %>% 
    summarize(swingStrike = sum(swingMiss)/sum(counter), pitch_type_count = sum(counter)) %>%
    filter(pitch_type %in% c("FA", "CU", "SL"))  %>% 
    group_by(pitch_type) %>% 
    mutate(swingMissStd = (swingStrike-mean(swingStrike))/sd(swingStrike)) %>% 
    select(batter, pitch_type, swingMissStd) %>% 
    spread(pitch_type, swingMissStd) %>% 
    replace_na(list(FA = 0, CU = 0, SL = 0))
  
  names(miss_e17) <- c("batter","CU_miss", "FA_miss", "SL_miss")
  
  #late 2017
  swingMiss_l17 <- filter(tbl(db,"atbat"), batter %in% playersINeed17$batter) %>% 
    mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
    #filter(year < 2017) %>% 
    left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) 
  
  miss_l17 <- collect(swingMiss_l17)%>% 
    mutate(swingMiss = ifelse(des == "Swinging Strike", 1, 0), counter = 1) %>% 
    mutate(pitch_type = recode(pitch_type, "FA" = "FA", "CU" = "CU", "SL" = "SL", "FF" = "FA", 
                               "FT" = "FA", "FC" = "FA")) %>% 
    group_by(batter, pitch_type) %>% 
    summarize(swingStrike = sum(swingMiss)/sum(counter), pitch_type_count = sum(counter)) %>%
    filter(pitch_type %in% c("FA", "CU", "SL"))  %>% 
    group_by(pitch_type) %>% 
    mutate(swingMissStd = (swingStrike-mean(swingStrike))/sd(swingStrike)) %>% 
    select(batter, pitch_type, swingMissStd) %>% 
    spread(pitch_type, swingMissStd) %>% 
    replace_na(list(FA = 0, CU = 0, SL = 0))
  
  names(miss_l17) <- c("batter","CU_miss", "FA_miss", "SL_miss")
  
  

#######
###### Join the queried data sets
  newDat_e16 <- inner_join(heat_e16, miss_e16, by = c("batter" = "batter"))
  newDat_l16 <- inner_join(heat_l16, miss_l16, by = c("batter" = "batter"))
  newDat_e17 <- inner_join(heat_e17, miss_e17, by = c("batter" = "batter"))
  newDat_l17 <- inner_join(heat_l17, miss_l17, by = c("batter" = "batter"))

#####figure out which batters in 2016/7 don't have data from previous years - for these
##players use same year data for entire year
  noPre16 <- pitchDat %>% 
    filter(year == 2016) %>% 
    mutate(noPre16 = !batter %in% heat_e16$batter) %>% 
    filter(noPre16 == T) %>% 
    distinct(batter)
  
  noPre17 <- pitchDat %>% 
    filter(year == 2017) %>% 
    mutate(noPre16 = !batter %in% heat_e17$batter) %>% 
    filter(noPre16 == T) %>% 
    distinct(batter)

##### place hot zone data into pitch data frame
  #break pitch data up by year
    pitchDat16 <- filter(pitchDat, year == 2016)
    pitchDat17 <- filter(pitchDat, year == 2017)
    
  #place hot zone data in each year using end of year data
    pitchDat16.1 <- inner_join(newDat_l16, pitchDat16, by = c("batter" = "batter"))
    pitchDat17.1 <- inner_join(newDat_l17, pitchDat17, by = c("batter" = "batter"))
    
  #separate out records for which we are going to use last season's data 
    #keep late season data for these records
    pitchDat16.2 <- pitchDat16.1 %>% 
      filter(month > 5 | batter %in% noPre16$batter)
    pitchDat17.2 <- pitchDat17.1 %>% 
      filter(month > 5 | batter %in% noPre17$batter)
    #use early season for these records
    pitchDat16.3 <- pitchDat16 %>% 
      filter(month < 6 & !batter %in% noPre16$batter) %>% 
      select(-starts_with("zone_")) %>% 
      inner_join(newDat_e16,., by = c("batter" = "batter"))
    pitchDat17.3 <- pitchDat17 %>% 
      filter(month < 6 & !batter %in% noPre17$batter) %>% 
      select(-starts_with("zone_")) %>% 
      inner_join(newDat_e17,., by = c("batter" = "batter"))
    
#reconstitute data set
    pitchDat2 <- rbind(pitchDat16.2, pitchDat16.3, pitchDat17.2, pitchDat17.3)



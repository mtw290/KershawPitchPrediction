library(tidyverse)
library(RSQLite)
library(lubridate)
library(pitchRx)
library(dplyr)

pitchDat <- read.csv("C://Users/Mike/Documents/kershaw/current_kershaw.csv")
pitchDat <- pitchDat %>% 
  mutate(year = year(as.Date(date)), month = month(as.Date(date)))

db <- src_sqlite("D://pitchfx.sqlite3")

# db
# atBats <- tbl(db, "atbat")
# atBats
# tbl(db,"action")



playersINeed16 <- filter(pitchDat, year < 2017) %>% distinct(batter)

#early 2016
absINeed_e16 <- filter(tbl(db,"atbat"), batter %in% playersINeed16$batter) %>% 
  mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
  filter(year < 2016) %>% 
  left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) %>% 
  mutate(hit = ifelse(des == "In play, no out", 1, 0), counter = 1) %>% 
  group_by(batter, zone) %>% 
  summarize(heat = sum(hit)/sum(counter)) %>%
  filter(!is.na(zone)) 

heat_e16 <- collect(absINeed_e16) %>% 
  spread(zone, heat) %>% 
  gather(zone, heat, -batter) %>% 
  group_by(batter) %>% 
  mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
  select(-starts_with("heat")) %>% 
  spread(zone, new_heat)

names(heat_e16) <- c("batter", "zone_1", "zone_11", "zone_12", "zone_13", "zone_14",
                     "zone_2", "zone_3", "zone_4", "zone_5", "zone_6", "zone_7",
                     "zone_8", "zone_9")

#late 2016 - batting data up to end of 2016

absINeed_l16 <- filter(tbl(db,"atbat"), batter %in% playersINeed16$batter) %>% 
  mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
  filter(year < 2017) %>% 
  left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) %>% 
  mutate(hit = ifelse(des == "In play, no out", 1, 0), counter = 1) %>% 
  group_by(batter, zone) %>% 
  summarize(heat = sum(hit)/sum(counter)) %>%
  filter(!is.na(zone)) 

heat_l16 <- collect(absINeed_l16) %>% 
  spread(zone, heat) %>% 
  gather(zone, heat, -batter) %>% 
  group_by(batter) %>% 
  mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
  select(-starts_with("heat")) %>% 
  spread(zone, new_heat)

names(heat_l16) <- c("batter", "zone_1", "zone_11", "zone_12", "zone_13", "zone_14",
                     "zone_2", "zone_3", "zone_4", "zone_5", "zone_6", "zone_7",
                     "zone_8", "zone_9")

#2017
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

heat_e17 <- collect(absINeed_e17) %>% 
  spread(zone, heat) %>% 
  gather(zone, heat, -batter) %>% 
  group_by(batter) %>% 
  mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
  select(-starts_with("heat")) %>% 
  spread(zone, new_heat)

names(heat_e16) <- c("batter", "zone_1", "zone_11", "zone_12", "zone_13", "zone_14",
                     "zone_2", "zone_3", "zone_4", "zone_5", "zone_6", "zone_7",
                     "zone_8", "zone_9")

#late 2017
absINeed_l17 <- filter(tbl(db,"atbat"), batter %in% playersINeed17$batter) %>% 
  mutate(year = as.numeric(substr(start_tfs_zulu,1,4))) %>% 
  #filter(year < 2016) %>% 
  left_join(.,tbl(db,"pitch"), by = c("num", "gameday_link")) %>% 
  mutate(hit = ifelse(des == "In play, no out", 1, 0), counter = 1) %>% 
  group_by(batter, zone) %>% 
  summarize(heat = sum(hit)/sum(counter)) %>%
  filter(!is.na(zone)) 

heat_l17 <- collect(absINeed_l17) %>% 
  spread(zone, heat) %>% 
  gather(zone, heat, -batter) %>% 
  group_by(batter) %>% 
  mutate(new_heat = (heat-mean(heat))/sd(heat)) %>% 
  select(-starts_with("heat")) %>% 
  spread(zone, new_heat)

names(heat_l17) <- c("batter", "zone_1", "zone_11", "zone_12", "zone_13", "zone_14",
                     "zone_2", "zone_3", "zone_4", "zone_5", "zone_6", "zone_7",
                     "zone_8", "zone_9")


pitchDat <- pitchDat %>% mutate(zone_1 = 0, zone_11 = 0, zone_12 = 0, zone_13 = 0, zone_14 = 0,
                                zone_2 = 0, zone_3 = 0, zone_4 = 0, zone_5 = 0, zone_6 = 0, zone_7 = 0,
                                zone_8 = 0, zone_9 = 0)

neededNames <-  c("zone_1", "zone_11", "zone_12", "zone_13", "zone_14",
                  "zone_2", "zone_3", "zone_4", "zone_5", "zone_6", "zone_7",
                  "zone_8", "zone_9")
needIndex <- vector()
for (i in 1:length(neededNames)){
  needIndex <- c(needIndex, which(names(pitchDat) == neededNames[i]))
}


for (i in 1:nrow(pitchDat)){
  
  if (pitchDat$year[i] == 2016 & pitchDat$month[i] > 6){
    pitchDat[i,needIndex] <- filter(heat_e17, batter == pitchDat$batter[i])[,2:14]
    
  }
}



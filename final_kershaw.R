library(stattleshipR)
library(tidyverse)
library(lubridate)
library(dummies)
set_token("f685110b98683b562952ecdc164c90d8")

setwd("C://Users/Mike/Documents/kershaw/KershawPictchPrediction/")

source("data_grab.R")

kershaw16 <- get_pitches("mlb-clayton-kershaw", 2016)
set_token("f685110b98683b562952ecdc164c90d8")

kershaw17 <- get_pitches(pitcher = "mlb-clayton-kershaw", year = 2017)

source("savant_data.R")
source("hot_zones.R")

#####
pitchDat4 <- pitchDat3 %>% 
  ungroup() %>% 
  arrange(date, pitch_count) %>% 
  mutate(index = 1000:(nrow(pitchDat3)+999)) %>% 
  select(index,batter, starts_with("zone"), starts_with("next"), starts_with("cur"), CU_miss, FA_miss, SL_miss,
         at_bat_pitch_count, balls, strikes, on_3b, on_2b, on_1b, lp_zone, lp_type, bats_l, 
         inning, pitch_count, pitch_type.x,outs_when_up, scoreDiff) %>% 
  mutate(mon_third = if_else(on_3b == "null", -1, 1),
         mon_second = if_else(on_2b == "null", -1, 1),
         mon_first = if_else(on_1b == "null", -1, 1),
         #pitch_count = (pitch_count - mean(pitch_count))/sd(pitch_count),
         scoreStd = scoreDiff, #(scoreDiff-mean(scoreDiff))/sd(scoreDiff),
         lp_vertical = if_else(lp_zone %in% c(1:3, 11, 12), "high", 
                               if_else(lp_zone %in% c(4:6), "middle", "low")),
         lp_horizontal = if_else(lp_zone %in% c(1, 4, 7, 11, 13), "umpLeft", 
                                 if_else(lp_zone %in% c(2, 5, 8), "umpMiddle", "umpRight")),
         target_vertical = if_else(zone %in% c(1:3, 11, 12), "high", 
                                   if_else(zone %in% c(4:6), "middle", "low")),
         target_horizontal = if_else(zone %in% c(1, 4, 7, 11, 13), "umpLeft", 
                                     if_else(zone %in% c(2, 5, 8), "umpMiddle", "umpRight")),
         target_type = pitch_type.x) %>% 
  select(-pitch_type.x, -lp_zone, -zone)


outsd <- ifelse(dummy(pitchDat4$outs_when_up)==1, 1, -1)
strikesd <- ifelse(dummy(pitchDat4$strikes)==1, 1, -1)
ballsd <- ifelse(dummy(pitchDat4$balls)==1, 1, -1)
ab_countd <- ifelse(dummy(pitchDat4$at_bat_pitch_count)==1, 1, -1)
lp_typed <- ifelse(dummy(pitchDat4$lp_type)==1, 1, -1)
inningd <- ifelse(dummy(pitchDat4$inning)==1, 1, -1)
lp_hord <- ifelse(dummy(pitchDat4$lp_horizontal)==1, 1, -1)
lp_verd <- ifelse(dummy(pitchDat4$lp_vertical)==1, 1, -1)


dum_var <- data.frame(outsd, strikesd, ballsd, ab_countd, lp_typed, inningd, lp_hord, lp_verd)

names(dum_var) <- sub(".....1", "_", names(dum_var))






pitchDat5 <- pitchDat4 %>% 
  select(-starts_with("on_"), -outs_when_up, -strikes, -balls, -at_bat_pitch_count,
         -lp_type,-inning, -lp_horizontal, -lp_vertical, -scoreDiff) %>% 
  cbind(dum_var) %>% 
  filter(target_type %in% c("FA", "SL", "CU"))
#####

write.csv(pitchDat5, "current_kershaw2.csv")


source("last_data_clean_step.R")
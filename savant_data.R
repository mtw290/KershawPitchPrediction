library(lubridate)
library(tidyverse)
library(pitchRx)
setwd("C://Users/Mike/Documents/kershaw/")

#join savant data and stattleship data
kershawSavant <- read.csv("C://Users/Mike/Documents/kershaw/dodger_savant.csv") %>% 
              filter(player_name == "Clayton Kershaw") %>% 
              mutate(game_date = as.Date(game_date)) %>% 
              arrange(game_date, inning, at_bat_number, pitch_number) %>% 
              group_by(game_date, inning) %>% 
              mutate(inning_pitch_count = row_number())

new_kershaw <- rbind(read.csv("kershaw_16_current.csv"), read.csv("kershaw_17_current.csv")) %>% 
            mutate(date = ymd(paste(year(pitched_at),month(pitched_at),day(pitched_at)))) %>%
            mutate(date = as.Date(ifelse(date %in% kershawSavant$game_date, date, date-1), origin = "1970-01-01")) %>% 
            filter(date > as.Date("2016-04-01")) %>% 
            group_by(game_id, inning) %>% 
            mutate(inning_pitch_count = row_number()) %>% 
            inner_join(kershawSavant, by = c("date" = "game_date", "inning" = "inning", 
                                                         "inning_pitch_count" = "inning_pitch_count"))






batter_data_late <- data.frame(next1_avg = new_kershaw$l1_nbatting_average, next1_bbpa = new_kershaw$l1_nwalks_per_plate_appearance,
                          next1_iso = new_kershaw$l1_niso, next1_hits = new_kershaw$l1_nhits,
                          next1_hr = new_kershaw$l1_nhome_runs, next1_ops = new_kershaw$l1_nops,
                          next1_stkout = new_kershaw$l1_nstrikeouts, next1_rbi = new_kershaw$l1_nrbi,
                          
                          next2_avg = new_kershaw$l2_nbatting_average, next2_bbpa = new_kershaw$l2_nwalks_per_plate_appearance,
                          next2_iso = new_kershaw$l2_niso, next2_hits = new_kershaw$l2_nhits,
                          next2_hr = new_kershaw$l2_nhome_runs, next2_ops = new_kershaw$l2_nops,
                          next2_stkout = new_kershaw$l2_nstrikeouts, next2_rbi = new_kershaw$l2_nrbi,
                          
                          cur_avg = new_kershaw$batting_average_cur, cur_bbpa = new_kershaw$walks_per_plate_appearance_cur,
                          cur_iso = new_kershaw$iso_cur, cur_hits = new_kershaw$hits_cur,
                          cur_hr = new_kershaw$home_runs_cur, cur_ops = new_kershaw$ops_cur,
                          cur_stkout = new_kershaw$strikeouts_cur, cur_rbi = new_kershaw$rbi_cur)

batter_data_early <- data.frame(next1_avg = new_kershaw$e1_nbatting_average, next1_bbpa = new_kershaw$e1_nwalks_per_plate_appearance,
                               next1_iso = new_kershaw$e1_niso, next1_hits = new_kershaw$e1_nhits,
                               next1_hr = new_kershaw$e1_nhome_runs, next1_ops = new_kershaw$e1_nops,
                               next1_stkout = new_kershaw$e1_nstrikeouts, next1_rbi = new_kershaw$e1_nrbi,
                               
                               next2_avg = new_kershaw$e2_nbatting_average, next2_bbpa = new_kershaw$e2_nwalks_per_plate_appearance,
                               next2_iso = new_kershaw$e2_niso, next2_hits = new_kershaw$e2_nhits,
                               next2_hr = new_kershaw$e2_nhome_runs, next2_ops = new_kershaw$e2_nops,
                               next2_stkout = new_kershaw$e2_nstrikeouts, next2_rbi = new_kershaw$e2_nrbi,
                               
                               cur_avg = new_kershaw$batting_average_past, cur_bbpa = new_kershaw$walks_per_plate_appearance_past,
                               cur_iso = new_kershaw$iso_past, cur_hits = new_kershaw$hits_past,
                               cur_hr = new_kershaw$home_runs_past, cur_ops = new_kershaw$ops_past,
                               cur_stkout = new_kershaw$strikeouts_past, cur_rbi = new_kershaw$rbi_past)


early_index <- which(month(new_kershaw$date) < 6)

batter_data <- batter_data_late
batter_data[early_index,] <- batter_data_early[early_index,]

remove_list <- c(grep("l1", names(new_kershaw)), grep("l2", names(new_kershaw)),
                 grep("e1", names(new_kershaw)), grep("e2", names(new_kershaw)),
                 grep("_cur", names(new_kershaw)), grep("_past", names(new_kershaw)))

new_kershaw <- new_kershaw[,-remove_list]
new_kershaw <- data.frame(batter_data, new_kershaw)

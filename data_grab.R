setwd("C://Users/Mike/Documents/kershaw/")



get_pitches <- function(pitcher, year){
  library(stattleshipR)
  library(tidyverse)

  set_token("")

  #api call for pitch data
  a <- ss_get_result(sport = "baseball", 
                league = "mlb", 
                ep = "pitches", 
                query = list(pitcher_id = pitcher,
                             season_id = paste("mlb-", year, sep = ""),
                             interval_type = "regularseason"),
                walk = T)
  
  #put pitch data into a data frame
  b <- do.call("rbind", lapply(a, function(x) x$pitches))
  
  #put hitter data into a data frame
  c <- do.call("rbind", lapply(a, function(x) x$hitters))
    #clean out duplicates from the hitter data frame
    c1 <- filter(c, !duplicated(id))
  
  #join pitch data with hitter data
  e <- inner_join(b, c1, by = c("hitter_id" = "id"))
  
  #api call for player stat data from previous year
  f <- ss_get_result(sport = "baseball",
                     league = "mlb",
                     ep = "player_season_stats",
                     query = list(season_id = paste("mlb-", (year-1), sep = ""),
                                  on = paste((year-1), "-11-30", sep = "")),
                     walk = T)
  g <- do.call("rbind", lapply(f, function(x) x$player_season_stats))
  keepList <- which(names(g) %in% c("hits", "batting_average", "walks_per_plate_appearance", "ops", "rbi",
                                    "stolen_bases", "home_runs","iso", "strikeouts"))
  g1 <- g[,keepList] %>%
    rename(hits_past = hits, 
           batting_average_past = batting_average, 
           walks_per_plate_appearance_past = walks_per_plate_appearance,
           ops_past = ops, 
           rbi_past = rbi, 
           stolen_bases_past = stolen_bases, 
           home_runs_past = home_runs, 
           iso_past = iso, 
           strikeouts_past = strikeouts) %>% 
        apply(., 2, function(x) (x-mean(x, na.rm = T))/sd(x, na.rm = T)) %>% 
        data.frame(.,player_id = g$player_id) %>% 
        filter(player_id %in% e$hitter_id) %>% 
        filter(!is.na(apply(.[,1:8], 1, sum))) %>% 
        right_join(., e, by = c("player_id" = "hitter_id"))
  
  
  
  h <- ss_get_result(sport = "baseball",
                     league = "mlb",
                     ep = "player_season_stats",
                     query = list(season_id=paste("mlb-", year, sep = ""),
                                  on = paste(year, "-10-01", sep = ""),
                                  interval_type = "regularseason"),
                     walk = T)
  i <- do.call("rbind", lapply(h, function(x) x$player_season_stats))
  
  keepList <- which(names(i) %in% c("hits", "batting_average", "walks_per_plate_appearance", "ops", "rbi",
                                    "stolen_bases", "home_runs","iso", "strikeouts"))
  
  j <- i[,keepList] %>% 
        rename(hits_cur = hits, 
               batting_average_cur = batting_average, 
               walks_per_plate_appearance_cur = walks_per_plate_appearance,
               ops_cur = ops, 
               rbi_cur = rbi, 
               stolen_bases_cur = stolen_bases, 
               home_runs_cur = home_runs, 
               iso_cur = iso, 
               strikeouts_cur = strikeouts) %>% 
        apply(., 2, function(x) (x-mean(x, na.rm = T))/sd(x, na.rm = T)) %>% 
        data.frame(.,player_id = i$player_id) %>% 
        filter(player_id %in% e$hitter_id) %>% 
        filter(!is.na(apply(.[,1:8], 1, sum))) %>% 
        right_join(., g1, by = c("player_id" = "player_id")) %>% 
        arrange(game_id, pitch_count) %>% 
        group_by(game_id) %>% 
        mutate(at_bat_outs1 = cumsum(is_at_bat_over)-is_at_bat_over+1) %>% 
        mutate(batting_average_past = ifelse(is.na(batting_average_past), yes = batting_average_cur, no = batting_average_past),
               walks_per_plate_appearance_past = ifelse(is.na(walks_per_plate_appearance_past), walks_per_plate_appearance_cur, 
                                                   walks_per_plate_appearance_past),
               iso_past = ifelse(is.na(iso_past), iso_cur, iso_past),
               hits_past = ifelse(is.na(hits_past), hits_cur, hits_past), 
               home_runs_past = ifelse(is.na(home_runs_past), home_runs_cur, home_runs_past),
               ops_past = ifelse(is.na(ops_past), ops_cur, ops_past),
               strikeouts_past = ifelse(is.na(strikeouts_past), strikeouts_cur, strikeouts_past),
               rbi_past = ifelse(is.na(rbi_past), rbi_cur, rbi_past),
               stolen_bases_past = ifelse(is.na(stolen_bases_past), stolen_bases_cur, stolen_bases_past))
  #this is all pitches for the entire year for requested pitcher with past and present year batter data
  #   j
  source("early_season_next_2_batters.R", local = T)
  source("late_season_next_2_batters.R", local = T)
  k <- inner_join(early_season_next, early_season_next2_1) %>% 
                  inner_join(late_season_next) %>% 
                  inner_join(late_season_next2_1) %>% 
                  inner_join(j)
  
}

# 
# kershaw16 <- get_pitches("mlb-clayton-kershaw", 2016)
# kershaw17 <- get_pitches("mlb-clayton-kershaw", 2017)
# 
# source("savant_data.R")



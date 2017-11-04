early_next <- j %>%
  #condense so that each batter has one stats record
  group_by(game_id, at_bat_outs1) %>% 
  summarize(e1_nbatting_average = mean(batting_average_past),
            e1_nwalks_per_plate_appearance = mean(walks_per_plate_appearance_past), 
            e1_niso = mean(iso_past), 
            e1_nhits = mean(hits_past),
            e1_nhome_runs = mean(home_runs_past),
            e1_nops = mean(ops_past),
            e1_nstrikeouts = mean(strikeouts_past),
            e1_nrbi = mean(rbi_past))%>%
  #add column that we can use to figure out if the "at bat" is the last of the game - 
  #we need to figure out who next batter would be
  group_by(game_id) %>% 
  mutate(nbatters = max(at_bat_outs1))


early1_keep_max <- early_next %>% 
  filter(at_bat_outs1 == nbatters) %>% 
  mutate(nbatters = ((at_bat_outs1) %% 9)+1) %>% 
  select(game_id, at_bat_outs1, nbatters)



early1_thru_9 <- early_next[early_next$at_bat_outs1 < 10,]

early1_max <- inner_join(early1_keep_max, early1_thru_9, by = c("game_id" = "game_id", "nbatters" = "at_bat_outs1")) %>% 
  select(-starts_with("nbatters"))

early_season_next <- filter(early_next, at_bat_outs1 != nbatters) %>% 
  select(game_id, at_bat_outs1) %>% 
  mutate(at_bat_outs1 = at_bat_outs1+1) %>% 
  inner_join(early_next) %>% 
  select(-starts_with("nbatters")) %>% 
  mutate(at_bat_outs1 = at_bat_outs1-1) %>% 
  rbind(early1_max)



# batter after next
early_next2 <- j %>%
  #condense so that each batter has one stats record
  group_by(game_id, at_bat_outs1) %>% 
  summarize(e2_nbatting_average = mean(batting_average_past),
            e2_nwalks_per_plate_appearance = mean(walks_per_plate_appearance_past), 
            e2_niso = mean(iso_past), 
            e2_nhits = mean(hits_past),
            e2_nhome_runs = mean(home_runs_past),
            e2_nops = mean(ops_past),
            e2_nstrikeouts = mean(strikeouts_past),
            e2_nrbi = mean(rbi_past))%>%
  #add column that we can use to figure out if the "at bat" is the last of the game - 
  #we need to figure out who next batter would be
  group_by(game_id) %>% 
  mutate(nbatters = max(at_bat_outs1))


early2_keep_max <- early_next2 %>% 
  filter(at_bat_outs1 == nbatters) %>% 
  mutate(nbatters = ifelse((((at_bat_outs1)+2) %% 9) == 0, 9, (((at_bat_outs1)+2) %% 9))) %>% 
  select(game_id, at_bat_outs1, nbatters)



early2_thru_9 <- early_next2[early_next2$at_bat_outs1 < 10,]

early2_max <- inner_join(early2_keep_max, early2_thru_9, by = c("game_id" = "game_id", "nbatters" = "at_bat_outs1")) %>% 
  select(-starts_with("nbatters"))

early_season_next2 <- filter(early_next2, at_bat_outs1 != nbatters) %>% 
  select(game_id, at_bat_outs1) %>% 
  mutate(at_bat_outs1 = at_bat_outs1+2) %>% 
  inner_join(early_next2) %>% 
  select(-starts_with("nbatters")) %>% 
  mutate(at_bat_outs1 = at_bat_outs1-2) %>% 
  rbind(early2_max)


#max-1
early2_keep_max_1 <- early_next2 %>% 
  filter(at_bat_outs1 == nbatters-1) %>% 
  mutate(nbatters = ifelse((((at_bat_outs1)+2) %% 9) == 0, 9, (((at_bat_outs1)+2) %% 9))) %>% 
  select(game_id, at_bat_outs1, nbatters)



early2_thru_9 <- early_next2[early_next2$at_bat_outs1 < 10,]

early2_max1 <- inner_join(early2_keep_max_1, early2_thru_9, by = c("game_id" = "game_id", "nbatters" = "at_bat_outs1")) %>% 
  select(-starts_with("nbatters"))

early_season_next2_1<- rbind(early_season_next2, early2_max1)



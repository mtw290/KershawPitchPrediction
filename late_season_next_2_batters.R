late_next <- j %>%
  #condense so that each batter has one stats record
  group_by(game_id, at_bat_outs1) %>% 
  summarize(l1_nbatting_average = mean(batting_average_cur),
            l1_nwalks_per_plate_appearance = mean(walks_per_plate_appearance_cur), 
            l1_niso = mean(iso_cur), 
            l1_nhits = mean(hits_cur),
            l1_nhome_runs = mean(home_runs_cur),
            l1_nops = mean(ops_cur),
            l1_nstrikeouts = mean(strikeouts_cur),
            l1_nrbi = mean(rbi_cur))%>%
  #add column that we can use to figure out if the "at bat" is the last of the game - 
  #we need to figure out who next batter would be
  group_by(game_id) %>% 
  mutate(nbatters = max(at_bat_outs1))


late1_keep_max <- late_next %>% 
  filter(at_bat_outs1 == nbatters) %>% 
  mutate(nbatters = ((at_bat_outs1) %% 9)+1) %>% 
  select(game_id, at_bat_outs1, nbatters)



late1_thru_9 <- late_next[late_next$at_bat_outs1 < 10,]

late1_max <- inner_join(late1_keep_max, late1_thru_9, by = c("game_id" = "game_id", "nbatters" = "at_bat_outs1")) %>% 
  select(-starts_with("nbatters"))

late_season_next <- filter(late_next, at_bat_outs1 != nbatters) %>% 
  select(game_id, at_bat_outs1) %>% 
  mutate(at_bat_outs1 = at_bat_outs1+1) %>% 
  inner_join(late_next) %>% 
  select(-starts_with("nbatters")) %>% 
  mutate(at_bat_outs1 = at_bat_outs1-1) %>% 
  rbind(late1_max)



# batter after next
late_next2 <- j %>%
  #condense so that each batter has one stats record
  group_by(game_id, at_bat_outs1) %>% 
  summarize(l2_nbatting_average = mean(batting_average_cur),
            l2_nwalks_per_plate_appearance = mean(walks_per_plate_appearance_cur), 
            l2_niso = mean(iso_cur), 
            l2_nhits = mean(hits_cur),
            l2_nhome_runs = mean(home_runs_cur),
            l2_nops = mean(ops_cur),
            l2_nstrikeouts = mean(strikeouts_cur),
            l2_nrbi = mean(rbi_cur))%>%
  #add column that we can use to figure out if the "at bat" is the last of the game - 
  #we need to figure out who next batter would be
  group_by(game_id) %>% 
  mutate(nbatters = max(at_bat_outs1))


late2_keep_max <- late_next2 %>% 
  filter(at_bat_outs1 == nbatters) %>% 
  mutate(nbatters = ifelse(((at_bat_outs1) %% 9)+2 == 10, 1, ((at_bat_outs1) %% 9)+2)) %>% 
  select(game_id, at_bat_outs1, nbatters)



late2_thru_9 <- late_next2[late_next2$at_bat_outs1 < 10,]

late2_max <- inner_join(late2_keep_max, late2_thru_9, by = c("game_id" = "game_id", "nbatters" = "at_bat_outs1")) %>% 
  select(-starts_with("nbatters"))

late_season_next2 <- filter(late_next2, at_bat_outs1 != nbatters) %>% 
  select(game_id, at_bat_outs1) %>% 
  mutate(at_bat_outs1 = at_bat_outs1+2) %>% 
  inner_join(late_next2) %>% 
  select(-starts_with("nbatters")) %>% 
  mutate(at_bat_outs1 = at_bat_outs1-2) %>% 
  rbind(late2_max)

#max-1
late2_keep_max_1 <- late_next2 %>% 
  filter(at_bat_outs1 == nbatters-1) %>% 
  mutate(nbatters = ifelse((((at_bat_outs1)+2) %% 9) == 0, 9, (((at_bat_outs1)+2) %% 9))) %>% 
  select(game_id, at_bat_outs1, nbatters)



late2_thru_9 <- late_next2[late_next2$at_bat_outs1 < 10,]

late2_max1 <- inner_join(late2_keep_max_1, late2_thru_9, by = c("game_id" = "game_id", "nbatters" = "at_bat_outs1")) %>% 
  select(-starts_with("nbatters"))

late_season_next2_1<- rbind(late_season_next2, late2_max1)



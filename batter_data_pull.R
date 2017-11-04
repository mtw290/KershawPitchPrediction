library(stattleshipR)



set_token("")


#grab all batting data for 2015
batters2015 <- ss_get_result(sport = "baseball",
                             league = "mlb",
                             ep = "player_season_stats",
                             query = list(season_id = "mlb-2015"),
                             walk = T)

Batters2015 <- do.call("rbind", lapply(batters2015, function(x) x$player_season_stats))

write.csv(Batters2015, "C://Users/Mike/Documents/kershaw/batters2015.csv")

today = as.character(Sys.Date())
# grab all batting data for 2016
batters2016 <- ss_get_result(sport = "baseball",
                             league = "mlb",
                             ep = "player_season_stats",
                             query = l1,
                             walk = T)

Batters2016 <- do.call("rbind", lapply(batters2016, function(x) x$player_season_stats))

batters2017 <- ss_get_result(sport = "baseball",
                             league = "mlb",
                             ep = "player_season_stats",
                             query = list(season_id = "mlb-2017"),
                             walk = T)
Batters2017 <- do.call("rbind", lapply(batters2017, function(x) x$player_season_stats))

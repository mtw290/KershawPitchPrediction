library(stattleshipR)
library(tidyverse)
library(lubridate)

setwd("C://Users/Mike/Documents/kershaw/")

source("data_grab.R")

kershaw16 <- get_pitches("mlb-clayton-kershaw", 2016)
kershaw17 <- get_pitches("mlb-clayton-kershaw", 2017)

source("savant_data.R")
source("hot_zones.R")

write.csv(pitchDat2, "current_kershaw.csv")

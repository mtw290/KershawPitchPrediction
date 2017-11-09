#####
##### create columns for last pitch and zone of last pitch


pitchDat2$lp_zone = 0
pitchDat2$lp_type = "ND"

pitchDat3 <- pitchDat2[1,]


for (i in 1:length(unique(pitchDat2$date))){
  
  new <- filter(pitchDat2, date == unique(pitchDat2$date)[i])
  
    for (j in 1:length(unique(new$pitch_count))){
      
      if (((unique(new$pitch_count)[j])-1) %in% new$pitch_count){
        
        new$lp_zone[j] <- new$zone[which(new$pitch_count == (unique(new$pitch_count)[j])-1)] 
        new$lp_type[j] <- as.character(new$pitch_type.x[which(new$pitch_count == (unique(new$pitch_count)[j])-1)])
        
      }
    }
  pitchDat3 <- rbind(pitchDat3, new)
}
pitchDat3 <- pitchDat3[-1,]
pitchDat3$lp_type[pitchDat3$lp_type == ""] <- NA



library(dplyr)



kershaw <- read.csv("current_kershaw2.csv")

#standardize function
standardize <- function(x){
  (x - mean(x))/sd(x)
}


# final data clean up
kershaw1 <- kershaw %>% 
  
  #remove records with NA
  na.omit() %>%
  
  #remove one class of each dummy variable 
  select(-balls_0, -strikes_1, -outs_when_up_0, -inning_1,
         -lp_horizontal_umpLeft, -lp_vertical_high, -lp_type_FA, 
         -at_bat_pitch_count_0, -at_bat_pitch_count_1, -X) %>% 
  mutate(next1_avg = next1_avg + .001,
         next2_avg = next2_avg + .001,
         cur_avg = cur_avg + .001) %>% 
  
  #change the number of strikeouts to the rate of strikeouts per at-bat
  mutate(next1_ab = (next1_hits/next1_avg)+1,
         next2_ab = (next2_hits/next2_avg)+1,
         cur_ab = (cur_hits/cur_avg)+1) %>% 
  mutate(next1_stkrt = next1_stkout/next1_ab,
         next2_sktrt = next2_stkout/next2_ab,
         cur_stkrt = cur_stkout/cur_ab) %>%

  #remove strikeout totals
  select(-cur_stkout, -next1_stkout, -next2_stkout)

#standardize data
kershaw2 <- kershaw1 %>% 
  select(-starts_with("target"), -index, -batter) %>% 
  select(which(apply(., 2, function(x) length(unique(x)) > 2))) %>% 
  apply(., 2, function(x) standardize(x))

kershaw3 <- kershaw1 %>% 
  select(-which(apply(., 2, function(x) length(unique(x)) > 2)), starts_with("target")) %>% 
  data.frame(index = kershaw1$index,
             batter = kershaw1$batter,
             kershaw2, .)
             

#select training data
set.seed(13214321)
trainInd <- sample(nrow(kershaw3), .7*nrow(kershaw3))
leftOver <- kershaw3[-trainInd,]
validInd <- sample(nrow(leftOver), .5*nrow(leftOver))

#extract train/valid/test data
trainDat <- kershaw1[trainInd,]
validDat <- leftOver[validInd,]
testDat <- leftOver[-validInd,]

#extract target data
trainTargets <- trainDat %>% 
  select(starts_with("target"), index, batter)
validTargets <- validDat %>% 
  select(starts_with("target"), index, batter)
testTargets <- testDat %>% 
  select(starts_with("target"), index, batter)


#drop target data from data matrix
trainMat <- trainDat %>% 
  select(-starts_with("target"), -batter)
validMat <- validDat %>% 
  select(-starts_with("target"), -batter)
testMat <- testDat %>% 
  select(-starts_with("target"), -batter)


write.csv(trainTargets, "trainTargets.csv")
write.csv(validTargets, "validTargets.csv")
write.csv(testTargets, "testTargets.csv")
write.csv(trainDat, "trainDat.csv")
write.csv(validDat, "validDat.csv")
write.csv(testDat, "testDat.csv")


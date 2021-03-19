#### Make prediction Model
#### Get all match-ups
library(tidyverse)


ncaa <- read.csv("NCAA_cleaned_final.csv")

ncaa64_high <- ncaa %>% filter(Seed <= 8) %>% mutate(gameid = paste(Conference,Seed))
ncaa64_low <- ncaa %>% filter(Seed > 8) %>% mutate(gameid = paste(Conference,17-Seed))

#join on gameid, get upset flag, and if ppm predicts winner
ncaa64 <- ncaa64_high %>% 
  left_join(ncaa64_low,by = c("year","gameid")) %>% 
  mutate(ppmHigh = AdjO.x*AdjD.y*AdjT.x/2, 
         ppmLow = AdjO.y*AdjD.x*AdjT.y/2,
         ppmDiff = ppmHigh - ppmLow,
         x_wins = R32.x
  ) 



# ncaa32
ncaa32_ <- ncaa %>% 
  filter(R32 == 1) %>% 
  mutate(gameid64 = ifelse(Seed <= 8, Seed, 17 - Seed),
         gameid = ifelse(gameid64 <= 4, gameid64, 9 - gameid64),
         gameid = paste(Conference,gameid)) 
ncaa32_high <- ncaa32_ %>% filter(gameid64 <= 4) %>% select(-gameid64)
ncaa32_low <- ncaa32_ %>% filter(gameid64 > 4) %>% select(-gameid64)


#join on gameid, get upset flag, and if ppm predicts winner
ncaa32 <- ncaa32_high %>% 
  left_join(ncaa32_low,by = c("year","gameid")) %>% 
  mutate(ppmHigh = case_when(
    Seed.x > Seed.y ~ AdjO.x*AdjD.y*AdjT.x/2,
    Seed.y > Seed.x ~ AdjO.y*AdjD.x*AdjT.y/2),
    ppmLow = case_when(
      Seed.y > Seed.x ~ AdjO.x*AdjD.y*AdjT.x/2,
      Seed.x > Seed.y ~ AdjO.y*AdjD.x*AdjT.y/2),
    ppmDiff = ppmHigh - ppmLow,
    x_wins = R16.x
  ) #%>% select(-gameid64)

# ncaa16
ncaa16_ <- ncaa %>% 
  filter(R16 == 1) %>% 
  mutate(gameid64 = ifelse(Seed <= 8, Seed, 17 - Seed),
         gameid32 = ifelse(gameid64 <= 4, gameid64, 9 - gameid64),
         gameid = ifelse(gameid32 <= 2, gameid32, 5 - gameid32),
         gameid = paste(Conference,gameid)) 
ncaa16_high <- ncaa16_ %>% filter(gameid32 <= 2) %>% select(-gameid64, -gameid32)
ncaa16_low <- ncaa16_ %>% filter(gameid32 > 2) %>% select(-gameid64, -gameid32)


#join on gameid, get upset flag, and if ppm predicts winner
ncaa16 <- ncaa16_high %>% 
  left_join(ncaa16_low,by = c("year","gameid")) %>% 
  mutate(ppmHigh = case_when(
    Seed.x > Seed.y ~ AdjO.x*AdjD.y*AdjT.x/2,
    Seed.y > Seed.x ~ AdjO.y*AdjD.x*AdjT.y/2),
    ppmLow = case_when(
      Seed.y > Seed.x ~ AdjO.x*AdjD.y*AdjT.x/2,
      Seed.x > Seed.y ~ AdjO.y*AdjD.x*AdjT.y/2),
    ppmDiff = ppmHigh - ppmLow,
    x_wins = R8.x
  ) 

# ncaa8
ncaa8_ <- ncaa %>% 
  filter(R8 == 1) %>% 
  mutate(gameid64 = ifelse(Seed <= 8, Seed, 17 - Seed),
         gameid32 = ifelse(gameid64 <= 4, gameid64, 9 - gameid64),
         gameid16 = ifelse(gameid32 <= 2, gameid32, 5 - gameid32),
         gameid = ifelse(gameid16 <= 1, gameid16, 3 - gameid16),
         gameid = paste(Conference,gameid)) 
ncaa8_high <- ncaa8_ %>% filter(gameid16 <= 1) %>% select(-gameid64, -gameid32, -gameid16)
ncaa8_low <- ncaa8_ %>% filter(gameid16 > 1) %>% select(-gameid64, -gameid32, -gameid16)


#join on gameid, get upset flag, and if ppm predicts winner
ncaa8 <- ncaa8_high %>% 
  left_join(ncaa8_low,by = c("year","gameid")) %>% 
  mutate(ppmHigh = case_when(
    Seed.x > Seed.y ~ AdjO.x*AdjD.y*AdjT.x/2,
    Seed.y > Seed.x ~ AdjO.y*AdjD.x*AdjT.y/2),
    ppmLow = case_when(
      Seed.y > Seed.x ~ AdjO.x*AdjD.y*AdjT.x/2,
      Seed.x > Seed.y ~ AdjO.y*AdjD.x*AdjT.y/2),
    ppmDiff = ppmHigh - ppmLow,
    x_wins = R4.x
  ) 


#ncaa4
ncaa4_A <- ncaa %>% filter(R4 == 1, (year %in% c(2013, 2014, 2015, 2017, 2019) & Conference %in% c("S","W"))|(year %in% c(2016, 2018) & Conference %in% c("S","E")) ) %>% 
  mutate(gameid = case_when(
    year %in% c(2013, 2014, 2015, 2017, 2019) & Conference %in% "W" ~ "N",
    year %in% c(2016, 2018) & Conference %in% "E" ~ "N",
    Conference == "S" ~ "S"
  )
  )  
ncaa4_B <- ncaa %>% filter(R4 == 1, (year %in% c(2013, 2014, 2015, 2017, 2019) & Conference %in% c("E","M"))|(year %in% c(2016, 2018) & Conference %in% c("W","M")) ) %>% 
  mutate(gameid = case_when(
    year %in% c(2013, 2014, 2015, 2017, 2019) & Conference %in% "E" ~ "S",
    year %in% c(2016, 2018) & Conference %in% "W" ~ "S",
    Conference == "M" ~ "N"
  )
  )  

ncaa4 <- ncaa4_A %>% 
  left_join(ncaa4_B,by = c("year","gameid")) %>% 
  mutate(ppmHigh = case_when(
    Seed.x >= Seed.y ~ AdjO.x*AdjD.y*AdjT.x/2,
    Seed.y > Seed.x ~ AdjO.y*AdjD.x*AdjT.y/2),
    ppmLow = case_when(
      Seed.y > Seed.x ~ AdjO.x*AdjD.y*AdjT.x/2,
      Seed.x >= Seed.y ~ AdjO.y*AdjD.x*AdjT.y/2),
    ppmDiff = ppmHigh - ppmLow,
    x_wins = R2.x
  ) 

#ncaa2
ncaa2_W <- ncaa %>% filter(Winner == 1)
ncaa2_L <- ncaa %>% filter(R2 == 1 & Winner == 0)

ncaa2 <- ncaa2_W %>% 
  left_join(ncaa2_L,by = "year") %>% 
  mutate(gameid = "F",
         ppmHigh = case_when(
           Seed.x >= Seed.y ~ AdjO.x*AdjD.y*AdjT.x/2,
           Seed.y > Seed.x ~ AdjO.y*AdjD.x*AdjT.y/2),
         ppmLow = case_when(
           Seed.y > Seed.x ~ AdjO.x*AdjD.y*AdjT.x/2,
           Seed.x >= Seed.y ~ AdjO.y*AdjD.x*AdjT.y/2),
         ppmDiff = ppmHigh - ppmLow,
         x_wins = Winner.x
  ) 

# combine into one match-up file
ncaa_matchups <- ncaa2 %>% full_join(ncaa4) %>% full_join(ncaa8) %>% 
  full_join(ncaa16) %>% full_join(ncaa32) %>% full_join(ncaa64)


#### Make the model
# Use Machine learning in r
# Set up 10-fold cross validation
library(caret)
library(e1071)

# create a list of 80% of the rows in the original dataset we can use for training
dataset <- ncaa_matchups %>% mutate(x_wins = as.factor(x_wins)) %>% 
  select(-Rank.x, -Team.x, -Rank.y, -Team.y, -Conf.x, -Conf.y, -Conference.x, -Conference.y,
         -gameid,-Winner.y, -Winner.x, -year, -R64.x, -R64.y,
         -R32.x, -R32.y, -R16.x, -R16.y, -R8.x, -R8.y, -R4.x, -R4.y, -R2.x, -R2.y,
         -Seed.x, -Seed.y) %>% as.tibble()


control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
set.seed(53818)
fit.lda <- train(x_wins~., data=dataset, method="lda", metric=metric, trControl=control)

set.seed(53818)
fit.cart <- train(x_wins~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(53818)
fit.knn <- train(x_wins~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(53818)
fit.svm <- train(x_wins~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(53818)
fit.rf <- train(x_wins~., data=dataset, method="rf", metric=metric, trControl=control)



#### Get 2021 matchups
ncaa2021 <- read.csv("Scored_64mod.csv") #my file now has the picks by round
ncaa2021 <- ncaa2021 %>% 
  mutate(gameid64 = ifelse(Seed <= 8, Seed, 17 - Seed),
         gameid32 = ifelse(gameid64 <= 4, gameid64, 9 - gameid64),
         gameid16 = ifelse(gameid32 <= 2, gameid32, 5 - gameid32),
         gameid8 = ifelse(gameid16 <= 1, gameid16, 3 - gameid16),
         gameid4 = ifelse(Conference %in% c("W", "E"),"WvE","SvM"),
         gameid2 = "Championship")

#### function to get predictions based on match-ups
headtohead <- function(x = "Gonzaga", y = "Michigan", round = 64){
  teamx <- ncaa2021 %>% filter(Team %in% x)
  teamy <- ncaa2021 %>% filter(Team %in% y)
  named <- case_when(
    round == 64 ~ c("year", "gameid64", "Conference"),
    round == 32 ~ c("year", "gameid32", "Conference"),
    round == 16 ~ c("year", "gameid16", "Conference"),
    round == 8 ~ c("year", "gameid8", "Conference"),
    round == 4 ~ c("year", "R4", "gameid4"),
    round == 2 ~ c("year", "R2", "gameid2")
  )
  
  both <- teamx %>% left_join(teamy, by = named)%>% 
    mutate(ppmHigh = case_when(
      Seed.x >= Seed.y ~ AdjO.x*AdjD.y*AdjT.x/2,
      Seed.y > Seed.x ~ AdjO.y*AdjD.x*AdjT.y/2),
      ppmLow = case_when(
        Seed.y > Seed.x ~ AdjO.x*AdjD.y*AdjT.x/2,
        Seed.x >= Seed.y ~ AdjO.y*AdjD.x*AdjT.y/2),
      ppmDiff = ppmHigh - ppmLow,
      matchup = paste(Team.x,Seed.x,"vs", Team.y, Seed.y),
      advance.sam = ifelse(prob_win.x - prob_win.y > 0, 1, 0)
    ) 
  
  
  rf <- predict(fit.rf, both, type = "prob")[,2]
  lda <- predict(fit.lda, both, type = "prob")[,2]#ifelse(predict(fit.lda, both, type = "prob")[,2] > 0.5, 1, 0)
  #cart <- predict(fit.cart, both, type = "prob")[,2]#ifelse(predict(fit.cart, both, type = "prob")[,2] > 0.5, 1, 0)
  knn <- predict(fit.knn, both, type = "prob")[,2]#ifelse(predict(fit.knn, both, type = "prob")[,2] > 0.5, 1, 0)
  #svm <- predict(fit.svm, both, type = "prob")[,2]#ifelse(predict(fit.svm, both, type = "prob")[,2] > 0.5, 1, 0)
  sam <- both$prob_win.x/(both$prob_win.x+both$prob_win.y)#ifelse(both$prob_win.x > both$prob_win.y, 1, 0)
  cbind(x, y, lda, rf, knn, sam)
}

#### The Match-ups themselves with some pictures


R64_high <- ncaa2021 %>% filter(Seed <= 8) %>% mutate(gameid = paste(Conference,gameid64))
R64_low <- ncaa2021 %>% filter(Seed > 8) %>% mutate(gameid = paste(Conference,gameid64))

R64_matches <- R64_high %>% left_join(R64_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R64_results <- as.tibble(headtohead(x = R64_matches$Team.x, y = R64_matches$Team.y, round = 64)) %>% 
  mutate(game = paste(x,"vs",y)) %>% 
  pivot_longer(cols = c("lda","rf","knn","sam"),names_to = "modelused", values_to = "probxwins")
R64_results %>% ggplot(aes(x=game, y = as.numeric(probxwins), color = modelused)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#### Get round 32 match-ups based on Round 64 picks
R32_high <- ncaa2021 %>% filter(gameid64 <= 4, R32 == 1) %>% mutate(gameid = paste(Conference,gameid32))
R32_low <- ncaa2021 %>% filter(gameid64 > 4, R32 == 1) %>% mutate(gameid = paste(Conference,gameid32))

R32_matches <- R32_high %>% left_join(R32_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R32_results <- as.tibble(headtohead(x = R32_matches$Team.x, y = R32_matches$Team.y, round = 32)) %>% 
  mutate(game = paste(x,"vs",y)) %>% 
  pivot_longer(cols = c("lda","rf","knn","sam"),names_to = "modelused", values_to = "probxwins")
R32_results %>% ggplot(aes(x=game, y = as.numeric(probxwins), color = modelused)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#### Get round of 16 based on round of 32 picks
R16_high <- ncaa2021 %>% filter(gameid32 <= 2, R16 == 1) %>% mutate(gameid = paste(Conference,gameid16))
R16_low <- ncaa2021 %>% filter(gameid32 > 2, R16 == 1) %>% mutate(gameid = paste(Conference,gameid16))

R16_matches <- R16_high %>% left_join(R16_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R16_results <- as.tibble(headtohead(x = R16_matches$Team.x, y = R16_matches$Team.y, round = 16)) %>% 
  mutate(game = paste(x,"vs",y)) %>% 
  pivot_longer(cols = c("lda","rf","knn","sam"),names_to = "modelused", values_to = "probxwins")
R16_results %>% ggplot(aes(x=game, y = as.numeric(probxwins), color = modelused)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### Get round of 8 based on round of 16 picks
R8_high <- ncaa2021 %>% filter(gameid16 <= 1, R8 == 1) %>% mutate(gameid = paste(Conference,gameid8))
R8_low <- ncaa2021 %>% filter(gameid16 > 1, R8 == 1) %>% mutate(gameid = paste(Conference,gameid8))

R8_matches <- R8_high %>% left_join(R8_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R8_results <- as.tibble(headtohead(x = R8_matches$Team.x, y = R8_matches$Team.y, round = 8)) %>% 
  mutate(game = paste(x,"vs",y)) %>% 
  pivot_longer(cols = c("lda","rf","knn","sam"),names_to = "modelused", values_to = "probxwins")
R8_results %>% ggplot(aes(x=game, y = as.numeric(probxwins), color = modelused)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### Get round of 4 based on round of 8 picks
R4_high <- ncaa2021 %>% filter(Conference %in% c("W", "S"), R4 == 1) %>% mutate(gameid = gameid4)
R4_low <- ncaa2021 %>% filter(Conference %in% c("E", "M"), R4 == 1) %>% mutate(gameid = gameid4)

R4_matches <- R4_high %>% left_join(R4_low, by = c("year", "R4", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R4_results <- as.tibble(headtohead(x = R4_matches$Team.x, y = R4_matches$Team.y, round = 4)) %>% 
  mutate(game = paste(x,"vs",y)) %>% 
  pivot_longer(cols = c("lda","rf","knn","sam"),names_to = "modelused", values_to = "probxwins")
R4_results %>% ggplot(aes(x=game, y = as.numeric(probxwins), color = modelused)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#### Get round of 2

headtohead2(x = "Gonzaga", y = "Illinois", round = 2)


#### Automatically choose winners by round using lda score
R64_update <- as.tibble(headtohead(x = R64_matches$Team.x, y = R64_matches$Team.y, round = 64))
ncaa64_win <- ncaa2021 %>% 
  left_join(R64_update, by = c("Team" = "x")) %>% mutate(R32 = ifelse(as.numeric(lda) > 0.5, 1,0)) %>% 
  left_join(R64_update, by = c("Team" = "y")) %>% mutate(R32 = case_when(
    as.numeric(lda.y) <= 0.5 ~ 1,
    as.numeric(lda.x) > 0.5 ~ 1, 
    TRUE ~ 0)) %>% select(-lda.x, -lda.y, -x, -y, -rf.x, -rf.y, -knn.x, -knn.y, -sam.x, -sam.y)


R32_high <- ncaa64_win %>% filter(gameid64 <= 4, R32 == 1) %>% mutate(gameid = paste(Conference,gameid32))
R32_low <- ncaa64_win %>% filter(gameid64 > 4, R32 == 1) %>% mutate(gameid = paste(Conference,gameid32))

R32_matches <- R32_high %>% left_join(R32_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R32_update <- as.tibble(headtohead(x = R32_matches$Team.x, y = R32_matches$Team.y, round = 32))
ncaa32_win <- ncaa64_win %>% 
  left_join(R32_update, by = c("Team" = "x")) %>% mutate(R16 = ifelse(as.numeric(lda) > 0.5, 1,0)) %>% 
  left_join(R32_update, by = c("Team" = "y")) %>% mutate(R16 = case_when(
    as.numeric(lda.y) <= 0.5 ~ 1,
    as.numeric(lda.x) > 0.5 ~ 1, 
    TRUE ~ 0)) %>% select(-lda.x, -lda.y, -x, -y, -rf.x, -rf.y, -knn.x, -knn.y, -sam.x, -sam.y)


R16_high <- ncaa32_win %>% filter(gameid32 <= 2, R16 == 1) %>% mutate(gameid = paste(Conference,gameid16))
R16_low <- ncaa32_win %>% filter(gameid32 > 2, R16 == 1) %>% mutate(gameid = paste(Conference,gameid16))

R16_matches <- R16_high %>% left_join(R16_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R16_update <- as.tibble(headtohead(x = R16_matches$Team.x, y = R16_matches$Team.y, round = 16))
ncaa16_win <- ncaa32_win %>% 
  left_join(R16_update, by = c("Team" = "x")) %>% mutate(R8 = ifelse(as.numeric(lda) > 0.5, 1,0)) %>% 
  left_join(R16_update, by = c("Team" = "y")) %>% mutate(R8 = case_when(
    as.numeric(lda.y) <= 0.5 ~ 1,
    as.numeric(lda.x) > 0.5 ~ 1, 
    TRUE ~ 0))%>% select(-lda.x, -lda.y, -x, -y, -rf.x, -rf.y, -knn.x, -knn.y, -sam.x, -sam.y)


R8_high <- ncaa16_win %>% filter(gameid16 <= 1, R8 == 1) %>% mutate(gameid = paste(Conference,gameid8))
R8_low <- ncaa16_win %>% filter(gameid16 > 1, R8 == 1) %>% mutate(gameid = paste(Conference,gameid8))

R8_matches <- R8_high %>% left_join(R8_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R8_update <- as.tibble(headtohead(x = R8_matches$Team.x, y = R8_matches$Team.y, round = 8))
ncaa8_win <- ncaa16_win %>% 
  left_join(R8_update, by = c("Team" = "x")) %>% mutate(R4 = ifelse(as.numeric(lda) > 0.5, 1,0)) %>% 
  left_join(R8_update, by = c("Team" = "y")) %>% mutate(R4 = case_when(
    as.numeric(lda.y) <= 0.5 ~ 1,
    as.numeric(lda.x) > 0.5 ~ 1, 
    TRUE ~ 0)) %>% select(-lda.x, -lda.y, -x, -y, -rf.x, -rf.y, -knn.x, -knn.y, -sam.x, -sam.y)

R4_high <- ncaa8_win %>% filter(Conference %in% c("W", "S"), R4 == 1) %>% mutate(gameid = gameid4)
R4_low <- ncaa8_win %>% filter(Conference %in% c("E", "M"), R4 == 1) %>% mutate(gameid = gameid4)

R4_matches <- R4_high %>% left_join(R4_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R4_update <- as.tibble(headtohead(x = R4_matches$Team.x, y = R4_matches$Team.y, round = 4))
ncaa4_win <- ncaa8_win %>% 
  left_join(R4_update, by = c("Team" = "x")) %>% mutate(R2 = ifelse(as.numeric(lda) > 0.5, 1,0)) %>% 
  left_join(R4_update, by = c("Team" = "y")) %>% mutate(R2 = case_when(
    as.numeric(lda.y) <= 0.5 ~ 1,
    as.numeric(lda.x) > 0.5 ~ 1, 
    TRUE ~ 0))%>% select(-lda.x, -lda.y, -x, -y, -rf.x, -rf.y, -knn.x, -knn.y, -sam.x, -sam.y)
write.csv(ncaa4_win,"ncaa_blacks_brack.csv", row.names = F)

R2_high <- ncaa4_win %>% filter(Conference %in% c("W", "E"), R2 == 1) %>% mutate(gameid = gameid2)
R2_low <- ncaa4_win %>% filter(Conference %in% c("S", "M"), R2 == 1) %>% mutate(gameid = gameid2)

R2_matches <- R2_high %>% left_join(R2_low, by = c("year", "gameid")) %>% 
  select(Team.x, Team.y, gameid)

R2_update <- as.tibble(headtohead(x = R2_matches$Team.x, y = R2_matches$Team.y, round = 2))
ncaa2_win <- ncaa4_win %>% 
  left_join(R2_update, by = c("Team" = "x")) %>% mutate(Winner = ifelse(as.numeric(lda) > 0.5, 1,0)) %>% 
  left_join(R2_update, by = c("Team" = "y")) %>% mutate(Winner = case_when(
    as.numeric(lda.y) <= 0.5 ~ 1,
    as.numeric(lda.x) > 0.5 ~ 1, 
    TRUE ~ 0))%>% select(-lda.x, -lda.y, -x, -y, -rf.x, -rf.y, -knn.x, -knn.y, -sam.x, -sam.y)


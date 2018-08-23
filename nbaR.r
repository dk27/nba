library(data.table)
library(caret)
library(h2o)
library(rpart)
library(glmnet)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("C:/Users/daria/Desktop/nba_shots")
data<-read.csv(file="nba_shots.csv")
table(is.na(data$PTS_TYPE))
table(is.na(data$SHOT_DIST))
table(data$PTS_TYPE, data$SHOT_RESULT)

### could have two models:
### -- a multinomial model that takes 4 values for a target
### -- one model for 2 points and one model for 3 points

## data cleaning
data$PERIOD<-as.factor(data$PERIOD)
data$PTS_TYPE<-as.factor(data$PTS_TYPE)
data$FGM<-as.factor(data$FGM)

# convert game clock to seconds
gameClock<-strptime(data$GAME_CLOCK, '%M:%S')
data$GAME_CLOCK_SEC<-gameClock$min*60 +gameClock$sec
## are there any missing values?

## EDA
data %>%
  group_by(SHOT_RESULT) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=SHOT_RESULT, y=count))+geom_bar(fill="royalblue",
                                               stat="identity")+
  xlab("Shot Result") + ylab("Count")

ggplot(data=data, aes(x=PTS_TYPE, fill=SHOT_RESULT))+
  geom_bar(stat="count")

ggplot(data=data, aes(x=PTS_TYPE, fill=LOCATION))+
  geom_bar(stat="count")

## distribution of SHOT Distance for each made/missed 2& 3 points
## extract date
## extract cities
data$player_id<-as.factor(data$player_id)
table(data$player_id)
data %>%
  group_by (player_id) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

data %>%
  filter(player_id %in% c('201935', '101145', '200746', '203081', '2544') & PTS_TYPE == '2') %>%
  count(player_id, SHOT_RESULT) %>%
  spread(SHOT_RESULT, n) 



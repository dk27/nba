#library(data.table)
library(caret)
library(h2o)
library(rpart)
#library(glmnet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(party)
library(partykit)
library(ROCR)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
#(nnet)

setwd("C:/Users/daria/Desktop/nba_shots")
data<-read.csv(file="nba.csv")
table(is.na(data$pts_type))
table(is.na(data$shot_dist))
#table(data$PTS_TYPE, data$SHOT_RESULT)

### could have two models:
### -- a multinomial model that takes 4 values for a target
### -- one model for 2 points and one model for 3 points

## data cleaning
data$period<-as.factor(data$period)
data$pts_type<-as.factor(data$pts_type)
data$player_id<-as.factor(data$player_id)
table(data$player_id)

# convert game clock to seconds
gameClock<-strptime(data$game_clock, '%M:%S')
data$GAME_CLOCK_SEC<-gameClock$min*60 +gameClock$sec

df <-data %>%
  #filter(player_id %in% c('201935', '101145', '200746', '203081', '2544') & PTS_TYPE == '2') %>%
  count(player_id, shot_result) %>%
  spread(shot_result, n) 

df$total <- df$made + df$missed
df$totalPcn<-df$total/sum(df$total)
#summary(df$totalPcn)
#mean<-sum(df$totalPcn)/count(df)
df$playerIDGroup<-as.factor(ifelse(df$totalPcn>=0.006, as.character(df$player_id), "other"))
#sum(df$totalPcn)
#table(df$playerIDGroup)
#df$pcnt_made<-df$made/df$total
#df$pcnt_miss<-df$missed/df$total
#order.made<-order(df$pcnt_made)
#df<-df[order.made,]

data$player_id<-as.factor(data$player_id) ## 281 levels
#join data with df on played id, take playerIDgroup from df
data<-merge(data, subset(df, select=c(player_id, playerIDGroup)), by = 'player_id')
data$date<-substr(data$matchup, 1,12)
data$teams<-trimws(sub('.*-', '', data$matchup))

data$team1<-substr(data$teams, 1,4)
data$c1<-regexpr('@', data$teams)
data$c2<-regexpr('vs.', data$teams)

data$team2<-ifelse(data$c1<0, sub('.*vs.', '', data$teams), sub('.*@', '', data$teams))

data$PriorGamesExperience<-ifelse(data$priorFreq<0.4 | data$priorFreq >1000, "low",
                           ifelse(data$priorFreq < 0.5, "average", "high"))
data$priorFreq1<-ifelse(data$priorFreq>100, 0.4521469, data$priorFreq)

#priorGames<-
#  data %>%
#  count(PriorGamesExperience, shot_result) %>%
#  spread(shot_result, n) 
#priorGames$pcntMade<-priorGames$made/(priorGames$made+priorGames$missed)

#library(printr)
#knitr::kable(priorGames, digits = 2, caption = "A table produced by printr.")
#priorGames
################################################ EDA

data$gameClockGp<-cut(data$GAME_CLOCK_SEC, 10)

gameClock<- 
  data %>%
  count(gameClockGp, shot_result, pts_type) %>%
  spread(shot_result,  n)

gameClock2<-subset(gameClock, pts_type=='2')
gameClock2$pcnt_made2<-gameClock2$made/(gameClock2$made+gameClock2$missed)
gameClock3<-subset(gameClock, pts_type=='3')
gameClock3$pcnt_made3<-gameClock3$made/(gameClock3$made+gameClock3$missed)
gameClockFinal<-cbind(gameClock2, gameClock3)
gameClockFinal<-gameClockFinal[,c(1,5,10)]

gameClock$pcnt_made<-gameClock$made/(gameClock$made+gameClock$missed)

ggplot(gameClockFinal, aes(as.numeric(gameClockGp)))+geom_line(aes(y=pcnt_made2), colour='lightcoral')+
  geom_line(aes(y=pcnt_made3), colour='mediumturquoise')+xlab("Game Clock Group")+
  ylab("Precent Made")+scale_x_continuous(breaks=1:10)+theme_bw()
                                                    
ggplot(gameClockFinal, aes(gameClockGp,pcnt_made2, pcnt_made3)) + 
  geom_line() + 
  geom_line()

# overall distribution
data %>%
  group_by(SHOT_RESULT) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=SHOT_RESULT, y=count))+geom_bar(fill="royalblue",
                                               stat="identity")+
  xlab("Shot Result") + ylab("Count")

p<-ggplot(data=data, aes(x=PTS_TYPE, fill=SHOT_RESULT))+
  geom_bar(stat="count") + xlab("Points Type")+theme_bw()
p + scale_fill_discrete(name="Shot Result")

#shots made by location
ggplot(data=data, aes(x=PTS_TYPE, fill=LOCATION))+
  geom_bar(stat="count") + xlab("Points Type") + theme_bw()+
  ggtitle("Missed/Made Shots \nby Location")

#Shot Distance
data %>% 
  #filter(PTS_TYPE=='2') %>%
  ggplot(aes(x=SHOT_DIST,fill=SHOT_RESULT)) +
  geom_density(stat='bin',alpha = .5) +
  coord_cartesian(xlim=c(0,30))+
  theme_bw()+xlab("Shot Distance")+scale_fill_discrete(name="Shot Result")+
  facet_wrap(~PTS_TYPE)

data %>% 
  #filter(PTS_TYPE=='2') %>%
  ggplot(aes(x=CLOSE_DEF_DIST,fill=SHOT_RESULT)) +
  geom_density(stat='bin',alpha = .5) +
  coord_cartesian(xlim=c(0,30))+
  theme_bw()+xlab("Shot Distance")+scale_fill_discrete(name="Shot Result")+
  facet_wrap(~PTS_TYPE)

## player
#prior games % made
df1 <-data %>%
  #filter(player_id %in% c('201935', '101145', '200746', '203081', '2544') & PTS_TYPE == '2') %>%
  count(PriorGamesExperience, shot_result) %>%
  spread(shot_result, n) 

df1$pcnt_made <-df1$made/(df1$made+df1$missed)

df1$PriorGamesExperience <- factor(df1$PriorGamesExperience, levels = df1$PriorGamesExperience[order(df1$pcnt_made)])

df1 %>%
  ggplot(aes(x=PriorGamesExperience, y=pcnt_made))+geom_bar(fill="mediumturquoise",
                                                            stat="identity")+
  ylab("% Made")+theme_bw()

############################ player id distribution
# create grouped dataset for 2 points
df_2pts <- data %>%
  filter( pts_type == '2') %>%
  count(player_id,  shot_result) %>%
  spread(shot_result, n) 
df_2pts$total<-df_2pts$made+df_2pts$missed
df_2pts$pcnt_made <- df_2pts$made/df_2pts$total



# create grouped dataset for 3 points
df_3pts <- data %>%
  filter( pts_type == '3') %>%
  count(player_id,  shot_result) %>%
  spread(shot_result, n) 
df_3pts$total<-df_3pts$made+df_3pts$missed
df_3pts$pcnt_made <- df_3pts$made/df_3pts$total

df_2pts %>%
  mutate(pcnt1 = pcnt_made-0.5) %>%
  mutate(PercentMade = ifelse(pcnt_made < 0.5, 'Below Average' , 'Above Average')) %>%
  arrange(desc(pcnt1)) %>%
  filter(total>650) %>%
  ggplot(aes( x = factor(player_id, levels=player_id), y = pcnt1)) + 
  geom_bar(stat = 'identity', aes(fill=PercentMade), width = .6) +  
  coord_flip() + 
  labs(x = 'Player ID', y = '% Made', title = 'Player % Made 2 Points') + 
  theme_bw()

df_3pts %>%
  mutate(pcnt1 = pcnt_made-0.35) %>%
  mutate(PercentMade = ifelse(pcnt_made < 0.35, 'Below Average' , 'Above Average')) %>%
  arrange(desc(pcnt1)) %>%
  filter(total>290) %>%
  ggplot(aes( x = factor(player_id, levels=player_id), y = pcnt1)) + 
  geom_bar(stat = 'identity', aes(fill=PercentMade), width = .6) +  
  coord_flip() + 
  labs(x = 'Player ID', y = '% Made', title = 'Player % Made 3 Points') + 
  theme_bw()


####### OVERALL MADE
df %>%
  mutate(pcnt1 = pcnt_made-0.45) %>%
  mutate(PercentMade = ifelse(pcnt_made < 0.45, 'Below Average' , 'Above Average')) %>%
  arrange(desc(pcnt1)) %>%
  filter(playerIDGroup != 'other') %>%
  ggplot(aes( x = factor(player_id, levels=player_id), y = pcnt1)) + 
  geom_bar(stat = 'identity', aes(fill=PercentMade), width = .6) +  
  coord_flip() + 
  labs(x = 'Player ID', y = '% Made', title = 'Player % Made') + 
  theme_bw()

### Defender ID
defend <-data %>%
  #filter(player_id %in% c('201935', '101145', '200746', '203081', '2544') & PTS_TYPE == '2') %>%
  count(closest_defender_player_id, shot_result) %>%
  spread(shot_result, n) 

defend$total <- defend$made + defend$missed
defend$pcnt_made<-defend$made/defend$total

defend %>%
  mutate(pcnt1 = pcnt_made-0.45) %>%
  mutate(PercentMade = ifelse(pcnt_made < 0.45, 'Below Average' , 'Above Average')) %>%
  arrange(desc(pcnt1)) %>%
  filter(total > 600) %>%
  ggplot(aes( x = factor(closest_defender_player_id, levels=closest_defender_player_id), y = pcnt1)) + 
  geom_bar(stat = 'identity', aes(fill=PercentMade), width = .6) +  
  coord_flip() + 
  labs(x = 'Defender ID', y = '% Made') + 
  theme_bw()

## distribution of SHOT Distance for each made/missed 2& 3 points
## extract date
## extract cities

data %>%
  group_by (player_id) %>%
  summarise(count=n()) %>%
  arrange(desc(count))


data %>%
  group_by(player_id,PTS_TYPE) %>%
  summarize(total = n(), 
            mean = mean(as.numeric(FGM)-1)) %>%
  filter(total > 10) %>%
  ggplot(aes( x = mean)) + 
  geom_density(fill = 'green') + 
  facet_wrap(~PTS_TYPE) + 
  theme_bw()




table(data$playerIDGroup)
table(data$SHOT_NUMBER) #include as it is
table(data$PERIOD) #include as it is
summary(data$GAME_CLOCK_SEC) #include as it is
summary(data$SHOT_DIST) #include as it is
###################################### Modeling
### start with a Decision Tree
#create target

data$target<-as.factor(ifelse(data$shot_result == 'made' & data$pts_type == '2', '1',
             ifelse(data$shot_result == 'made' & data$pts_type == '3', '2',
             ifelse(data$shot_result == 'missed' & data$pts_type == '2', '3', '4'))))
table(data$target)
data$PriorGamesExperience<-as.factor(data$PriorGamesExperience)
data$team1<-as.factor(data$team1)

data$target1<-as.factor(ifelse(data$shot_result == 'made', '1', '0'))
data$period1<-as.factor(ifelse(data$period %in% c('1', '2', '3', '4'), as.character(data$period), 'OT'))
data_2<-subset(data, pts_type == '2')
data_3<-subset(data, pts_type == '3')

############################## build decision tree for 2 points
trainingRows<-createDataPartition(data_2$target1, p=0.7, list = FALSE)

train<-data_2[trainingRows,]
test<-data_2[-trainingRows,]
fit <- ctree(target1~location+shot_number+period+shot_dist+close_def_dist+GAME_CLOCK_SEC+team1+
               dribbles1+touch_time+PriorGamesExperience, 
             data=train, control=ctree_control(minbucket=4500, maxdepth=4))
plot(fit, type="simple")
pred<-predict(fit, newdata=test)
confusionMatrix(test$target1, pred) #not great

predProb <- predict(fit, newdata=test,type="prob")
# plot ROC
roc_pred <- prediction(predProb[,2], test$target1)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, col="red")
abline(0,1,col="grey")

rfit<-rpart(target1~location+shot_number+period+shot_dist+close_def_dist+GAME_CLOCK_SEC+team1+
             +dribbles+touch_time+PriorGamesExperience, data=train, 
             control=rpart.control(cp = 0.001, minbucket =1500))
plot(rfit)
text(rfit)
fancyRpartPlot(rfit)

prp(rfit, type=4, extra=106, tweak=0.9, space=0, varlen=0, faclen=0,
    box.col=c("palegreen3", "skyblue1")[rfit$frame$yval])
print(rfit)
pred<-predict(rfit, newdata=test)
y<-pred[,2]
test<-cbind(test,y)
test$yTarget<-ifelse(test$y>=0.5, '1', '0')

confusionMatrix(test$target1, test$yTarget) #not great

predProb <- predict(fit, newdata=test,type="prob")
# plot ROC
roc_pred <- prediction(pred[,2], test$target1)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, col="red")
abline(0,1,col="grey")

# get area under the curve
performance(roc_pred,"auc")@y.values
print(rfit)

############################# build decision tree for 3 points
trainingRows1<-createDataPartition(data_3$target1, p=0.7, list = FALSE)

train1<-data_3[trainingRows1,]
test1<-data_3[-trainingRows1,]

rfit1<-rpart(target1~LOCATION+SHOT_NUMBER+PERIOD+SHOT_DIST+CLOSE_DEF_DIST+GAME_CLOCK_SEC+team1+
                +DRIBBLES+TOUCH_TIME+playerIDGroup.x, 
              data=train1, control=rpart.control(cp=0.001+minbucket=500))
plot(rfit1)
text(rfit1)
print(rfit1)
fit1 <- ctree(target1~LOCATION+SHOT_NUMBER+PERIOD+SHOT_DIST+CLOSE_DEF_DIST+GAME_CLOCK_SEC+as.factor(team1)+
               as.factor(team2)+DRIBBLES+TOUCH_TIME, 
             data=train1, control=ctree_control(minbucket=500, maxdepth=6))
plot(fit1, type="simple")
pred1<-predict(fit1, newdata=test1)
predProb1 <- predict(fit1, newdata=test1,type="prob")
y<-predProb1[,2]
y1<-data.frame(y)
y1$target<-ifelse(y1$y>=0.4, '1', '0')
confusionMatrix(test1$target1, y1$target) #bad


# plot ROC
roc_pred <- prediction(y1$y, test1$target1)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, col="red")
abline(0,1,col="grey")

# get area under the curve
performance(roc_pred,"auc")@y.values ## bad
#split the data into train & test

rm(data)
##################################### H2O
## try random forest & GBM

h2o.init(nthreads=-1)
df_2<-as.h2o(data_2)
splits <- h2o.splitFrame(df_2,c(0.7),seed=1234)  

train <- h2o.assign(splits[[1]], "train.hex")   
test <- h2o.assign(splits[[2]], "test.hex") 

x<-c("player_id", "location", "shot_number", "period1", "shot_clock", "dribbles", "shot_dist", "touch_time", 
     "closest_defender_player_id", "close_def_dist", "GAME_CLOCK_SEC", "date", "priorFreq1")
y<-"target1"

rf <- h2o.randomForest(training_frame = train, 
                        validation_frame = test,
                        y=y,
                        x=x,
                        ntrees=200,
                        seed=1234)     

model_path <- h2o.saveModel(object=rf, path=getwd(), force=TRUE)
print(model_path)


rf1<-h2o.randomForest(y=y,
                     x=x,
                     training_frame = train,
                     validation_frame = test,
                     nfolds=5,
                     ntrees=50,
                     seed=1234)
model_path <- h2o.saveModel(object=rf1, path=getwd(), force=TRUE)

print(model_path)


### GBM Overfits
gbm1 <- h2o.gbm(
  training_frame = train,     ##
  validation_frame = test,   ##
  x=x,                     ##
  y=y,                       ## 
  ntrees = 200,           ## increase the learning rate even further
  max_depth = 10,             ## 
  sample_rate = 0.7,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
  stopping_rounds = 2,        ## 
  stopping_tolerance = 0.01,  ##
  ##
  seed = 2000000)    

#### 3 points
df_3<-as.h2o(data_3)
splits <- h2o.splitFrame(df_3,c(0.7),seed=1234)  

train <- h2o.assign(splits[[1]], "train.hex")   
test <- h2o.assign(splits[[2]], "test.hex") 

x<-c("player_id", "location", "shot_number", "period1", "shot_clock", "dribbles", "shot_dist", "touch_time", 
     "closest_defender_player_id", "close_def_dist", "GAME_CLOCK_SEC", "date", "priorFreq1")
y<-"target1"

rf_3 <- h2o.randomForest(training_frame = train, 
                       validation_frame = test,
                       y=y,
                       x=x,
                       ntrees=200,
                       seed=1234)     

model_path <- h2o.saveModel(object=rf_3, path=getwd(), force=TRUE)

print(model_path)


######################### multinomial model
h2o.init(nthreads = -1)
df<-as.h2o(data)
splits <- h2o.splitFrame(df,c(0.7),seed=1234)  

train <- h2o.assign(splits[[1]], "train.hex")   
test <- h2o.assign(splits[[2]], "test.hex") 

x<-c("player_id", "location", "shot_number", "period1", "shot_clock", "dribbles", "shot_dist", "touch_time", 
     "closest_defender_player_id", "close_def_dist", "GAME_CLOCK_SEC", "date", "priorFreq1")
y<-"target"



rfall <- h2o.randomForest(training_frame = train, 
                         validation_frame = test,
                         y=y,
                         x=x,
                         ntrees=200,
                         seed=1234)     

model_path <- h2o.saveModel(object=rfall, path=getwd(), force=TRUE)

print(model_path)



rfall <- h2o.randomForest(training_frame = train, 
                          validation_frame = test,
                          y=y,
                          x=x,
                          ntrees=200,
                          seed=1234)    



gbmall = h2o.gbm(x = x,
               y = y,
               training_frame = train, 
               validation_frame = test, 
               ntrees=100,
               seed = 1234)
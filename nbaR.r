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
#(nnet)

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
data$player_id<-as.factor(data$player_id)
table(data$player_id)

# convert game clock to seconds
gameClock<-strptime(data$GAME_CLOCK, '%M:%S')
data$GAME_CLOCK_SEC<-gameClock$min*60 +gameClock$sec

df <-data %>%
  #filter(player_id %in% c('201935', '101145', '200746', '203081', '2544') & PTS_TYPE == '2') %>%
  count(player_id, SHOT_RESULT) %>%
  spread(SHOT_RESULT, n) 

df$total <- df$made + df$missed
df$totalPcn<-df$total/sum(df$total)
#summary(df$totalPcn)
#mean<-sum(df$totalPcn)/count(df)
df$playerIDGroup<-as.factor(ifelse(df$totalPcn>=0.0065, as.character(df$player_id), "other"))
#sum(df$totalPcn)
#table(df$playerIDGroup)
#df$pcnt_made<-df$made/df$total
#df$pcnt_miss<-df$missed/df$total
#order.made<-order(df$pcnt_made)
#df<-df[order.made,]

data$player_id<-as.factor(data$player_id) ## 281 levels
#join data with df on played id, take playerIDgroup from df
data<-merge(data, subset(df, select=c(player_id, playerIDGroup)), by = 'player_id')
data$date<-substr(data$MATCHUP, 1,12)
data$teams<-trimws(sub('.*-', '', data$MATCHUP))

data$team1<-substr(data$teams, 1,4)
data$c1<-regexpr('@', data$teams)
data$c2<-regexpr('vs.', data$teams)

data$team2<-ifelse(data$c1<0, sub('.*vs.', '', data$teams), sub('.*@', '', data$teams))

## EDA
data %>%
  group_by(SHOT_RESULT) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=SHOT_RESULT, y=count))+geom_bar(fill="royalblue",
                                               stat="identity")+
  xlab("Shot Result") + ylab("Count")

p<-ggplot(data=data, aes(x=PTS_TYPE, fill=SHOT_RESULT))+
  geom_bar(stat="count") + xlab("Points Type")+theme_bw()
p + scale_fill_discrete(name="Shot Result")

ggplot(data=data, aes(x=PTS_TYPE, fill=LOCATION))+
  geom_bar(stat="count") + xlab("Points Type") + theme_bw()+
  ggtitle("Missed/Made Shots \nby Location")

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

### start with a Decision Tree
#create target

data$target<-ifelse(data$SHOT_RESULT == 'made' & data$PTS_TYPE == '2', '2Y',
             ifelse(data$SHOT_RESULT == 'made' & data$PTS_TYPE == '3', '3Y',
             ifelse(data$SHOT_RESULT == 'missed' & data$PTS_TYPE == '2', '2N', '3N')))
table(data$target)

data$target1<-as.factor(ifelse(data$SHOT_RESULT == 'made', '1', '0'))
data_2<-subset(data, PTS_TYPE == '2')
data_3<-subset(data, PTS_TYPE == '3')

############################## build decision tree for 2 points
trainingRows<-createDataPartition(data_2$target1, p=0.7, list = FALSE)

train<-data_2[trainingRows,]
test<-data_2[-trainingRows,]
fit <- ctree(target1~LOCATION+SHOT_NUMBER+PERIOD+SHOT_DIST+CLOSE_DEF_DIST+GAME_CLOCK_SEC+as.factor(team1)+
               as.factor(team2)+DRIBBLES+TOUCH_TIME, 
             data=train, control=ctree_control(minbucket=3000, maxdepth=3))
plot(fit, type="simple")
pred<-predict(fit, newdata=test)
confusionMatrix(test$target1, pred) #not great

predProb <- predict(fit, newdata=test,type="prob")
# plot ROC
roc_pred <- prediction(predProb[,2], test$target1)
perf <- performance(roc_pred, "tpr", "fpr")
plot(perf, col="red")
abline(0,1,col="grey")

rfit<-rpart(target1~LOCATION+SHOT_NUMBER+PERIOD+SHOT_DIST+CLOSE_DEF_DIST+GAME_CLOCK_SEC+team1+
             +DRIBBLES+TOUCH_TIME+playerIDGroup.y, data=train, control=rpart.control(cp = 0.001, minbucket =1500))
plot(rfit)
text(rfit)
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

## try random forest & GBM

h2o.init(nthreads=-1)
df_2<-as.h2o(data_2)
splits <- h2o.splitFrame(df_2,c(0.6,0.2),seed=1234)  

train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex")  
test <- h2o.assign(splits[[3]], "test.hex") 

x<-c("player_id", "LOCATION", "SHOT_NUMBER", "PERIOD", "SHOT_CLOCK", "DRIBBLES", "SHOT_DIST", "TOUCH_TIME", 
     "CLOSEST_DEFENDER_PLAYER_ID", "CLOSE_DEF_DIST", "GAME_CLOCK_SEC", "date", "team1", "team2")
y<-"target1"

rf1 <- h2o.randomForest(training_frame = train, 
                        validation_frame = valid,
                        y=y,
                        x=x,
                        ntrees=200,
                        seed=1234)     




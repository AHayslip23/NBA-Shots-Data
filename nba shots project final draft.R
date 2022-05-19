knitr::opts_chunk$set(echo=TRUE)

Briefly describe your data: The data that I collected is from kaggle.com and is csv file that describes every shot that was taken during the 2014-2015 NBA season. This dataset includes variables whether the shot was made, how far the player was away from the hoop, whether the shot was worth 2 points or 3 points as well as how much time was on the shot clock

My research questions are the following

1) Which variables will have the most impact on whether a shot will go in the basket?
  2) is a random forest model a good model to use to predicting whether a shot will go in? Why or Why not? If not, explain what model you would use instead and why?
  
  explain random forest model and demo it 
The random forest model is a supervised learning algorithm that randomly craetes and merges multiple decision trees into on forest. The idea is not to rely on a single learning model, but a collection of decision models to improve accuracy.

library(TH.data)
library(rpart)
library(partykit)

glaucoma_rpart <- rpart(Class ~ ., data = GlaucomaM)

plot(as.party(glaucoma_rpart), tp_args = list(id = FALSE))

printcp(glaucoma_rpart)
50% error 

library(randomForest)
rf <- randomForest(Class ~ ., data = GlaucomaM)

print(rf)
error estimate 13.7% 

example <- table(predict(rf), GlaucomaM$Class)
example

print(round(example[1,1] / colSums(example)[1] * 100))
the random forest model predicts glaucoma with 83% accuracy 

print(round(example[2,2] / colSums(example)[2] * 100))
the random forest model predicts normal with a 88% accuracy 

exploratory data analysis reading the data 

getwd()
setwd("C:/Users/sdcha/Desktop/SDSU/Stats 410 R programming")

nba_shots <- read.csv("C:/Users/sdcha/Desktop/SDSU/Stats 410 R programming/nba_shot_data.csv")

step 1: exploatory data analysis cleaning data 

part 1 

I noticed that there a lot of NA values for shot clock, considnering that this is bad data, I will filter out all null values in the dataset 
library(tidyverse)
nba_shots <- drop_na(nba_shots) 

print(head(nba_shots))
I cleaned the dataset to remove 6000 data points that were no good as they had an NA value for shot clock 

part 2 visualizations 

library(ggplot2)

shot_distance <- ggplot(nba_shots, aes(x=SHOT_DIST, color=SHOT_MAKE, fill=SHOT_MAKE)) + geom_histogram()

print(shot_distance)

this is a histogram that compares whether a shot will go into the hoop or not based on how far they stood from the basket 
based on the results there are far more missed shots than made shots 
in particular the number of missed shots peaks at 25 feet from the basket
The number of made shots peaks around 5 feet from the basket 

shot_clock <- ggplot(nba_shots, aes(x=SHOT_CLOCK, color=SHOT_MAKE, fill=SHOT_MAKE)) + geom_histogram()
print(shot_clock)

this is a histogram that compares whether a shot will go in based on the amount of time that is left on the shot clock 
the number of missed shots peaks around 12 to 12.5 feet away from the basket 
additionally the number of made shots peaks around 12 to 12.5 feet away from the basket 


homevsaway <- ggplot(nba_shots, aes(x=LOCATION, y=SHOT_DIST, color=SHOT_MAKE, fill=SHOT_MAKE)) + geom_boxplot()
homevsaway

i create a boxplot to compare the distance that players shoot from at home as opposed to away, this is complimented by a fill that filters for whether the shot is made or not 
visitng players who miss shots tend to take shots between 6 and 23 feet away from the basket vistting players who make shots tend to take shots between 4 and 21 feet away from the basket
home players who miss shots tend to take shots between 6 and 23 feet away from the basket 
home players who make shots tend to take shots betwen 4 to 21 feet away from the basket 

View(nba_shots)

makeormiss <- ggplot(nba_shots, aes(x=as.factor(PTS_TYPE), y=SHOT_CLOCK, color=SHOT_MAKE, fill=SHOT_MAKE)) + geom_boxplot()
print(makeormiss)

i created a boxplot that factors out each point type by whether the type of shot is a two pointer or a three pointer as well as the fact that the fill is based on whether the player made or missed the shot 
if a player missess a two point shot, they probably took the shot between 7.5 and 15.5 seconds 
if a player makes a two point shot, they probably took the shot between 9 and 22.5 seconds 
if a player misses a three point shot they probably took the shot between 7.5 seconds and 16 seconds 
if a player makes a three point shot they probably took the shot between 8 seconds and 16.5 seconds 


makeormiss2 <- ggplot(nba_shots,aes(x=PERIOD, color=SHOT_MAKE, fill=SHOT_MAKE)) + geom_bar()
makeormiss2

i created a barplot that compares that shots that are made and missed by the period of the game
the players make the most shots in the first quarter and they miss the most shots in the fourth quarter. maybe the players are confident to start the game and crack under pressure when the game advances to the fourth quarter
The players miss the most shots in the first quarter and they miss the least amount of shots in the fourth quarter. I think that the players miss most frequently in the first quarter as they are trying to be the most agreesive in the early going to help their team build a lead. I could see some players being tentitaive to shoot in the fourth quarter if they have missed a lot of shots earlier in the game or if the game is close and they dont want to hold the team back by missing 
Games rarely go to overtime so the data that is collected for periods 5 for 7 is insignificant compared to the data collected for periods 1 to 4 

step 2: model selection train and test split 
library(caTools)
set.seed(410)

model <- sample.split(nba_shots$SHOT_MAKE, SplitRatio = 0.70)

train <- subset(nba_shots, model == T)
test <- subset(nba_shots, model == F)

before i create the tree i have decided to split the data into test and train data sets at a 0.70 split ratio
i will use the training data set to craete a model and create a decision tree 
i also created the train and test split as i will compare the train and test data frames to assess the probability that each shot will make it in the hoop

step 3: create decision tree 
library(rpart)
library(rpart.plot)

tree <- rpart(SHOT_MAKE ~ LOCATION + SHOT_NUMBER + PERIOD + SHOT_CLOCK + DRIBBLES + SHOT_DIST + PTS_TYPE + CLOSE_DEF_DIST + player_name , method='class', data=train)

summary(tree)
according to the data set the three most important vairables are shot distance shot clock and close defender distance and palyer_name 
the most important variables is shot_distance 


step 4: visualize inital tree 
library(partykit)
plot(as.party(tree), tp_args = list(id = FALSE))

based on this tree if a shot is outside of 4.9 feet you have a 60% of missing and a 40% chance of making it 
based on this tree if a shot is inside 4.9 feet you have a 38% chance of missing it and a 62% chance of making it 


step 5: evaluate inital tree
printcp(tree)
the only variable that is used is shot_distance
the root error is 45.6%

step 6: predict the probability that each shot will go in 
treepredict <- predict(tree, test)
print(treepredict)
showcases the probablity of each shot going in the hoop by comparing the training set and test training set  


step 7: add variable to represent whether the shot will go in based on the proabilities of no and yes 
treepredict <- as.data.frame(treepredict)
joiner <- function(x) {
if (x>=0.6020302) {
return("No")
}else{
return("Yes")
}
}

i wrote this function to return no if the probabilty exceeds 0.6020302 #if the probability is anything else return yes 
i have a better idea of what kind of shot was taken 
based on this function the shots that are predicted to miss where more than likely further away from the hoop and the shots that made it in were more than likely closer to the hoop 

treepredict$Shot_Make <- sapply(treepredict$No, joiner)

print(treepredict)

step 8 create table to assess accuracy of training vs test
nbashotstable <- table(treepredict$Shot_Make, test$SHOT_MAKE)
print(nbashotstable)

assess accuracy that player actually missed the shot 
print(round(nbashotstable[1,1] / colSums(nbashotstable)[1] * 100))
predict shots that are missed with 82% accuracy 


assess accuracy that a player atually made a shot 
print(round(nbashotstable[2,2] / colSums(nbashotstable)[2] * 100))
predicts shots that are made with 35% accuracy 

step 9 create random forest model to improve the accuracy 
library(randomForest)

rf.model <- randomForest(SHOT_MAKE ~ LOCATION + SHOT_NUMBER + PERIOD + 
SHOT_CLOCK + DRIBBLES + SHOT_DIST + PTS_TYPE + CLOSE_DEF_DIST, data = train, importance = TRUE)

rf.model
the random forest model reduces the error slightly from 45.6% to 38.9% 

step 10 create predictions
rfpredict <- predict(rf.model, test)

nbashotstable2 <- table(rfpredict, test$SHOT_MAKE)
print(nbashotstable2)

print(round(nbashotstable2[1,1] / colSums(nbashotstable2)[1] * 100))
the random forest model predicts shots that are missed with 83% accuracy 


print(round(nbashotstable2[2,2] / colSums(nbashotstable2)[2] * 100))
the random forest model predidcts shots that are made with 35% accuracy 

step 11 write conculusions 
conclusion: based on the results of this analysis I have came up with a few conclusions 
1) #the variables that are the most important in building the are shot distance shot clock and close defender distance. The only variable that is used in buliding the tree is the shot distance. The decision tree was built on whether a play takes a shot from 4.9 feet away and the likelihood as to whether or not they would make or miss the shotwould depend on how far they stood from the basket 

2) The random forest model does a better job than the decision tree of indicating whether a shot is missed. There isn't a difference between the decision tree and the random forest model of indicating whether a shot would be made as both models predict shots made with a 35% accuracy rate. I would recommend using the random forest model over the decision tree as it does a better job of reducing accuracy issues than the decision tree. Additionally, the error is reduced as well which makes the model reliable. 


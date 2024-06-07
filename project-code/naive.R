library(ggplot2)
library(e1071)
library(caret)
library(caTools)
library(yardstick)
library(tidyverse)


# I am going to do the Naive Bayes on the rating 
df=read.csv('./data_cleaned/ratings for the different manufactures.csv')
head(df)
colnames(df)

# select the column that will use later
df_use=subset(df,select = c(brand,manufacturer,reviews.rating,reviews.numHelpful))
head(df_use)
summary(df_use$reviews.rating)

# check the values in a column
unique(df_use$manufacturer)

# only consider the Microsoft first, use it as an example
df_use_Mic=df_use[df_use$manufacturer=='Microsoft',]
summary(df_use_Mic$reviews.rating)

# save the rating as factor
df_use_Mic$reviews.rating=as.factor(df_use_Mic$reviews.rating)

# checking the bar plot 
barplot(table(df_use$reviews.rating),xlab='ratings')

# separate the training and the testing set
splitdata<- sample.split(df_use_Mic,SplitRatio = 0.9)
traindata <- subset(df_use_Mic, splitdata == TRUE)
testdata <- subset(df_use_Mic, splitdata == FALSE)
nrow(traindata)


nav.mod <- naiveBayes(reviews.rating~.,traindata)
Micnb.pred<-predict(nav.mod,testdata,type="class" )
Micnb.prob<-predict(nav.mod,testdata,type="raw" )
truth_predicted <- data.frame(
  obs = testdata$reviews.rating,
  pred = Micnb.pred
)
truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)
#confusion matrix
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "orange", high = "green")

# Interpreting Naive Bayes model results
print(nav.mod)

# checking the accuracy
cMatrix <- table(Micnb.pred, testdata$reviews.rating)
confusionMatrix(cMatrix)

# The accuracy is low, therefore it's not relevance. 
# so I choose another way to do the Naive Bayes


# try to consider as a whole(test whether the manufacture influence the rating) and check whether the rating is 5 out of 5 using the naive bayes.
df_use_5=df_use
df_use_5$reviews.rating=as.factor(ifelse(df_use$reviews.rating==5,1,0))

summary(df_use_5$reviews.rating)

# separate the training and the testing set
splitdata<- sample.split(df_use_5,SplitRatio = 0.9)
traindata <- subset(df_use_5, splitdata == TRUE)
testdata <- subset(df_use_5, splitdata == FALSE)


nav.mod <- naiveBayes(reviews.rating~.,traindata)
Allnb.pred<-predict(nav.mod,testdata,type="class" )
Allnb.prob<-predict(nav.mod,testdata,type="raw" )
truth_predicted <- data.frame(
  obs = testdata$reviews.rating,
  pred = Allnb.pred
)
truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)
#confusion matrix
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "orange", high = "green")


# Interpreting Naive Bayes model results
print(nav.mod)

# checking the accuracy
cMatrix <- table(Allnb.pred, testdata$reviews.rating)
confusionMatrix(cMatrix)

# The result of accuracy is higher than only consider Microsoft, which means the rating including the different manufacture is higher than 
# only consider one manufacture, so manufacture may be a factor highly influencing the rating.  
# But the general accuracy is still low, we need more data to increasing the accuracy.




# Then I will do a Naive Bayes test about whether a consumer will choose to buy certain product or not.
df1=read.csv('./data/online_shoppers_intention.csv')
colnames(df1)

# select the column
df1=subset(df1,select = c("BounceRates","ExitRates","PageValues","Month","Administrative_Duration","Informational_Duration","ProductRelated_Duration","SpecialDay"))

# change the column type
df1$PageValues=as.factor(ifelse(df1$PageValues>0,1,0))

# separate the training and the testing set
splitdata<- sample.split(df1,SplitRatio = 0.9)
traindata <- subset(df1, splitdata == TRUE)
testdata <- subset(df1, splitdata == FALSE)
nrow(traindata)

nav.mod <- naiveBayes(PageValues~.,traindata)
Buynb.pred<-predict(nav.mod,testdata,type="class" )
Buynb.prob<-predict(nav.mod,testdata,type="raw" )
truth_predicted <- data.frame(
  obs = testdata$PageValues,
  pred = Buynb.pred
)
truth_predicted$obs <- as.factor(truth_predicted$obs)
truth_predicted$pred <- as.factor(truth_predicted$pred)
#confusion matrix
cm <- conf_mat(truth_predicted, obs, pred)
autoplot(cm, type = "heatmap") +
  scale_fill_gradient(low = "orange", high = "purple")


# Interpreting Naive Bayes model results
print(nav.mod)

# checking the accuracy
cMatrix <- table(Buynb.pred, testdata$PageValues)
confusionMatrix(cMatrix)

# The accuracy is relatively high. Using more data will improve the accuracy of test. 
# The columns I use above are factors that affecting whether people will buy a product.

#######################
#############################3






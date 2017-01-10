remove(list=ls())
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("e1071")
#install.packages("psych")
#install.packages("party")
#install.packages("rminer")
#install.packages("ROCR")
#install.packages("mice")
#install.packages("AUC")
#install.packages("partykit")
library(rpart)
library(rpart.plot)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(psych)
library(party)
library(plyr)
library(mice)
library(ROCR)
library(AUC)
library(partykit)

###---------------Read data--------------###
bank_df=read.csv("bankfull_NonNa.csv")
pre_campaign=as.factor(ifelse(bank_df$pdays==0,"no","yes"))
bank_df[["pdays"]] <- pre_campaign
colnames(bank_df)[colnames(bank_df) == "pdays"] <- "pre_campaign"
str(bank_df) #show the classes of all independent variables

###---------------Split the data for training and testing(80%/20%)---------------###
#Because the data is sorted in time order so get the random rows number? Or maybe more recent data should be used for testing?(in the paper)
set.seed(124)
n <- nrow(bank_df) 
sample.size <- ceiling(n*0.8) 
idx.train <- sample(n, sample.size) 
#s=createDataPartition(data_NonNA$y,times=1,p=0.8,list=FALSE)
data_train=bank_df[idx.train,]
data_test=bank_df[-idx.train,]

###--------------build the decison tree model-----------------------###
##using rpart()
dtm<-rpart(y~.,data_train,control=rpart.control(minsplit=20, minbucket = round(20/3), cp=0.005))
printcp(dtm)
bestcp <- dtm$cptable[which.min(dtm$cptable[,"xerror"]),"CP"];bestcp
summary(dtm)
rpart.plot(dtm,type=1,extra=100)


##using ctree()
ctm <- ctree(y ~ ., data = data_train)
plot(as.simpleparty(ctm),gp = gpar(fontsize = 8))
pred_prob_ct<-predict(ctm,newdata=data_test,type="prob")
class(pred_prob_ct)
pred_prob_ct <- matrix(unlist(pred_prob_ct), ncol = 2 ,byrow = TRUE)
dim(pred_prob_ct)
count(pred_prob_ct)

###-------------test the classifier in testing dataset---------------###

##using type="prob" returns the class-specific probabilities, type="class" returns a factor
pred_prob_dt<-predict(dtm,newdata=data_test)
pred_prob_dt

pred_prob_ct<-predict(ctm,newdata=data_test,type="prob")
pred_prob_ct

###--------------set the threshold to see the Confusion Matrix------------------------###

threshold <- 0.35
pred_dt<-as.matrix(factor(ifelse(pred_prob_dt[,2]>threshold,"yes","no")))
pred_ct=as.matrix(factor(ifelse(pred_prob_ct[,2]>threshold,"yes","no")))
##Confusion Matrix
confusionMatrix(pred_dt, data_test$y,positive = "yes")
confusionMatrix(pred_ct, data_test$y,positive = "yes")


###-------------ROC and AUC---------------------###


roc_dt=roc(pred_prob_dt[,2],data_test$y);roc_dt
plot(roc_dt)

roc_ct=roc(pred_prob_ct[,2],data_test$y);roc_ct
plot(roc_ct)

auc_dt=auc(roc_dt,min=0,max=1)
auc_dt

auc_ct=auc(roc_ct,min=0,max=1)
auc_ct
###Ctree got higher AUC
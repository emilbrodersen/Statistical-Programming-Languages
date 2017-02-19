####----------Deal with imbalanced dataset: Oversampling---------------------#####

install.packages("unbalanced")
install.packages("FNN")
library("unbalanced")
library(FNN)

# Adjust the response variable to 0/1 binary
data_train$y=as.factor(ifelse(data_train$y=="yes","1","0"))

summary(data_train$y)

# Calculate the SMOTE parameters
sum=length(data_train$y);sum
npos=round(sum*(1/2));npos
oripos=length(which(data_train$y == 1));oripos
newpos=npos-oripos;newpos
s = (npos/oripos) - 1 ; s
m = (sum-npos)/newpos ; m

# Implementing SMOTE
newdata= ubSMOTE(data_train[,-20],data_train[,20], 
                 perc.over = 347.6, k = 10,
                 perc.under = 128.8,
                 verbose = FALSE)

balancedTrain = as.data.frame(newdata)
colnames(balancedTrain)=colnames(data_train)
balancedTrain$age = round(balancedTrain$age)
balancedTrain$campaign = round(balancedTrain$campaign)
balancedTrain$previous = round(balancedTrain$previous)

#write.csv(balancedTrain, "balancedTrain.csv")

summary(newdata$y)
setwd("C:/HU/HU-S-3/SPL/project/bank")
bankfull <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";")

##################################   cleaning dataset  ##############################
##  removing variable “duration”
bank$duration <-NULL
##  changing variable “pdays”
bank$pdays <- ifelse(bank$pdays == 999, 0, 1)
#---------------------------------------------------------------------------------
## the total number of missing value
NAs <- bank == "unknown"
is.na(bank)[NAs] <- TRUE
sapply(bank,function(x) sum(is.na(x)))         
sapply(bank, function(x) length(unique(x)))
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(bank,2,pMiss)      # varialbe default has more than 20% missing
#----------------------------------------------------------------------------------
## predict missing value with MICE package
library(mice)
md.pattern(bank)
tempData <- mice(bank,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
bankfull_NonNA <- complete(tempData,1)

#################################   splitting data   ###############################
set.seed(124)
n <- nrow(bankfull_NonNA) 
sample.size <- ceiling(n*0.8) 
idx.train <- sample(n, sample.size) 
bank_train <- bankfull_NonNA[idx.train, ] 
bank_test <-  bankfull_NonNA[-idx.train, ]


###########################  Questionablea: cleaning data method2   ####################
## predict missing value in variable "marital"
library(nnet)
marital_train <- bank[which(bank$marital == "single"|bank$marital == "married"|
                              bank$marital == "divorced"),]
marital_test <- bank[which(is.na(bank$marital)),]
marital_train$marital1<-relevel(marital_train$marital, ref = "divorced")
marital_model <- multinom(marital1 ~age, data = marital_train)
summary(marital_model)
z <- summary(marital_model)$coefficients/summary(marital_model)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2    
exp(coef(marital_model))
head(pp <- fitted(marital_model))
predictMNL <- function(model, newdata) {
  if (is.element("nnet",class(model))) {
       probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
        vals <- runif(nrow(newdata))
        tmp <- cbind(cum.probs,vals)
       k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
       return(ids)
  }
}
marital_test$y1 <- predictMNL(marital_model,marital_test)
marital_test$marital<-NULL
library(reshape)
marital_test <- rename(marital_test, c(y1="marital"))
marital_train$marital1<-NULL
total <- rbind(marital_test, marital_train)
#------------------------------------------------------------------------------------

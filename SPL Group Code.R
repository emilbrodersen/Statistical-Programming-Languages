setwd("C:/HU/HU-S-3/SPL/project/bank")
bank <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";")

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
## predict structured missing value with Hmisc
install.packages("Hmisc")
library(Hmisc)
#using argImpute
levels(bank$job)[levels(bank$job)=='unknown'] <- NA
levels(bank$marital)[levels(bank$marital)=='unknown'] <- NA
levels(bank$education)[levels(bank$education)=='unknown'] <- NA
levels(bank$housing)[levels(bank$housing)=='unknown'] <- NA
levels(bank$loan)[levels(bank$loan)=='unknown'] <- NA
levels(bank$default)[levels(bank$default)=='unknown'] <- NA
impute_arg <- aregImpute(~ job + marital + education + default +
                           housing + loan, data = bank, n.impute = 5)
impute_arg
impute_arg$imputed$default    #check imputed variable default
# Conclusion: default has the following levels with < 5 observations: yes 
# Consider using the group parameter to balance bootstrap samples 
#----------------------------------------------------------------
## predict missing value with missForest
install.packages("missForest")
library(missForest)
bank.imp <- missForest(bank)
bank.imp$ximp        #check imputed values
bank.imp$OOBerror    #check imputation error
# Conclusion: 19% error, which is not good
#---------------------------------------------------------------
## predict missing value with mi package
install.packages("mi")
library(mi)
mi_data <- mi(iris.mis, seed = 335)
summary(mi_data)
# NOTE: The following pairs of variables appear to have the same missingness pattern.
# Please verify whether they are in fact logically distinct variables.
#----------------------------------------------------
## predict missing value with MICE package
library(mice)
md.pattern(bank)
install.packages("VIM")
library(VIM)
mice_plot <- aggr(bank, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,
                  labels=names(bank), cex.axis=.7, gap=3, 
                  ylab=c("Missing data","Pattern"))
tempData <- mice(bank,m=5,maxit=50,meth='polyreg',seed=500)
summary(tempData)
bankfull_NonNA <- complete(tempData,1)
#-------------------------------------------------------------------------
write.csv(bankfull_NonNA, "bankclean.csv")
bankclean <-read.csv('bankclean.csv', sep = ',', header = TRUE)
bankclean <- bankclean[,-1] 
library(Hmisc)
xyplot(bankclean,marital ~ job+housing+loan+education+default.R,pch=18,cex=1)
densityplot(bankclean)
stripplot(bankclean, pch = 20, cex = 1.2)
modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit1))

#################################   splitting data   ###############################
set.seed(124)
n <- nrow(bankclean) 
sample.size <- ceiling(n*0.8) 
idx.train <- sample(n, sample.size) 
bank_train <- bankclean[idx.train, ] 
bank_test <-  bankclean[-idx.train, ]


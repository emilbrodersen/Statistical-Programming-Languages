rm(list=ls())

#Load data
df <- read.csv("C:\\Users\\Emil\\Desktop\\Statistical Programming Languages\\Amazon Fine Foods\\amazon-fine-foods\\Reviews.csv")

#Dataframe with only the variables we need
mydf <- df[, c(5,6,7,10)]

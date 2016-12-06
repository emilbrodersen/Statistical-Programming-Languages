install.packages("data.table")
library(data.table)

df <- fread("C:\\Users\\Emil\\Desktop\\Statistical Programming Languages\\Amazon Fine Foods\\amazon-fine-foods\\Reviews.csv")

# Subset relevant columns of the dataset (Helpfullness numerator and denominator, 
# Score and Text)
df <- df[, c(5, 6, 7, 10)]

# Subset reviews with more then 10 helpful or not indications
df <- df[df$HelpfulnessDenominator>10]

# Count number of NA observations. There are zero.
sum(is.na(df))

# Convert text to lowercase
df$Text <- tolower(df$Text)

# Clean Text variable
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
df$Text <- cleanFun(df$Text)

df$Text <- gsub("length::", "", df$Text)

df$Text <- gsub("\\d+:\\d+", "", df$Text)

sum(sum(df$HelpfulnessNumerator)/sum(df$HelpfulnessDenominator)
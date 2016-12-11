install.packages("data.table")
library(data.table)

df <- fread("C:\\Users\\Emil\\Desktop\\Statistical Programming Languages\\Amazon Fine Foods\\amazon-fine-foods\\Reviews.csv")

# Create a 0/1 dependent variable depending on helpfullness numerator and denominator
df$Helpful <- ifelse(df$HelpfulnessNumerator/df$HelpfulnessDenominator>0.8, 1, 0)
df$Helpful.pct <- df$HelpfulnessNumerator/df$HelpfulnessDenominator

# Subset relevant columns of the dataset (Helpfullness numerator and denominator, 
# Score and Text)
#df <- df[, c(5, 6, 7, 10)]

# Subset reviews with more then 10 helpful or not indications
df <- df[df$HelpfulnessDenominator>10]

# Count number of NA observations. There are zero.
sum(is.na(df))

# Convert text to lowercase
df$Text <- tolower(df$Text)

# Clean Text variable
df$Text <- gsub("<.*?>", "", df$Text)
df$Text <- gsub("length::", "", df$Text)
df$Text <- gsub("\\d+:\\d+", "", df$Text)
df$Text = gsub("[[:punct:]]", "", df$Text)
df$Text = gsub("[[:digit:]]", "", df$Text)
df$Text = gsub("http\\w+", "", df$Text)
df$Text = gsub("[ \t]{2,}", "", df$Text)
df$Text = gsub("^\\s+|\\s+$", "", df$Text)

# Count how many times there is identical UserID. Then how many identical texts.
df$n <- as.numeric(ave(as.character(df$UserId), df$UserId, FUN = length))
df$n2 <- as.numeric(ave(as.character(df$Text), df$Text, FUN = length))

df.uniq <- unique(df, by = c(10))

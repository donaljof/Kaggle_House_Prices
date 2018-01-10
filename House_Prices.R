setwd("C:/Users/Donal/Documents/GitHub/Kaggle_House_Prices")

library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)

test <- read.csv("test.csv", stringsAsFactors = T)

train <- read.csv("train.csv", stringsAsFactors = T) 

str(train)

#are there any dud sales proces
mean(is.na(train$SalePrice))

hist(train$SalePrice)

lowprice <- subset(train, train$SalePrice < 90000)

#create a summary of Na % to identify poor features
nans <- summarise_all(train, funs( mean(is.na(.))))
nans
#exclude these cols in any analysis
for(column in col(nans)){
  if(nans[1,column] > 0.3){
    print(colnames(nans)[column])
    print(nans[1,column])
  }
}
# checking Na pool quality means no pool
poolarea <- subset(train, train$PoolArea > 0)
poolarea$PoolQC

#looking at some likely predictive features

ggplot(train, aes(x = LotArea, y = SalePrice)) +
  geom_point()

sale_plot <- function(df, xval, color = NULL, facet = FALSE){
  ggplot(df, aes_string(x = xval, y = "SalePrice", color = color)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = 'lm', se = F) +
    if(facet){
      facet_wrap(~facet)
    }
}
sale_plot(train, "LotArea")
sale_plot(train, "YearBuilt")
sale_plot(train, "YearRemodAdd")

sale_plot(train, "Neighborhood")

as.numeric(train$Neighborhood)


for(feature in colnames(train)){
  print (cor(train["SalePrice"], as.numeric(train[feature])))
}
                      
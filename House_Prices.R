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
  ggplot(df, aes_string(x = xval, y = "SalePrice", 
                        color = color)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = 'loess', se = F) +
    if(facet){
      facet_wrap(~facet)
    }
}
sale_plot(train, "LotArea")
sale_plot(train, "YearBuilt")
sale_plot(train, "YearRemodAdd")

sale_plot(train, "Neighborhood")

#Converting all factors to integer values and calulating r values
corr_table <- data.frame()
fisher.test()
corr_table["test"] <- train["SalePrice"][,1]
train["PoolQC"][is.na(train["PoolQC"][,1]),1]
train["PoolQC"][,1]

for(feature in colnames(train)){
  if(is.factor(train[feature][,1])){
    train[feature][,1] <- as.integer(train[feature][,1])
    
  }
  train[feature][is.na(train[feature][,1]),1] <- 0
  print(paste(feature, cor(train["SalePrice"][,1],
                           as.integer(train[feature][,1]))))
}

#there are 3 non factors with Na's present,
#moving the na conversion to apply to all features fixed this
#but without normalization these could cause issues:
sale_plot(train, "LotFrontage")
sale_plot(train, "MasVnrArea")
sale_plot(train, "GarageYrBlt")
#Garage year build is the only mad looking one 0.26
train_0G <- subset(train, train$GarageYrBlt != 0)
plot(train_0G$GarageYrBlt, train_0G$YearBuilt)
#most of the time this is the same as house age, update to age
train$GarageAge <- ifelse(train$GarageYrBlt == 0,
                          2010 - train$YearBuilt,
                          2010 - train$GarageYrBlt)
sale_plot(train, "GarageNewness")
#some negative numbers
oldgs <- subset(train, train$GarageNewness < 0 )
#remodelling year could be treated similar
plot(train$YearBuilt, train$YearRemodAdd)
sale_plot(train, "YearBuilt")
sale_plot(train, "YearRemodAdd")
train$RemoAge <- ifelse(train$YearBuilt == train$YearRemodAdd |
                          train$YearRemodAdd == 1950,
                        2010 - train$YearBuilt,
                        2010 - train$YearRemodAdd)

plot(train$RemoAge, train$YearBuilt)
sale_plot(train, "RemoAge")

#rerun the corr code and save as DF
corr_table <- NULL
for(feature in colnames(train)){
  r_value = cor(train["SalePrice"][,1], 
                as.integer(train[feature][,1]))
  print( (data.frame(feature, r_value)))
  corr_table <- rbind(corr_table, data.frame(feature, r_value))
}
#arrange by |R| and show top 10
corr_table <- arrange(corr_table, desc(abs(r_value)))
head(corr_table, 11)
plot(abs(corr_table$r_value))

#look at some high r releationships
sale_plot(train, "OverallQual")
sale_plot(train, "GrLivArea")
sale_plot(train, "GarageCars")
sale_plot(train, "ExterQual")
sale_plot(train, "GarageArea")
sale_plot(train, "TotalBsmtSF")
sale_plot(train, "X1stFlrSF")
sale_plot(train, "KitchenQual")
sale_plot(train, "FullBath")
sale_plot(train, "TotRmsAbvGrd")

train$TotArea <- train$GrLivArea + train$X1stFlrSF + train$X2ndFlrSF

sale_plot(train,"TotArea", color = "TotRmsAbvGrd")

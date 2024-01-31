
# load packages
library(ggplot2)
library(GGally)

library(lmtest)
library(olsrr)

library(faraway)
library(usdm)
library(car)

library(plm)
library(gplots) 

AutoLoss <- read.csv ("AutoLoss.csv", na.strings = "?", header = TRUE) #? Recognized as NA (missing)
AutoLoss <- na.omit(AutoLoss) #Removing missing data entry

str(AutoLoss)
summary(AutoLoss)

##### DataSet: AutoLoss #####

## a) Run a linear regression using Losses as the dependent variable and al lthe other numerical variables as independent variables 
##    Which cariables are significatnt at 0.05 level? What is the adj R Square od the Regression?

## Step 1. run regression
reg.mlr <- lm(Losses ~  Length + Width + Height + Weight + EngineSize + Horsepower + PeakRPM + Citympg + Price, data=AutoLoss)

## Step 2. See the summary result
summary(reg.mlr)

## b) Examine the correlations of the X variables in the above regression.
##    Are you concerned about potential multicollienarrity> Support your answer
str(AutoLoss)
colnames(AutoLoss)
round(cor(AutoLoss[,7:15]),3) #correlation of each variables rounding 3 decimal
ggpairs(AutoLoss[,7:15]) #plotting the correlations

## c) Use vif function to find the VIF value of each variable in the above regression. Print your result
##    Which variable has the largest VIF value?
##    Which variables have VIF values greater than 5? 
##    If you plan to use a threshold of 5 for VIF value, would you remove all these variables at once? Why?, or Why not?

# Formal multicolinearity analysis
# Checking VIF (Variance inflation factors): VIF assesses the severity of Multicollinearity
# VIF measures how much the variance of the slope will be inflated by multicollinearity. 
# Typical cutoff is 10 or 5 or can be lower (e.g., 2.5).
# VIF = 1 means the estimate of the coefficient is not affected by multicollinearity

# VIF (in car package requires model as input, VIF in usdm requires x variables as input, faraway accepts both)
usdm::vif(AutoLoss[,7:15])
faraway::vif(AutoLoss[,7:15])

# or test VIF from regression results; VIF scores should be the same
reg.mlr2 <- lm(Losses ~  Length + Width + Height + Weight + EngineSize + Horsepower + PeakRPM + Citympg + Price, data=AutoLoss)
car::vif(reg.mlr2) 
faraway::vif(reg.mlr2)

## d) Use vifsetp() function from the usdm package to remove variables with VIF value greater than 5. 
##    Which variables are removed?

# When we drop a variable, the VIF of the rest variable changes. Should do VIF stepwise (usdm)
# VIF Stepwise
usdm::vifstep(AutoLoss[,7:15], th=5) # th = threshold

## e) Run a regression with the remaining variables. 
##    Which variables are significantly at 0.05 level and what is the adj R Square this time?
##    Compare with your previous answers (Particularly R Square) and comment on whether removing those variables in d) is a good idea.
reg.mlr3 <- lm(Losses ~ Width + Height + EngineSize + PeakRPM + Citympg + Price, data=AutoLoss)
summary (reg.mlr3)

## f) Include the categorical variables from the dateset together with the above numerical variables from e)
##    Run a regression again. What doefficients do you get for variable NumDoors?
##    How do you explain it? Doe it make sense?
reg.mlr4 <- lm(Losses ~ Width + Height + EngineSize + PeakRPM + Citympg + Price + as.factor(FuelType) + as.factor(Aspiration) + as.factor(NumDoors) + as.factor(BodyStyle) + as.factor(DriveWheels), data=AutoLoss)
summary(reg.mlr4)

## g) Examine the residual plots. 
##    Are you concerend about normality, constant standard deication, or outliers?
par(mfrow = c(2,2))
plot(reg.mlr4)

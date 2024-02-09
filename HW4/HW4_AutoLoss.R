install.packages("fastDummies") #  create dummy variables
install.packages("glmnet") # for LASSO, Ridge, elastic net
install.packages('Rcpp') # fixing an error when using glmnet function; "function 'Rcpp_precious_remove' not provided by package 'Rcpp'"
install.packages("caret")


library(ggplot2)
library(GGally)
library(lmtest)
library(olsrr)
library(faraway)
library(usdm)
library(car)

library(fastDummies)

library(glmnet) 
library(caret)
library(Rcpp)

AutoLoss <- read.csv("AutoLoss.csv", na.strings = "?")
AutoLoss <- na.omit(AutoLoss)

str(AutoLoss)
summary(AutoLoss)
colnames(AutoLoss)

# a) Convert string variables from the original dataset to dummy variables.
#    Create a new data set with including the dummy variables
AutoLoss2 <- dummy_columns(AutoLoss, select_columns = c("FuelType", "Aspiration", "NumDoors", "BodyStyle", "DriveWheels"), remove_first_dummy = TRUE, remove_selected_columns=TRUE)

colnames(AutoLoss2)

# b) Fit a LASSO model to the data set, using Losses at the response and all other variables as predictors
# ranging from lambda = 10

## Lasso (Least Absolute Shrinkage and Selection Operator) is one of the most popular regularized (penalty-based) 
## regression model especially when the number of features (independent variables) is large. 

#Step 1. Build a Lasso model and examine coefficient 
#Ridge and Lasso requires separating X variables and Y variable. 
x=as.matrix(AutoLoss2[,-c(1)]) # remove first column (Losses)
y=as.matrix(AutoLoss2["Losses"])
lasso_10 <- glmnet(x, y, alpha =1 ,lambda= 10) # alpha = 0 for Ridge, 1 for Lasso 
coef_10 <- round(coef(lasso_10),4)
coef_10
# interpretation: 

#Step 2. post-LASSO regression 
# which columns are non-zeros (selected columns)
names(lasso_10) # a0 = intercept
lasso_10$beta #same with coef_10, but without intercept
which(lasso_10$beta!=0) #show beta which is not 0
x_selected=x[,which(lasso_10$beta!=0)]
post_10<-lm(Losses ~ ., data=data.frame(cbind(y,x_selected)))
summary(post_10)

# c) For Lambda = 1: What predictors are included in the resulting model? 
#    For Lambda = 1.96: What predictors are included in the resulting model?
lasso_1 <- glmnet(x, y, alpha =1 ,lambda= 1) # alpha = 0 for Ridge, 1 for Lasso 
coef_1 <- round(coef(lasso_1),4)
coef_1
lasso_1$beta #same with coef_10, but without intercept
which(lasso_1$beta!=0) #show beta which is not 0
x_selected=x[,which(lasso_1$beta!=0)]
post_1<-lm(Losses ~ ., data=data.frame(cbind(y,x_selected)))
summary(post_1)

lasso_196 <- glmnet(x, y, alpha =1 ,lambda= 1.96) # alpha = 0 for Ridge, 1 for Lasso 
coef_196 <- round(coef(lasso_196),4)
coef_196
lasso_196$beta #same with coef_10, but without intercept
which(lasso_196$beta!=0) #show beta which is not 0
x_selected=x[,which(lasso_196$beta!=0)]
post_196<-lm(Losses ~ ., data=data.frame(cbind(y,x_selected)))
summary(post_196)

# d) What do you observe about the coefficient estimates you obtain as lambda increases?
#    Should use post LASSO linear regression coefficient
post_10$coefficients
post_196$coefficients
post_1$coefficients

# Comparing three lambdas 
coef_lasso<-cbind(coef(lasso_10), coef(lasso_196),coef(lasso_1)) # compare results from two lambdas
colnames(coef_lasso)<-c("lambda=10", "lambda=1.96", "lambda=1") # rename column names
coef_lasso


# e) Use 6-fold cross-validation to find the best value for lambda. Remember to include set.seed(566)
#    before using cv.glmnet(), so we all end up making the same split. 
#    Paste the MSE plot. State the best value of the tuning parameter lambda that minimize cross validated MSE.

# Step 1. Run CV-lasso
set.seed(566) # set random seed first such as results can be replicated.
fit_lasso_cv <- cv.glmnet(x, y, alpha = 1, nfolds =6) # 10-fold cross validation
fit_lasso_cv 
plot(fit_lasso_cv) 

fit_lasso_cv$lambda.min
# lambda that minimizes RMSE = sqrt(mean squared error); mean cross-validated error
# min Lambda: 1.629

# f) Find the value of lambda such that the cross-validated error is exactly 1 standard error away from the minimize error. 
#    Describe how sparse the model is (how many variables are selected at this value?)

fit_lasso_cv$lambda.1se #1 Standard Error
# lambda that the RMSE is within 1 standard error of the minimum; larger lambda, sparser model
# 1se Lambda: 6.576 
# How sparse the model is.. 6 variables are selected at this value

coef(fit_lasso_cv,s="lambda.min") 
coef(fit_lasso_cv,s="lambda.1se") # default

# g) Fit and OLS linear regression model using variables selected in (f). 
#    Summarize your linear regression output. Report which variables are statistically significant at 0.05 level.
#    What is the adjusted R-squared of the linear model?
#    OLS: Ordinary Least Squares Regressions

# Step 2. Run post-LASSO regression
# predicted Y value with the best LASSO coefficient (default is lambda.1se)
which(coef(fit_lasso_cv)!=0)
# x_selected include only selected columns
x_selected=x[ , which(coef(fit_lasso_cv)!=0)-1] # -1: take out the intercept away

# create a new dataframe and run a linear regression
postdata<-data.frame(cbind(y,x_selected))
post_cv<-lm(Losses ~ ., postdata)
summary(post_cv)

# 0.05 significant: Height, NumDoors_two, BodyStyle_wagon, DriveWheels_rwd
# adjusted R square: 0.3739





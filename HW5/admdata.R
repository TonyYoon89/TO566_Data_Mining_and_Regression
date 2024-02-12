
library(pscl)
library(MASS) 
library(rsq)
library(fastDummies)
library(caret)
library(e1071)

adm <- read.csv("admdata.csv", header = TRUE)

str(adm)
summary(adm)
colnames(adm)

## a) Tabulate the variable admit. What is the average probability of an applicant being admitted to a graduate program?
adm2 <- dummy_columns(adm, select_columns = c("admit", "rank"), remove_first_dummy = TRUE, remove_selected_columns=TRUE)
colnames(adm2)

table(adm2$admit_yes)
prop.table(table(adm2$admit_yes)) # tabulate percentage instead of frequency

## b) Run a logistic regression of the outcome variable on the other variables.
log<-glm(admit_yes ~ ., data=adm2, family= binomial)
summary(log)

## c) Compare the odds of admission for applicants graduating from each tier of undergraduate institutions. 

## d) what is the predicted admission probability for an applicant with GRE 720, GPA=3.85, 
##    who graduated from a Tier2 undergrad institution?

applicant <- data.frame(gre=720, gpa = 3.85, rank_Tier2 = 1, rank_Tier3 = 0, rank_Tier4 = 0)
predict(log, newdata = applicant, type = "response")


## e) Predict probabilities of admission for all applicants in the dataset. Use 0.5 as the cutoff to make predictions of admission status (yes vs. no). 
adm2$admit_prob <- predict(log, newdata = adm2, type="response") # same scale as the response variable, which gives us a probability between 0, 1
head(adm2$admit_prob)

adm2$admit_prediction <- 0
adm2$admit_prediction[adm2$admit_prob>0.5] <- 1 # predicted DOT delay = 1
str(adm2)

conf_table <- confusionMatrix(as.factor(adm2$admit_prediction), as.factor(adm2$admit_yes), positive="1")
conf_table

library(usdm)
library(fastDummies)

boat <- read.csv("boatingtrips.csv", header = TRUE)
str(boat)
summary(boat)
colnames(boat)

## a) Plot histogram for the outcome variable (trips)
hist(boat$trips)

## b) Analyze potential multicollinearity issue of the X variables (all variables) 
##    th=10

# Convert string variables from the original dataset to dummy variables.
boat2 <- dummy_columns(boat, select_columns = c("ski", "userfee"), remove_first_dummy = TRUE, remove_selected_columns=TRUE)
colnames(boat2)

usdm::vifstep(boat2[,], th=10)

# Eliminated variables: costC, costH

## c) Run poisson regression of the outcome variable on the rmaining variabls.
##    Which variable are significnt predictors of trips at 0.05 level?

pois1 <- glm(trips ~ quality + income + costS + ski_yes + userfee_yes, data = boat2, family = poisson)
summary(pois1)

# significant predictors at 0.05 level: quality, userfee_yes

## d) How does the expected number of boating trips differ for owners who have paid annual user fees vs. those who haven't?
## Userfee estimante = 0.8625390


## e) Run a negative binomial regression of the outcome variable on the same set of variables as in (c)
##    What is the your answer to (d)?
nbin <- MASS::glm.nb(trips ~ quality + income + costS + ski_yes + userfee_yes, data = boat2,)
summary(nbin)

## Userfee estimante = 1.596247

## f) Which model provides better model fit? Poisson or negative binomial? Suport your answer.

## Model comparison
# compare coefficients
round(cbind("Pois" =pois1$coefficients,"NB" = nbin$coefficients),3)

# compare model fit
rbind(AIC = sapply(models, function(x) AIC(x)), 
      BIC = sapply(models, function(x) BIC(x)), 
      logLik = sapply(models, function(x) logLik(x))) 
# lower in AIC, BIC / higher in logLik is better -> NB is better model.





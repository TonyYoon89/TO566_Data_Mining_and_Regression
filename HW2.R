# TO566 HW 02 - Tony Yoon

# packages for graphics
install.packages("ggplot2")
install.packages("GGally")

# packages for regression diagnostics 
install.packages("lmtest")
install.packages("olsrr")

# package for robust estimation
install.packages("estimatr")

library(ggplot2)
library(GGally)
library(lmtest)
library(olsrr)
library(estimatr)

Bikeshare <-read.csv("Bikeshare.csv")

View (Bikeshare)

str(Bikeshare)
dim(Bikeshare)
summary(Bikeshare)

# lm: Fittign Linear Models: "Rental" as the output variable and "Temperature" as the input variable
reg_tem <- lm(Rentals ~ Temperature, data = Bikeshare)
summary(reg_tem)

# Rentals = 6640.7 * Temperature + 1214.6
# Check p-value of Temperature (<2e-16) and p-value of entire model (<2.2e-16)

# Standardized temperature in Celsius = (Temperature on that day - t_min) / (t_max - t_min) 
# t_max = +39, t_min = -8
# Standardized temperature = (Temperature on that day + 8) / 47 = Temperature on that day / 47 + 8 / 47

# lm: Fittign Linear Models: "Rental" as the output variable and "Humidity" as the input variable
reg_hum <- lm(Rentals ~ Humidity, data = Bikeshare)
summary(reg_hum)

# lm: Fittign Linear Models: "Rental" as the output variable and "Windspeed" as the input variable
reg_win <- lm(Rentals ~ Windspeed, data = Bikeshare)
summary(reg_win)

# Scatter plot + regression line + confidence interval
# geom_smooth: trend line especially for scatter plot 
ggplot(Bikeshare, aes(x=Temperature, y=Rentals)) + 
  geom_point() + 
  geom_smooth(method ="lm", level = 0.95)
  ggtitle("scatter + regression line")

predict(reg_tem, Bikeshare, interval="prediction", level=0.95) # 95% prediction interval
pred_interval <- predict(reg_tem, Bikeshare, interval="prediction", level =0.95) 
Bikeshare11 <- data.frame(Bikeshare, pred_interval)

#Add regression line with 95% prediction interval
ggplot(Bikeshare11, aes(x = Temperature, y = Rentals)) + 
  geom_point() + 
  geom_line(aes(y = lwr), linetype = "dashed", color = "blue") +
  geom_line(aes(y = upr), linetype = "dashed", color = "blue") +
  ggtitle("Rentals vs. Temperature with Prediction Interval")

# Scatter plot + regression line + confidence interval + prediction interval
ggplot(Bikeshare11, aes(x=Temperature, y=Rentals)) + 
  geom_point() + 
  geom_smooth(method ="lm", level = 0.95) +
  geom_line(aes(y = lwr), linetype = "dashed", color = "blue") +
  geom_line(aes(y = upr), linetype = "dashed", color = "blue") +
  ggtitle("scatter + 95% confidence and prediction interval")

# Provide 90% prediction interval with fitted values for the 5 cases
newdata<-data.frame(Temperature=c(0.25,0.33,0.50,0.66,0.75)) # fitted values (Check what original figures look like)
predict(reg_tem, newdata, interval="prediction", level=0.90) # 90% prediction interval

# Add lower and upper limits of prediction interval 
pred_interval <- predict(reg_tem, Bikeshare, interval="prediction", level =0.90) 
# add the new prediction columns to the data frame
Bikeshare1 <- data.frame(Bikeshare, pred_interval)

# plot residuals using ggplot
Bikeshare1$fittedvalue<-reg_tem$fitted.values
Bikeshare1$res<-reg_tem$residuals

# plot residuals against Temperature
ggplot(Bikeshare1, aes(x=Temperature, y=res)) + 
  geom_point() +  
  ggtitle("Temperature vs. residuals") 


# After we run the regression, the following four plots are automatically generated. 
plot(reg_tem)

# show all four plots in 2 by 2; define multiple frame; details on slides
par(mfrow = c(2,2))
plot(reg_tem)

# How to interpret the figures?
# 1. normality assumption
# 2. constant standard deviations of the error
# 3. influence outliers

# Use robust standard errors. Double check if statistical significance still hold
reg_robust <- lm_robust(Rentals ~ Temperature, data = Bikeshare)
summary(reg_robust)

# Multivariate regression
# before running a regression, first think about what variables are potentially important to control
reg_all <- lm(Rentals ~ Temperature + Humidity + Windspeed, data = Bikeshare)
summary(reg_all)

# See the residual plots.
par(mfrow = c(2,2))
plot(reg_all)

# Rental prediction with Temperature 20, Humidity 65, and Windspeed 8
t_min = -8
t_max = 39
wind_max = 67
temp_standardized = (20 - t_min) / (t_max - t_min)  # Temperature = 20 degrees Celsius
humidity_standardized = 65 / 100                    # Humidity = 65%
windspeed_standardized = 8 / wind_max               # Windspeed = 8 km/h

# Creating new data frame with standardized values
newData <- data.frame(Temperature = temp_standardized,
                      Humidity = humidity_standardized,
                      Windspeed = windspeed_standardized)

# Predicting rentals with the 95% prediction interval
predict(reg_all, newData, interval="prediction", level = 0.95)


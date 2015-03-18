#PREDICTING FINE PARTICULATE MATTER IN BEIJING

#Need to run prior:
#merge_beijing.r
#scrape_weather.r OR use datafiles in Github (better!)
#merge_weather.r

#Create the outcome variables:
#Average PM 2.5 levels for the next five days
##Issue: This should work instead of the kludgy list of numbers
##cat(paste(shQuote(0:23, type="cmd"), collapse=", "))
##But for some reason it's producing a null at the end of the list. Why?

air$average.today <- rowMeans(air[,c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","23")], na.rm=T)

#Create lagged variables

air$average.d1 <- c(air$average.today[2:length(air$average.today)], NA)
air$average.d2 <- c(air$average.today[3:length(air$average.today)], NA, NA)
air$average.d3 <- c(air$average.today[4:length(air$average.today)], NA, NA, NA)
air$average.d4 <- c(air$average.today[5:length(air$average.today)], NA, NA, NA, NA)
air$average.d5 <- c(air$average.today[6:length(air$average.today)], NA, NA, NA, NA, NA)

###
### PREDICTION USING REGRESSION
###

#First, predictions running a least squares regression model
#As a first cut, we'll use 2008-2013 data and 'validate' on 2014

model.ols <- lm(average.d1~particulate_23+Temp_11_00_PM+Dew_Point_11_00_PM+Humidity_11_00_PM+Pressure_11_00_PM+Visibility_11_00_PM+Wind_Dir_11_00_PM+Wind_Speed_11_00_PM+Conditions_11_00_PM, data=air[which(air$Year_x<2014),])

summary(model.ols)

#Create a validation set, while dealing with some factors that are unique to 2014
validation <- air[which(air$Year_x==2014),]
validation$Wind_Dir_11_00_PM[which(validation$Wind_Dir_11_00_PM=="Calm")] <- NA
validation$Conditions_11_00_PM[which(validation$Conditions_11_00_PM=="Thunderstorm")] <- NA
validation$Conditions_11_00_PM[which(validation$Conditions_11_00_PM=="Partial Fog")] <- NA

#The range for each category in the official Air Quality Index is 50
#(e.g. Healthy is 0 to 50; moderate 51 to 100)
#Arbitarily, I'll count a prediction as correct if it's +/- 25 of the actual value

error.ols <- as.vector(predict(model.ols, validation, type="response")-validation$average.d1)
length(which(error.ols<25))/length(which(is.na(error.ols)==FALSE))

###OLS: 59.6 percent prediction rate, as defined above.
###Second, predictions using a Poisson (count) model

model.poisson <- glm(average.d1~particulate_23+Temp_11_00_PM+Dew_Point_11_00_PM+Humidity_11_00_PM+Pressure_11_00_PM+Visibility_11_00_PM+Wind_Dir_11_00_PM+Wind_Speed_11_00_PM+Conditions_11_00_PM+as.factor(Month_x), data=air[which(air$Year_x<2014),], family="poisson")

summary(model.poisson)

error.poisson <- as.vector(predict(model.poisson, validation, type="response")-validation$average.d1)
length(which(error.poisson<25))/length(which(is.na(error.poisson)==FALSE))

### Poisson: 52.3 percent prediction rate, as defined above.
### Interestingly, worse than OLS!

###
### PREDICTION USING MACHINE LEARNING
###

##Random Forest

##SuperLearner

library(SuperLearner)





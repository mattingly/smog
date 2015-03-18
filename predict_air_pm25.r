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
names(air)

rowMeans(air[,c(shQuote(paste("particulate", 0:23, sep="_")))], na.rm=T)

paste(as.character(particulate)
air$average.today <- rowMeans(air[,c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","23")], na.rm=T)

#Create lagged variables

air$average.d1 <- c(air$average.today[2:length(air$average.today)], NA)
air$average.d2 <- c(air$average.today[3:length(air$average.today)], NA, NA)
air$average.d3 <- c(air$average.today[4:length(air$average.today)], NA, NA, NA)
air$average.d4 <- c(air$average.today[5:length(air$average.today)], NA, NA, NA, NA)
air$average.d5 <- c(air$average.today[6:length(air$average.today)], NA, NA, NA, NA, NA)


#As a first cut, we'll use 2008-2013 data to train the models and validate on 2014
#Create the training set
train <- air[which(air$Year_x<2014),]
variables <- c("average.d1", "particulate_23","Temp_11_00_PM","Dew_Point_11_00_PM","Humidity_11_00_PM","Pressure_11_00_PM","Visibility_11_00_PM","Wind_Dir_11_00_PM","Wind_Speed_11_00_PM","Conditions_11_00_PM")
train <- train[,variables]

#Create the validation set, while dealing with some factors that are unique to 2014
validation <- air[which(air$Year_x==2014),]
validation$Wind_Dir_11_00_PM[which(validation$Wind_Dir_11_00_PM=="Calm")] <- NA
validation$Conditions_11_00_PM[which(validation$Conditions_11_00_PM=="Thunderstorm")] <- NA
validation$Conditions_11_00_PM[which(validation$Conditions_11_00_PM=="Partial Fog")] <- NA
validation <- validation[,variables]



###
### PREDICTION USING REGRESSION
###

###LEAST SQUARES REGRESSION###

model.ols <- lm(average.d1~particulate_23+Temp_11_00_PM+Dew_Point_11_00_PM+Humidity_11_00_PM+Pressure_11_00_PM+Visibility_11_00_PM+Wind_Dir_11_00_PM+Wind_Speed_11_00_PM+Conditions_11_00_PM, data=train)

summary(model.ols)

#The range for each category in the official Air Quality Index is 50
#(e.g. Healthy is 0 to 50; moderate 51 to 100)
#Arbitarily, I'll count a prediction as correct if it's +/- 25 of the actual value

error.ols <- as.vector(predict(model.ols, validation, type="response")-validation$average.d1)
length(which(error.ols<25))/length(which(is.na(error.ols)==FALSE))

##OLS: 60 percent prediction rate, as arbitrarily defined above##


###POISSON REGRESSION###

model.poisson <- glm(average.d1~particulate_23+Temp_11_00_PM+Dew_Point_11_00_PM+Humidity_11_00_PM+Pressure_11_00_PM+Visibility_11_00_PM+Wind_Dir_11_00_PM+Wind_Speed_11_00_PM+Conditions_11_00_PM, data=air[which(air$Year_x<2014),], family="poisson")

summary(model.poisson)

error.poisson <- as.vector(predict(model.poisson, validation, type="response")-validation$average.d1)
length(which(error.poisson<25))/length(which(is.na(error.poisson)==FALSE))

##Poisson: 52 percent prediction rate, as defined above##

###
### PREDICTION USING MACHINE LEARNING
###

###RANDOM FORESTS###

library(randomForest)

#For NA the randomForest function does not automatically delete listwise
#So I will manually do so... Future versions will use multiple imputation.

train.forest <- na.omit(train)

set.seed(822)

fit.forest <- randomForest(average.d1~particulate_23+Temp_11_00_PM+Dew_Point_11_00_PM+Humidity_11_00_PM+Pressure_11_00_PM+Visibility_11_00_PM+Wind_Dir_11_00_PM+Wind_Speed_11_00_PM+Conditions_11_00_PM, data=train.forest, importance=TRUE, ntree=2000)

varImpPlot(fit.forest)

error.forest <- as.vector(predict(fit.forest, validation, type="response")-validation$average.d1)
length(which(error.forest<25))/length(which(is.na(error.forest)==FALSE))

###In this instance, a 58 percent prediction rate


###CONDITIONAL INFERENCE TREES###

library(party)

set.seed(822)

fit.cforest <- cforest(average.d1~particulate_23+Temp_11_00_PM+Dew_Point_11_00_PM+Humidity_11_00_PM+Pressure_11_00_PM+Visibility_11_00_PM+Wind_Dir_11_00_PM+Wind_Speed_11_00_PM+Conditions_11_00_PM, 
               data=train.forest,
               controls=cforest_unbiased(ntree=2000, mtry=3))



error.cforest <- as.vector(predict(fit.cforest, validation,  OOB=TRUE, type="response")-validation$average.d1)
length(which(error.cforest<25))/length(which(is.na(error.cforest)==FALSE))

###In this instance, a 59 percent prediction rate

#Clean up the workspace
rm(error.cforest, error.forest, error.ls, error.poisson, fit.cforest, fit.forest, model.ols, model.poisson, GCtorture, error.ols, train.forest)

###SUPERLEARNER ALGORITHM###

library(SuperLearner)

Y <- train$average.d1

train.SL <- na.omit(train)
validation.SL <- na.omit(validation)

SL.library <- c("SL.glm", "SL.randomForest", "SL.gam",
                "SL.polymars", "SL.mean", "SL.cforest", "SL.bayesglm")

fit.SL <- SuperLearner(Y = train.SL$average.d1, X = train.SL[,variables[2:10]], newX = validation.SL[,variables[2:10]], SL.library = SL.library,
                     verbose = TRUE, method = "method.NNLS")


predict.SuperLearner(fit.SL, validation.SL, onlySL=TRUE)


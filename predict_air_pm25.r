#PREDICTING FINE PARTICULATE MATTER IN BEIJING

#Need to run prior:
#merge_beijing.r
#scrape_weather.r OR use datafiles in Github (better!)
#merge_weather.r



#You will likely need to install the SuperLearner package:
#install.packages("SuperLearner", dependencies=TRUE)

library(SuperLearner)

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


#

model <- lm(average.d1~ air$"23", data=air)
summary(model)

model <- lm(average.d1~air$"23"+air$"11_00_PM_Temp_"+air$"11_00_PM_Dew_Point"+air$"11_00_PM_Humidity"+air$"11_00_PM_Pressure"+air$"11_00_PM_Visibility"+air$"11_00_PM_Wind_Dir"+air$"11_00_PM_Wind_Speed"+air$"11_00_PM_Conditions", data=air)
summary(model)


model <- lm(average.d1~air$"23"+air$"11_00_PM_Temp_"+air$"11_00_PM_Dew_Point"+air$"11_00_PM_Humidity"+air$"11_00_PM_Pressure"+air$"11_00_PM_Visibility"+air$"11_00_PM_Wind_Dir"+air$"11_00_PM_Wind_Speed"+air$"11_00_PM_Conditions"+as.factor(air$Month_x), data=air)
summary(model)
head(air)
model <- glm(average.d1~air$"23"+air$"11_00_PM_Temp_"+air$"11_00_PM_Dew_Point"+air$"11_00_PM_Humidity"+air$"11_00_PM_Pressure"+air$"11_00_PM_Visibility"+air$"11_00_PM_Wind_Dir"+air$"11_00_PM_Wind_Speed"+air$"11_00_PM_Conditions", data=air, family="poisson")

gsub(" ", "", , fixed = TRUE)

paste(colnames(air)[c(5:220)], collapse = "+")

#Now on to the SuperLearner algorithm




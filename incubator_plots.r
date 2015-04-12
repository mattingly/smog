setwd("~/Dropbox/research/pollution")

#Read in hourly data for Beijing 
#This has been downloaded and merged separately
data <- read.csv("beijing_hourly.csv")

#Standardize NAs
data[data==-999] <- NA

#Rename months
data$Month <- as.factor(data$Month)
levels(data$Month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

###################################
# Which month is the most polluted?
# Visualize daily averages 2012-2014
###################################

library(plyr)

#Group data using plyr
data2 <- ddply(data, .(Month, Day), summarize, Value=mean(Value, na.rm=T))

library(ggplot2)

pdf('particulate_daily_ave.pdf')
ggplot(data2,aes(x = Month,y = Value)) + 
  geom_point(aes(colour = Value), position="jitter") +
  scale_colour_gradient2(low = "#7cd0f0", high = "#067fae", na.value = "white") + 
  geom_smooth(color = "#96baf0", aes(group = 1)) +
  scale_y_continuous(limits = c(45,240), breaks = seq(0,240,20)) +
  ggtitle ("Average Daily PM 2.5") +
  xlab("") +  ylab ("PM 2.5  microgram/m3") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

rm(list=ls())


###################################
# How are wind and temperature
# correlated with future pollution?
###################################

#Load in the dataset with scraped weather data
load("beijing_air_cleaned.dta")

#Reduce the dataset to the variables we want
air <- air[,c("Rain", "Temp_12_00_PM", "average.d1", "Conditions_12_00_PM")]
air <- na.omit(air)
air$Rain <- as.factor(air$Rain)

#Create a cold and warm days variable
air$Temp_Median <- "Cold Days"
air$Temp_Median[which(air$Temp_12_00_PM > median(air$Temp_12_00_PM, na.rm=T))] <- "Hot Days"
air$Temp_Median <- as.factor(air$Temp_Median)

#Setting the colors for ggplot
myColors <- c("#f5a6a7", "#e72b2d")
names(myColors) <- levels(air$Temp_Median)

pdf("wind_temp_pollution.pdf")
  ggplot(air,aes(x = air$Wind_Speed_12_00_PM,y = air$average.d1)) + 
  scale_fill_manual(values = c("green", "blue")) +
  geom_jitter(alpha = I(1/2), aes(color = Temp_Median), position = position_jitter(w = 2, h = 2)) +
  scale_y_continuous(limits = c(45,240)) +
  scale_x_continuous(limits = c(0,30)) +
  ggtitle ("Tomorrow's Pollution Levels") + 
  xlab("Wind Speed Today") +  ylab ("PM 2.5  microgram/m3") + 
  scale_colour_manual(name = "grp",values = myColors) +
  geom_smooth( aes(group = Temp_Median, color=Temp_Median), alpha = I(1/1.8))
dev.off()






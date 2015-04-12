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
# Do hot, rainy days foretell 
# clear skies the next day?
###################################

#Load a different dataset I have already created and cleaned
load("beijing_air_cleaned.dta")

#Reduce the dataset down
air <- air[,c("Rain", "Temp_12_00_PM", "average.d1", "Conditions_12_00_PM")]
air <- na.omit(air)
air$Rain <- as.factor(air$Rain)

#The scatterplot is not very informative
ggplot(air, aes(x=Temp_12_00_PM, y=average.d1, color=Rain)) + geom_point(shape=1, position="jitter")

#A conditional tree plot makes a (slightly) clearer point

library(partykit)
tree <- ctree(average.d1~Rain+Temp_12_00_PM, 
              data=air)

#There appears to be some truth to the perception about rain and heat
pdf("rain_heat_pollution.pdf")
plot(tree, main="Beijing Air Pollution, Conditional Tree")
dev.off()

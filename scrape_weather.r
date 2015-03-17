#SCRAPING WEATHER DATA

#NOTE: merge_beijing.r must be run first

#Data from Weather Underground
#Event history by hour
#Location: Beijing Airport

#Pulls dates from the Beijing air quality data frame 
#Returns a csv file with historical hourly weather data

library(XML)
setwd("~/Dropbox/research/pollution/data/beijing/weather")

#Loop over all the days in the dataset
#Wunderground stores a historical webpage for each day
#The web page contains an hourly table of the weather conditions

for(i in 1:nrow(beijing)){
  
  #Read in the HTLM data tables from the day that row represents
  
  tables <- readHTMLTable(paste(as.character('http://www.wunderground.com/history/airport/ZBAA'), 
                                as.character(beijing$Year[i]),  
                                as.character(beijing$Month[i]),  
                                as.character(beijing$Day[i]), 
                                as.character('DailyHistory.html'), 
                                sep="/"))
  
  #Read in the specific table with hourly observations
  observations <- data.frame(tables["obsTable"])  
  
  #Create more sensible column names than in the HTML table
  names(observations) <- substr(names(observations), 10, nchar(names(observations)))
  
  #Eliminate redundant units of measurement (which export poorly to csv)
  observations$Temp. <- substr(observations$Temp., 0, nchar(as.character(observations$Temp.))-3)
  observations$Dew.Point <- substr(observations$Dew.Point, 0, nchar(as.character(observations$Dew.Point))-3)
  observations$Humidity <- substr(observations$Humidity, 0, nchar(as.character(observations$Humidity))-1)
  observations$Pressure <- substr(observations$Pressure, 0, nchar(as.character(observations$Pressure))-3)
  observations$Visibility <- substr(observations$Visibility, 0, nchar(as.character(observations$Visibility))-3)
  observations$Wind.Speed <- substr(observations$Wind.Speed, 0, nchar(as.character(observations$Wind.Speed))-4)
 
  #Write to csv file
  write.csv(observations, 
            file=paste(as.character('Beijing.Weather.Hourly'), 
                       as.character(beijing$Year[i]),
                       as.character(beijing$Month[i]),
                       as.character(beijing$Day[i]),
                       sep=".",
                       as.character("csv")))
  
  #Print progress report
  print(i/nrow(beijing))           
}
  
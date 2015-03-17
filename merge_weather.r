#MERGING WEATHER DATA WITH AIR QUALITY DATA

#NOTE: merge_beijing.r must be run first
#NOTE: scrape_weather.r must be run first OR files must already exist in the proper directory

library(stringr)

#Create list of filepaths. 
#You may need to alter the filepath
filepaths <- list.files(path = "~/Github/smog/data/beijing/weather", full.names = TRUE, recursive = TRUE)

#The dataset sometimes offers multiple observations for each hour
#And on some days,  observations by the half hour
#We want only one observation per hour
#Arbitrarily, we will keep the first
times <- paste0(as.character(rep(seq(from=1, to=12,), 2)), as.character(":00"), as.character(c(rep(" AM", 12), rep(" PM", 12))))

#Create an empty value to recieve data
weather <- NULL

#This loop will loop over the csv files
#Extract data from these files
#Reshape the data into a single row
#The product is a dataframe with a row for each day
#The columns represent hourly observations

i <- 812

for(i in 1:length(filepaths)){
  
  #Read in the file
  table <- read.csv(filepaths[i])
  
  #Remove observations made at half past the hour
  table <- table[table$Time..CST.%in% times, ]
  
  #Remove duplicates
  table <- table[!duplicated(table$Time..CST.),]
  
  #Subset on the variables we want to keep
  #The other variables have large amounts of missing data or duplicate information
  table <- table[, c("Time..CST.", "Temp.", "Dew.Point", "Humidity", "Pressure", "Visibility", "Wind.Dir", "Wind.Speed", "Conditions")]
  
  #Add rows of NAs for missing hours
  if(nrow(table)!=24){
    missing <- times[which( ! times %in%  table$Time..CST.)]
    for(j in 1:length(missing)){
      levels(table$Time..CST.) <- c(levels(table$Time..CST.), as.character(missing[j]))
      newrow <- c(as.character(missing[j]), rep("NA", ncol(table)-1))
      suppressWarnings(table <- rbind(table, newrow))
    } 
  }
  
  #Melt the data frame
  melted <- suppressWarnings(melt(table, id.vars="Time..CST."))

  #Create an id variable for the recasting process
  melted <- data.frame(cbind(melted, "id"=1))
  
  #Recast as one row
  table <- dcast(melted,  id~ Time..CST. +variable )

  #Eliminate extraneous id variable created for the transformation
  table <- table[, !(names(table) %in% "id")]
  
  #Extract date of the observations from the file name
  table$Year <- str_extract_all(filepaths[i],"\\(?[0-9]+\\)?")[[1]][1]
  table$Month <- str_extract_all(filepaths[i],"\\(?[0-9]+\\)?")[[1]][2]
  table$Day <- str_extract_all(filepaths[i],"\\(?[0-9]+\\)?")[[1]][3]

  #Send to the appropriate row in the weather table
  if(i==1){weather <- table}
  if(i>1){weather[i,] <- table}

  #This loop will likely take a minute or two on a laptop
}

#Clean up the workspace a little
rm(melted, table, filepaths, i, j, times, missing, newrow)

#Create a date
beijing$Date <- paste(beijing$Year, beijing$Month, beijing$Day, sep="_")
weather$Date <- paste(weather$Year, weather$Month, weather$Day, sep="_")

#Now let's merge with the dataset of air quality obvservations
air <- merge(beijing, weather, by="Date")

rm(beijing, weather)

write.csv(air, "beijing_air_weather.csv")

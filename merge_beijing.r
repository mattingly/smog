setwd("~/Dropbox/research/pollution")

#A fact sheet with definitions and metadata for this dataset can be found at http://www.stateair.net/web/historical/1/1.html.
#The U.S. Department of State Data Use Statement at http://www.stateair.net/web/historical/1/1.html applies to these and all data available from the Mission China air quality monitoring program.

#Read in data
beijing.2008 <- read.csv("./data/beijing/Beijing_2008_HourlyPM25.csv")
beijing.2009 <- read.csv("./data/beijing/Beijing_2009_HourlyPM25.csv")
beijing.2010 <- read.csv("./data/beijing/Beijing_2010_HourlyPM25.csv")
beijing.2011 <- read.csv("./data/beijing/Beijing_2011_HourlyPM25.csv")
beijing.2012 <- read.csv("./data/beijing/Beijing_2012_HourlyPM25.csv")
beijing.2013 <- read.csv("./data/beijing/Beijing_2013_HourlyPM25.csv")
beijing.2014 <- read.csv("./data/beijing/Beijing_2014_HourlyPM25.csv")

#Standardize column names
names(beijing.2009) <- names(beijing.2008)
names(beijing.2010) <- names(beijing.2008)
names(beijing.2011) <- names(beijing.2008)
names(beijing.2012) <- names(beijing.2008)
names(beijing.2013) <- names(beijing.2008)
names(beijing.2014) <- names(beijing.2008)

#Fix daylight savings time glitch in data
beijing.2009$Hour[which(duplicated(beijing.2009$Date)==TRUE)-1] <- 2
beijing.2010$Hour[which(duplicated(beijing.2010$Date)==TRUE)-1] <- 2
beijing.2011$Hour[which(duplicated(beijing.2011$Date)==TRUE)-1] <- 2
beijing.2014$Hour[which(duplicated(beijing.2014$Date)==TRUE)-1] <- 2

#Combine into one file
beijing <- rbind(beijing.2008, beijing.2009, beijing.2010, beijing.2011, beijing.2012, beijing.2013, beijing.2014)
rm(beijing.2008, beijing.2009, beijing.2010, beijing.2011, beijing.2012, beijing.2013, beijing.2014)

#Remove columns with no unique information
remove <- c("Date..LST.","Parameter", "Duration", "Site", "QC.Name")
beijing <- beijing[,!(names(beijing) %in% remove)]
rm(remove)

#Recast to be a daily dataset
require(reshape2)
beijing <- dcast(beijing, Year + Month + Day ~ Hour, value.var="Value")
head(beijing)

#Standardize NAs
beijing[beijing==-999] <- NA



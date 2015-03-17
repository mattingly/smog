#MERGING WEATHER DATA WITH AIR QUALITY DATA

#NOTE: merge_beijing.r must be run first
#NOTE: scrape_weather.r must be run first OR files must already exist

#Create list of filepaths
filepaths <- list.files(path = "~/Dropbox/research/pollution/data/beijing/weather", full.names = TRUE, recursive = TRUE)

i <- 1

weather <- read.csv(filepaths[i])

names(weather)


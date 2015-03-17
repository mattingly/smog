# Smog

This contains files to scrape historical and current data on smog and weather conditions in China. 

It also contains draft R scripts used to make VERY preliminary predictions about future fine particulate matter (PM 2.5) levels in Beijing and other cities. It bears repeating that this is a work in progress!

Run the files in this order:

1. merge_beijing.r: Merges hourly data from 2008 to 2014 on PM 2.5 readings from the U.S. Embassy in Beijing.
2. scrape_weather.r: Scrapes hourly weather data from the web to csv files.
3. merge_weather.r: Merges weather data from csv files to the Beijing air quality dataset.
4. predict_air_pm25.r: Uses the super learner algorithm to generate future air quality predictions based on historical data.

# Smog

This contains files to scrape historical and current data on smog and weather conditions in China. 

It also contains draft R scripts used to make VERY preliminary predictions about future fine particulate matter (PM 2.5) levels in Beijing and other cities. 

The prediction relies on a combination of random forests, regression trees, generalized linear models, and regression splines. It currently forecasts smog with reasonable accuracy (that is, within 25 micograms per cubic meter of the actual average daily value) about 75 percent of the time.

Run the files in this order:

1. merge_beijing.r: Merges hourly data from 2008 to 2014 on PM 2.5 readings from the U.S. Embassy in Beijing.
2. scrape_weather.r: Scrapes hourly weather data from the web to csv files.
3. merge_weather.r: Merges weather data from csv files to the Beijing air quality dataset.
4. predict_air_pm25.r: Uses the super learner algorithm to generate future air quality predictions based on historical data.

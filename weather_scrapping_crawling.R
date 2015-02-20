#Scraping weather data in R

##Using rvest

#install and load the necessary packages

library(rvest)
library(plyr)
library(magrittr)

#test to see if the packages loaded correctly

lego_movie <- html("http://www.imdb.com/title/tt1490017/")

rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating

#scrape all data that is a .wx-value from the weather table for Cherry Hill, NJ for February 17, 2015
CH_weather_test <- html("http://www.wunderground.com/history/airport/KVAY/2015/2/17/DailyHistory.html?req_city=Cherry+Hill&req_state=NJ&req_statename=New+Jersey&reqdb.zip=08002&reqdb.magic=1&reqdb.wmo=99999")

ave_temp <- CH_weather_test %>% 
    html_nodes(".wx-value") %>%
    html_text() %>%
    as.numeric()
ave_temp

#scrape just the averge temperature for Cherry Hill, NJ for February 17, 2015 (extract2 is an alias for [[]])
CH_weather_test <- html("http://www.wunderground.com/history/airport/KVAY/2015/2/17/DailyHistory.html?req_city=Cherry+Hill&req_state=NJ&req_statename=New+Jersey&reqdb.zip=08002&reqdb.magic=1&reqdb.wmo=99999")

ave_temp <- CH_weather_test %>% 
    html_nodes(".wx-value") %>%
    extract2(1) %>%
	html_text() %>%
    as.numeric()
ave_temp

##Create a set of urls for a series of dates at Weather Underground
#set date range
dates <- seq(as.Date("2007-05-20"), as.Date("2015-02-17"), by = "day")

#Format dates

datez <- format(dates, "%m/%d/%Y")

#Construct urls

urlz <- paste0("http://www.wunderground.com/history/airport/KVAY/", datez, "/DailyHistory.html?req_city=Cherry+Hill&req_state=NJ&req_statename=New+Jersey&reqdb.zip=08002&reqdb.magic=1&reqdb.wmo=99999&format=1")

#Use plyr to read each url and combine

df <- plyr::ldply(urlz)

#rename url column to temp_urls
colnames(df)<-"temp_urls"

##Use the weatherData package to scrape data from Weather Underground from 5-20-2007 through 2-18-2015 (note: appears to be limited to 380 days of data at a time)

#load packages
library(weatherData)

#create data frames for dates of interest, by year
yr_2007 <- getSummarizedWeather("KVAY", "2007-05-20", "2007-12-31")
yr_2008 <- getSummarizedWeather("KVAY", "2008-01-01", "2008-12-31")
yr_2009 <- getSummarizedWeather("KVAY", "2009-01-01", "2009-12-31")
yr_2010 <- getSummarizedWeather("KVAY", "2010-01-01", "2010-12-31")
yr_2011 <- getSummarizedWeather("KVAY", "2011-01-01", "2011-12-31")
yr_2012 <- getSummarizedWeather("KVAY", "2012-01-01", "2012-12-31")
yr_2013 <- getSummarizedWeather("KVAY", "2013-01-01", "2013-12-31")
yr_2014 <- getSummarizedWeather("KVAY", "2014-01-01", "2014-12-31")
yr_2015 <- getSummarizedWeather("KVAY", "2015-01-01", "2015-02-18")

#combine all data frames into a single data frame
all_dates<-rbind(yr_2007,yr_2008, yr_2009, yr_2010, yr_2011, yr_2012, yr_2013, yr_2014, yr_2015)

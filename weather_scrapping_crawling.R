#Crawling and scrappig weather data in R

#install and load the necessary packages

library(rvest)
library(plyr)

#test to see if the packages loaded correctly - 7.X should print

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

#set date range
dates <- seq(as.Date("2007-05-20"), as.Date("2015-02-17"), by = "day")

#Format dates

datez <- format(dates, "%m/%d/%Y")

#Construct urls

urlz <- paste0("http://www.wunderground.com/history/airport/KVAY/", datez, "/DailyHistory.html?req_city=Cherry+Hill&req_state=NJ&req_statename=New+Jersey&reqdb.zip=08002&reqdb.magic=1&reqdb.wmo=99999&format=1")

#Then use a little plyr magic to read each url and combine

df <- plyr::ldply(urlz)


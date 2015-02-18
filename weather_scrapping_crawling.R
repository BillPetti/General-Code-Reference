#Crawling and scrappig weather data in R

#install and load rvest from github as well as other necessary packages

library(devtools)
library(magrittr)
install_github("hadley/rvest")
library(rvest)

#test to see if the packages loaded correctly - 7.X should print

lego_movie <- html("http://www.imdb.com/title/tt1490017/")

rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating

#scrape all data from the weather table for Cherry Hill, NJ for February 17, 2015
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


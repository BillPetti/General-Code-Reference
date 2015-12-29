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
library(weatherData) #to scrape daily weather data from Weather Underground
library(sqldf) #to run SQL queries on the date
library(plyr) #to reshape the data

#create data frames for dates of interest, by year
yr_2007 <- getSummarizedWeather("KVAY", "2007-05-20", "2007-12-31")
yr_2008 <- getSummarizedWeather("KVAY", "2008-01-01", "2008-12-31")
yr_2009 <- getSummarizedWeather("KVAY", "2009-01-01", "2009-12-31")
yr_2010 <- getSummarizedWeather("KVAY", "2010-01-01", "2010-12-31")
yr_2011 <- getSummarizedWeather("KVAY", "2011-01-01", "2011-12-31")
yr_2012 <- getSummarizedWeather("KVAY", "2012-01-01", "2012-12-31")
yr_2013 <- getSummarizedWeather("KVAY", "2013-01-01", "2013-12-31")
yr_2014 <- getSummarizedWeather("KVAY", "2014-01-01", "2014-12-31")
yr_2015 <- getSummarizedWeather("KVAY", "2015-01-01", Sys.Date())

#combine all data frames into a single data frame
all_dates<-rbind(yr_2007,yr_2008, yr_2009, yr_2010, yr_2011, yr_2012, yr_2013, yr_2014, yr_2015)

#sort all_dates by average temperature, ascending
sort<-arrange(all_dates, Mean_TemperatureF)

#reshape Date variable for querying
all_dates$Month<-month(all_dates$Date)
all_dates$Day<-day(all_dates$Date)
all_dates$Year<-year(all_dates$Date) #three seperate numeric variables that can then be combined into one "new date" variable

#create new table with Month, Day, Year split out and removing POSIXlt date column
all_dates_2<-select(all_dates, Month, Day, Year, Max_TemperatureF, Mean_TemperatureF, Min_TemperatureF)

all_dates$NewDate<-paste(all_dates_2$Month,all_dates_2$Day,sep='')
all_dates$NewDate<-paste(all_dates_2$NewDate,all_dates_2$Year,sep='') #single, numeric new date variable

### Another approach

library(XML)
library(dplyr)
library(ggplot2)

# test <- readHTMLTable("http://www.wunderground.com/history/airport/KVAY/2015/12/23/DailyHistory.html?req_city=Cherry+Hill&req_state=NJ&req_statename=New+Jersey&reqdb.zip=08002&reqdb.magic=1&reqdb.wmo=99999")
# 
# test[1]
# 
# test1 <- as.data.frame(test[1])
# names(test1) <- c("Element", "Actual", "Average", "Records")
# test1 <- select(test1, Element, Actual) %>% filter(Element == "Mean Temperature")

# create grid for station, year, and day values

# stations <- c("K12N", "KACY", "KBLM", "KCDW", "KNEL", "KMIV", "KMMU", "KVAY", "KEWR", "KSMQ", "KTEB", "KTTN", "KWWD", "KWRI")

stations <- "KACY"

years <- c(1945:2014)

days <- c(23, 24, 25, 26)

station_yrs <- expand.grid(stations, years, days)
names(station_yrs) <- c("station", "year", "day")

# create individual urls based on years and weather station

# urls <- paste0("http://www.wunderground.com/history/airport/",station_yrs$station,"/",station_yrs$year, "/12/",station_yrs$day, "/DailyHistory.html")

# urls <- as.data.frame(urls)

do_scrape <- function(x, y, z) {
  url <- paste0("http://www.wunderground.com/history/airport/",x,"/",y, "/12/",z, "/DailyHistory.html")
  scrape <- readHTMLTable(url, stringsAsFactors = FALSE)
  x <- as.data.frame(scrape[1])
  x
}

results <- station_yrs %>% group_by(station, year, day) %>% do(do_scrape(.$station, .$year, .$day))
results <- results[,c(1:7)]
names(results) <- c("station", "year", "day", "element", "actual", "average", "record")
reduced <- filter(results, element == "Mean Temperature" | element == "Max Temperature")
reduced$date <- with(reduced, paste0("12-", day, "-", year))
reduced$date <- as.Date(reduced$date, "%m-%d-%Y")
reduced <- ungroup(reduced)
Encoding(reduced$actual) <- "bytes"
reduced$actual <- gsub('\xc2\xa0\xc2\xb0F', '' , reduced$actual)
reduced$actual <- as.numeric(reduced$actual)

#2015 data

stations2015 <- "KACY"

years2015 <- 2015

days2015 <- c(23, 24, 25, 26)

station_yrs_2015 <- expand.grid(stations2015, years2015, days2015)
names(station_yrs_2015) <- c("station", "year", "day")

results2015 <- station_yrs_2015 %>% group_by(station, year, day) %>% do(do_scrape(.$station, .$year, .$day))
results2015 <- results2015[,c(1:7)]
names(results2015) <- c("station", "year", "day", "element", "actual", "average", "record")
reduced2015 <- filter(results2015, element == "Mean Temperature" | element == "Max Temperature")
reduced2015$date <- with(reduced2015, paste0("12-", day, "-", year))
reduced2015$date <- as.Date(reduced2015$date, "%m-%d-%Y")
reduced2015 <- ungroup(reduced2015)
Encoding(reduced2015$actual) <- "bytes"
reduced2015$actual <- gsub('\xc2\xa0\xc2\xb0F', '' , reduced2015$actual)
reduced2015$actual <- as.numeric(reduced2015$actual)

# plot results

data <- filter(reduced, element == "Mean Temperature")
data$day <- factor(data$day, labels = c("Dec 23rd", "Dec 24th", "Dec 25th", "Dec 26th"))
data_max <- data %>% group_by(day) %>% filter(actual == max(actual))
data_2015 <- filter(reduced2015, element == "Mean Temperature")
data_2015$day <- factor(data_2015$day, labels = c("Dec 23rd", "Dec 24th", "Dec 25th", "Dec 26th"))

q <- ggplot(data, aes(year, actual)) + geom_point(fill="grey50", colour = "black", size = 4.2, pch = 21, alpha = .5) + geom_smooth() + facet_grid(~day) + xlab("Year") + ylab("Average Daily Temperature in Fahrenheit (Year)") + geom_point(data = data_max, fill="red", colour = "black", size = 4.2, pch = 21) + geom_point(data = data_2015, fill="green", colour = "black", size = 4.2, pch = 21) + ggtitle("Average Daily Temperature, December 23-26, 1945-2015: \nSouth Jersey Regional Airport (Lumberton, New Jersey) Weather Station") + theme_bw() + theme(text = element_text(family = "Georgia", face = "bold"), plot.title = element_text(color = "black", size = 16)) + geom_text(data = data_max, aes(label=paste0(actual,"\n(", year, ")")), family = "Georgia", fontface = 2, size = 4.5, hjust=0.7, vjust=-0.35) + geom_text(data = data_2015, aes(label=actual), family = "Georgia", fontface = 2, size = 4.5, hjust=0.5, vjust=-1)

q

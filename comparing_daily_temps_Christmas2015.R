############################
# Comparing Daily Temperatures Over Time
# Scraping and Plotting data
# Bill Petti
# December 27, 2015
# Session information
# R version 3.2.2 (2015-08-14)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.11.2 (El Capitan)
#
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# loaded via a namespace (and not attached):
#  [1] labeling_0.3     MASS_7.3-43      colorspace_1.2-6 scales_0.3.0     magrittr_1.5     plyr_1.8.3
#  [7] tools_3.2.2      gtable_0.1.2     reshape2_1.4.1   Rcpp_0.12.1      ggplot2_1.0.1    stringi_0.5-5
# [13] grid_3.2.2       stringr_1.0.0    digest_0.6.8     proto_0.3-10     munsell_0.4.2
############################

# load the necessary packages

library(XML) # for scraping
library(dplyr) # for data manipulation
library(ggplot2) # for plotting

# create values for the elements of the url we need to vary; the station, the years, and the days of the month

stations <- "KACY"

years <- c(1945:2014)

days <- c(23, 24, 25, 26)

# create a full grid with the three elements above

station_yrs <- expand.grid(stations, years, days)
names(station_yrs) <- c("station", "year", "day")

# create a custom function for construction individual urls for every date, scraping the data, and then save the relevant html table as a data frame 

do_scrape <- function(x, y, z) {
  url <- paste0("http://www.wunderground.com/history/airport/",x,"/",y, "/12/",z, "/DailyHistory.html")
  scrape <- readHTMLTable(url, stringsAsFactors = FALSE)
  x <- as.data.frame(scrape[1])
  x
}

# run the do_scrape function, looping it over each combination of station, year, and day using do()

results <- station_yrs %>% group_by(station, year, day) %>% do(do_scrape(.$station, .$year, .$day))
results <- results[,c(1:7)]
names(results) <- c("station", "year", "day", "element", "actual", "average", "record")
reduced <- filter(results, element == "Mean Temperature")
reduced$date <- with(reduced, paste0("12-", day, "-", year))
reduced$date <- as.Date(reduced$date, "%m-%d-%Y")
reduced <- ungroup(reduced)
Encoding(reduced$actual) <- "bytes"
reduced$actual <- gsub('\xc2\xa0\xc2\xb0F', '' , reduced$actual)
reduced$actual <- as.numeric(reduced$actual)

# plot the initial data

data <- filter(reduced, element == "Mean Temperature")
data$day <- factor(data$day, labels = c("Dec 23rd", "Dec 24th", "Dec 25th", "Dec 26th")) # for faceting

p <- ggplot(data, aes(year, actual)) + geom_point(fill="grey50", colour = "black", size = 4.2, pch = 21, alpha = .5) + geom_smooth() + facet_grid(~day) + xlab("Year") + ylab("Average Daily Temperature (°F)") + ggtitle("Average Daily Temperature, December 23-26, 1945-2015: \nAtlantic City International Airport Weather Station") + theme_bw() + theme(text = element_text(family = "Georgia", face = "bold"), plot.title = element_text(color = "black", size = 16))

p

data_max <- data %>% group_by(day) %>% filter(actual == max(actual))
p <- p + geom_point(data = data_max, fill="red", colour = "black", size = 4.2, pch = 21) + geom_text(data = data_max, aes(label=paste0(actual,"\n(", year, ")")), family = "Georgia", fontface = 2, size = 4.5, hjust=0.7, vjust=-0.35)

#2015 data

stations2015 <- "KACY"

years2015 <- 2015

days2015 <- c(23, 24, 25, 26)

station_yrs_2015 <- expand.grid(stations2015, years2015, days2015)
names(station_yrs_2015) <- c("station", "year", "day")

results2015 <- station_yrs_2015 %>% group_by(station, year, day) %>% do(do_scrape(.$station, .$year, .$day))
results2015 <- results2015[,c(1:7)]
names(results2015) <- c("station", "year", "day", "element", "actual", "average", "record")
reduced2015 <- filter(results2015, element == "Mean Temperature")
reduced2015$date <- with(reduced2015, paste0("12-", day, "-", year))
reduced2015$date <- as.Date(reduced2015$date, "%m-%d-%Y")
reduced2015 <- ungroup(reduced2015)
Encoding(reduced2015$actual) <- "bytes"
reduced2015$actual <- gsub('\xc2\xa0\xc2\xb0F', '' , reduced2015$actual)
reduced2015$actual <- as.numeric(reduced2015$actual)

# plot results

data_2015 <- filter(reduced2015, element == "Mean Temperature")
data_2015$day <- factor(data_2015$day, labels = c("Dec 23rd", "Dec 24th", "Dec 25th", "Dec 26th"))

q <- p + geom_point(data = data_2015, fill="green", colour = "black", size = 4.2, pch = 21) + geom_text(data = data_2015, aes(label=actual), family = "Georgia", fontface = 2, size = 4.5, hjust=0.5, vjust=-1)

q

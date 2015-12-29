## Comparing Daily Average Temperatures Over Time

I live in the South Jersey area and we ended up with an oddly warm Christmas. The warm temperatures started well before Christmas and continued straight through the holiday. It wasn't just warmer than normal, but five to ten degrees warmer. It got me curious about whether the temperatures leading up to and through Christmas were the highest on record, and by how much.

I decided to scrape data from [Weather Underground](http://www.wunderground.com/history/) for each individual date from Decmeber 23rd through December 26th, going back as far as I could given what's available.

Weather Underground's historical archive contains daily information for every year back to 1945. Here is an example from the nearest airport weather station to my city, which is the South Jersey Regional Airport in Lumberton, NJ. Here is what the historical archive produces for December 25, 1998:

![alt text] (https://github.com/BillPetti/General-Code-Reference/blob/master/images/weatherunderground.screenshot.png?raw=true)

In order to scrape the data for each day and year we need to examine the url in more detail. Here's the url for the screen shot above:

http://www.wunderground.com/history/airport/KVAY/1998/12/25/DailyHistory.html?req_city=South+Jersey+Regional&req_state=NJ&req_statename=New+Jersey&reqdb.zip=08048&reqdb.magic=5&reqdb.wmo=99999

The history urls are set up in a very user friendly way, where the only elements one has to vary are the location or airport code (KVAY for the url above), and the year, month, and day of the date you want to view.

We can easily set up a loop in R to scrape the archived information for each date.

First, we need to create a grid with the various values needed to construct the urls for each date. We will start with data through 2014, looking for the previous daily averages. Upon examination of the archives I found that the KVAY station doesn't have actual data going back all that far, however the Atlantic City International Airport does, so I used that airport code (KACY):

```r 
stations <- "KACY"
years <- c(1945:2014)
days <- c(23, 24, 25, 26)
station_yrs <- expand.grid(stations, years, days)
names(station_yrs) <- c("station", "year", "day")
```
![alt text] (https://github.com/BillPetti/General-Code-Reference/blob/master/images/Screen%20Shot%202015-12-29%20at%208.31.03%20AM.png?raw=true)

Next, we create a custom function that will construct the urls, scrape and retain the relevant data, and save the data as a data frame:

```r
do_scrape <- function(x, y, z) {
  url <- paste0("http://www.wunderground.com/history/airport/",x,"/",y, "/12/",z, "/DailyHistory.html")
  scrape <- readHTMLTable(url, stringsAsFactors = FALSE)
  x <- as.data.frame(scrape[1])
  x
}
```

The x, y, and z variables in the function correspond to the station code, the year, and the day, respectively. We only want to keep the first table, since that is where the mean temperature can be found.

Run the do_scrape function while looping over all the combinations found in the grid we created above. We just want to keep the mean temperature, so we can use dplyr to filter for only rows that contain those values:

```r 
results <- station_yrs %>% group_by(station, year, day) %>% do(do_scrape(.$station, .$year, .$day))
results <- results[,c(1:7)]
names(results) <- c("station", "year", "day", "element", "actual", "average", "record")
reduced <- filter(results, element == "Mean Temperature")
reduced$date <- with(reduced, paste0("12-", day, "-", year))
reduced$date <- as.Date(reduced$date, "%m-%d-%Y")
reduced <- ungroup(reduced)
```
![alt text] (https://github.com/BillPetti/General-Code-Reference/blob/master/images/Screen%20Shot%202015-12-29%20at%208.38.16%20AM.png?raw=true)

Now, since we directly scraped the data we have some special characters mixed in with our temperature variables. We need to strip that data out before we transform the `actual` variable to numeric. We can strip out the unecessary characters by converting the encoding of the `actual` variable to "bytes" and then removing the string that results:

```r
Encoding(reduced$actual) <- "bytes"
reduced$actual <- gsub('\xc2\xa0\xc2\xb0F', '' , reduced$actual)
reduced$actual <- as.numeric(reduced$actual)
```
We can now plot the data. 

I am curious about how the mean temperature for each day has trended over time. I decided to facet the data by day of the month, which makes it easier to see how each daily average compares with previous years.

```r
data <- filter(reduced, element == "Mean Temperature")
data$day <- factor(data$day, labels = c("Dec 23rd", "Dec 24th", "Dec 25th", "Dec 26th")) # for faceting

p <- ggplot(data, aes(year, actual)) + geom_point(fill="grey50", colour = "black", size = 4.2, pch = 21, alpha = .5) + geom_smooth() + facet_grid(~day) + xlab("Year") + ylab("Average Daily Temperature (°F)") + ggtitle("Average Daily Temperature, December 23-26, 1945-2015: \nAtlantic City International Airport Weather Station") + theme_bw() + theme(text = element_text(family = "Georgia", face = "bold"), plot.title = element_text(color = "black", size = 16))
```
![alt text](https://github.com/BillPetti/General-Code-Reference/blob/master/images/prev.png?raw=true)

We can see that, in general, the average temperature for these days has been increasing over time. Not great if you enjoy a white Christmas. Visually, it's hard to immediately see where the record temperatures are. We can add a separate series to our plot that highlights the max average temperatures for each day of the month in a different color (along with some handy labels):

```r
data_max <- data %>% group_by(day) %>% filter(actual == max(actual))
p <- p + geom_point(data = data_max, fill="red", colour = "black", size = 4.2, pch = 21) + geom_text(data = data_max, aes(label=paste0(actual,"\n(", year, ")")), family = "Georgia", fontface = 2, size = 4.5, hjust=0.7, vjust=-0.35)
```
![alt text](https://github.com/BillPetti/General-Code-Reference/blob/master/images/pre_max.png?raw=true)

Now we have the previous daily average records highlighted in red and can easily see the temperature and the year. (Don't worry about those labels being cut off for now, we'll fix that next.)

The last thing we need to do is pull in our data for 2015 and then add it to our plot for comparison. We repeat the same steps as above except that we restrict the year to 2015, creating a data frame `reduced_2015`. Now to plot it, highlighting the current year's data in green:

```r
data_2015 <- filter(reduced2015, element == "Mean Temperature")
data_2015$day <- factor(data_2015$day, labels = c("Dec 23rd", "Dec 24th", "Dec 25th", "Dec 26th"))

q <- p + geom_point(data = data_2015, fill="green", colour = "black", size = 4.2, pch = 21) + geom_text(data = data_2015, aes(label=actual), family = "Georgia", fontface = 2, size = 4.5, hjust=0.5, vjust=-1)
```
![alt text](https://github.com/BillPetti/General-Code-Reference/blob/master/images/hist_ave_temps.png?raw=true)

We can see that this year saw record highs for December 23-25, and these highs beat the previous records by sizeable amounts. We managed to dip below the historical record for December 26th, but just by two degrees. In reality, this December 26th was tied for the fifth-highest since 1945.

The complete r code for scraping the data and creating the visualization can be found [here](https://github.com/BillPetti/General-Code-Reference/blob/master/comparing_daily_temps_Christmas2015.R)

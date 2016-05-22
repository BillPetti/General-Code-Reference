# function for creatng a boxscore for pitcher stats from MLBAM's Game Day application using the  a game's boxscore.xml

require(XML)

pitcher_boxscore <- function(x) {
  url <- x
  box <- xmlParse(url)
  xml_data <- xmlToList(box)
  end <- length(xml_data[[2]]) - 1
  x <- seq(1:end)
  away_pitchers <- lapply(xml_data[[2]][x], function(x) 
    as.data.frame.list(x, stringsAsFactors=FALSE)) %>%
    bind_rows()
  away_pitchers$team <- xml_data[[8]][6]
  h_end <- length(xml_data[[4]]) - 1
  h_x <- seq(1:h_end)
  home_pitchers <- lapply(xml_data[[4]][h_x], function(x) 
    as.data.frame.list(x, stringsAsFactors=FALSE)) %>%
    bind_rows()
  home_pitchers$team <- xml_data[[8]][7]
  names <- colnames(away_pitchers)
  names[28] <- "win_loss"
  names(away_pitchers) <- names
  names(home_pitchers) <- names
  pitchers <- rbind(away_pitchers, home_pitchers)
  pitchers
}

test <- pitcher_boxscore("http://gd2.mlb.com/components/game/mlb/year_2016/month_05/day_21/gid_2016_05_21_milmlb_nynmlb_1/boxscore.xml")

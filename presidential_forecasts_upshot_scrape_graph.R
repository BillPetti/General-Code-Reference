pres_forecasts <- function() {
  require(magrittr)
  require(rvest)
  require(scales)
  require(stringr)
  require(tidyverse)

  # load custom ggplot2 theme

  source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

  # scrape data

  url <- "http://www.nytimes.com/interactive/2016/upshot/presidential-polls-forecast.html?rref=collection%2Fsectioncollection%2Fupshot&action=click&contentCollection=upshot&region=rank&module=package&version=highlights&contentPlacement=2&pgtype=sectionfront&_r=2"

  data <- read_html(url) %>% 
    html_nodes("table") %>%
    .[[2]] %>%
    html_table(fill = TRUE)

  # format data

  names(data) <- data[1,]
  
  data <- data[-1, -c(12:13)]

  data_melt <- gather(data = data, key = poll_state, value = chance, -State, -E.V.)
  
  data_melt$E.V. %<>% as.numeric()
  
  data_melt$probability_win <- as.numeric(str_extract_all(data_melt$chance, "([0-9][0-9])", simplify = TRUE))/100
  
  data_melt$party_dem <- str_extract_all(string = data_melt$chance, pattern = "Dem", simplify = TRUE) %>%
    as.character()
  
  data_melt$party_rep <- str_extract_all(string = data_melt$chance, pattern = "Rep", simplify = TRUE) %>%
    as.character()
  
  data_melt <- data_melt %>% 
    mutate(party = ifelse(.$party_dem == "Dem", "Dem", .$party_rep)) %>%
    select(-party_rep, -party_dem)
  
  data_melt$prob_dem_win <- with(data_melt, ifelse(party == "Dem", probability_win, 1 - probability_win))
  
  data_melt$prob_rep_win <- 1 - data_melt$prob_dem_win
  
  cast_data_dem <- data_melt %>%
    filter(!is.na(prob_dem_win)) %>%
    select(-party, -probability_win, -chance, -prob_rep_win) %>%
    spread(key = poll_state, value = prob_dem_win) %>%
    mutate(straight_average = round(rowMeans(.[3:8], na.rm = TRUE), 3), party_code = "Democrat")
  
  cast_data_rep <- data_melt %>%
    filter(!is.na(prob_dem_win)) %>%
    select(-party, -probability_win, -chance, -prob_dem_win) %>%
    spread(key = poll_state, value = prob_rep_win) %>%
    mutate(straight_average = round(rowMeans(.[3:8], na.rm = TRUE), 3), party_code = "Republican")
  
  votes_by_prob_dem <- cast_data_dem %>% 
    group_by(party_code, straight_average) %>% 
    summarise(electoral_votes = sum(`E.V.`)) %>%
    arrange(desc(straight_average)) %>%
    mutate(cummulative_ev = cumsum(.$electoral_votes))
  
  votes_by_prob_rep <- cast_data_rep %>% 
    group_by(party_code, straight_average) %>% 
    summarise(electoral_votes = sum(`E.V.`)) %>%
    arrange(desc(straight_average)) %>%
    mutate(cummulative_ev = cumsum(.$electoral_votes))
  
  bind_party_prob <- rbind(votes_by_prob_dem, votes_by_prob_rep) %>%
    ungroup()
  
  bind_party_prob$date <- Sys.Date()
  
  data_melt$date <- Sys.Date()
  
  # write out data
  
  write.csv(data_melt, paste0("all_presidential_forecasts_", Sys.Date(), ".csv"), row.names = FALSE)
  
  write.csv(bind_party_prob, paste0("binded_ave_forecasts_party_", Sys.Date(), ".csv"), row.names = FALSE)
  
  # plot the data
  
  party_palette <- c("Democrat" = "#0040ff", "Republican" = "#ff0000")
  
  # party's faceted 
  
  bind_party_prob %>%
    ggplot(aes(straight_average, cummulative_ev, group = 1)) + 
    geom_line(aes(color = party_code)) + 
    geom_point(aes(color = party_code), size = 3) +
    geom_hline(yintercept = 270, linetype = "dashed", color = "blue") + 
    facet_wrap(~party_code, scales = "free_x") +
    scale_x_reverse(labels = percent, breaks = seq(from = 0, to = 1, by = .1)) +
    scale_color_manual(values = party_palette, guide = FALSE) + 
    ggtitle(paste0("\nCummulative Electoral Votes by Probability of Party Win (State-Level): ", format(Sys.Date(), "%B %d, %Y"), "\n")) +
    xlab("\nProbability of Party Win (State-Level: Average of Major Forecasts)") +
    ylab("\nCummulative Electoral Votes\n") +
    #labs(subtitle = "This graph compares the cummulative electoral votes for each candidates based on the probability that the candidate will win each state", caption = "Data acquired from The Upshot (http://www.nytimes.com/interactive/2016/upshot/presidential-polls-forecast.html?_r=0)\nCreated by Bill Petti") + 
    theme_bp_grey() + 
    theme(axis.text.x = element_text(size = 8), plot.title = element_text(size = 22), strip.text.x = element_text(face = "bold", size = 14))
  
  ggsave(paste0("cummulative_ev_2016_facet_", Sys.Date(), ".png"), scale = 1.1, height = 8.5, width = 14, units = "in")
  
  # party's plotted together
  
  bind_party_prob %>%
    ggplot(aes(straight_average, cummulative_ev, group = 1)) + 
    geom_line(aes(color = party_code, group = party_code)) + 
    geom_point(aes(color = party_code, group = party_code), size = 3) +
    geom_hline(yintercept = 270, linetype = "dashed", color = "blue") + 
    #facet_wrap(~party_code, scales = "free_x") +
    scale_x_reverse(labels = percent, breaks = seq(from = 0, to = 1, by = .1)) +
    scale_color_manual(values = party_palette, "Party") + 
    ggtitle(paste0("\nCummulative Electoral Votes by Probability of Party Win (State-Level): ", format(Sys.Date(), "%B %d, %Y"), "\n")) +
    xlab("\nProbability of Party Win (State-Level: Average of Major Forecasts)") +
    ylab("\nCummulative Electoral Votes\n") +
    #labs(subtitle = "This graph compares the cummulative electoral votes for each candidates based on the probability that the candidate will win each state", caption = "Data acquired from The Upshot (http://www.nytimes.com/interactive/2016/upshot/presidential-polls-forecast.html?_r=0)\nCreated by Bill Petti") + 
    theme_bp_grey() + 
    theme(axis.text.x = element_text(size = 8), plot.title = element_text(size = 22), strip.text.x = element_text(face = "bold", size = 14))
  
  ggsave(paste0("cummulative_ev_2016_", Sys.Date(), ".png"), scale = 1.1, height = 8.5, width = 14, units = "in")
}

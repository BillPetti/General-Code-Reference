require(magrittr)
require(rvest)
require(scales)
require(stringr)
require(tidyverse)

# load custom ggplot2 theme

source("https://raw.githubusercontent.com/BillPetti/R-Plotting-Resources/master/theme_bp_grey")

# scrape data

url <- "http://www.nytimes.com/interactive/2016/upshot/presidential-polls-forecast.html?rref=collection%2Fsectioncollection%2Fupshot&action=click&contentCollection=upshot&region=rank&module=package&version=highlights&contentPlacement=2&pgtype=sectionfront&_r=2"

test <- read_html(url) %>% 
  html_nodes("table") %>%
  .[[2]] %>%
  html_table(fill = TRUE)

# format data

names(test) <- test[1,]

test <- test[-1, -c(12:13)]

test_melt <- gather(data = test, key = poll_state, value = chance, -State, -E.V.)

test_melt$E.V. %<>% as.numeric()

test_melt$probability_win <- as.numeric(str_extract_all(test_melt$chance, "([0-9][0-9])", simplify = TRUE))/100

test_melt$party_dem <- str_extract_all(string = test_melt$chance, pattern = "Dem", simplify = TRUE) %>%
  as.character()

test_melt$party_rep <- str_extract_all(string = test_melt$chance, pattern = "Rep", simplify = TRUE) %>%
  as.character()

test_melt <- test_melt %>% 
  mutate(party = ifelse(.$party_dem == "Dem", "Dem", .$party_rep)) %>%
  select(-party_rep, -party_dem)

test_melt$prob_dem_win <- with(test_melt, ifelse(party == "Dem", probability_win, 1 - probability_win))

test_melt$prob_rep_win <- 1 - test_melt$prob_dem_win

cast_test_dem <- test_melt %>%
  filter(!is.na(prob_dem_win)) %>%
  select(-party, -probability_win, -chance, -prob_rep_win) %>%
  spread(key = poll_state, value = prob_dem_win) %>%
  mutate(straight_average = round(rowMeans(.[3:8], na.rm = TRUE), 3), party_code = "Democrat")

cast_test_rep <- test_melt %>%
  filter(!is.na(prob_dem_win)) %>%
  select(-party, -probability_win, -chance, -prob_dem_win) %>%
  spread(key = poll_state, value = prob_rep_win) %>%
  mutate(straight_average = round(rowMeans(.[3:8], na.rm = TRUE), 3), party_code = "Republican")

votes_by_prob_dem <- cast_test_dem %>% 
  group_by(party_code, straight_average) %>% 
  summarise(electoral_votes = sum(`E.V.`)) %>%
  arrange(desc(straight_average)) %>%
  mutate(cummulative_ev = cumsum(.$electoral_votes))

votes_by_prob_rep <- cast_test_rep %>% 
  group_by(party_code, straight_average) %>% 
  summarise(electoral_votes = sum(`E.V.`)) %>%
  arrange(desc(straight_average)) %>%
  mutate(cummulative_ev = cumsum(.$electoral_votes))

bind_party_prob <- rbind(votes_by_prob_dem, votes_by_prob_rep) %>%
  ungroup()

# prob_levels <- unique(bind_party_prob$straight_average)
# 
# bind_party_prob$straight_average %<>% factor(levels = prob_levels) 
# 
# levels(bind_party_prob$straight_average)

bind_party_prob %>%
  ggplot(aes(straight_average, cummulative_ev, group = 1)) + 
  geom_line(color = "orange") + 
  geom_point(color = "orange", size = 3) +
  geom_hline(yintercept = 270, linetype = "dashed", color = "blue") + 
  facet_wrap(~party_code, scales = "free_x") +
  scale_x_reverse(labels = percent) +
  annotate("text", x = .99, y = 270, label = "270 Electoral Votes", hjust = -2, vjust = -.5) +
  ggtitle("\nCummulative Electoral Votes by Probability of Party Win (State-Level)\n") +
  xlab("\nProbability of Party Win (State-Level: Average of Major Forecasts)") + 
  ylab("\nCummulative Electoral Votes\n") +
  theme_bp_grey() + 
  theme(axis.text.x = element_text(size = 8), plot.title = element_text(size = 18), strip.text.x = element_text(face = "bold", size = 14))
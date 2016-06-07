# Maxpreps functions
# Offense

maxprep_batting_ind <- function(url) {
  list <- readHTMLTable(url)
  x1 <- as.data.frame(list[1])
  x2 <- as.data.frame(list[2])
  x3 <- as.data.frame(list[3])
  names(x1) <- c("number", "Athlete.Name", "GP", "Avg", "PA", "AB", "R", "H", "RBI", "2B", "3B", "HR", "GS")
  names(x2) <- c("number", "Athlete.Name", "GP", "SF", "SH.B", "BB", "K", "HBP", "ROE", "FC", "LOB", "OBP", "SLG", "OPS")
  names(x3) <- c("number", "Athlete.Name", "GP", "SB", "SBA")
  x <- left_join(x1, x2, by = "Athlete.Name") %>% left_join(x3, by = "Athlete.Name")
  x <- x[,-c(14:15, 27:28)]
  names(x) <- c("number", "Athlete.Name", "GP", "Avg", "PA", "AB", "R", "H", "RBI", "2B", "3B", "HR", "GS", "SF", "SH.B", "BB", "K", "HBP", "ROE", "FC", "LOB", "OBP", "SLG", "OPS", "SB", "SBA")
  x[] <- lapply(x, as.character)
  x$Team <- filter(baseball_var_lu_table, stats_url == url) %>% .$school
  x$city_state <- filter(baseball_var_lu_table, stats_url == url) %>% .$city_state
  x[,c(3:26)] %<>% lapply(as.numeric)
  x[is.na(x)] <- 0
  x$BABIP <- with(x, round((H-HR)/(AB-K-HR-SF),3))
  x$Kper <- with(x, round(K/PA, 3))
  x$BBper <- with(x, round(BB/PA, 3))
  x$SBper <- with(x, round(SB/SBA, 3))
  x <- select(x, Team, city_state, everything())
  x
}

maxprep_batting_team <- function(url) {
  list <- readHTMLTable(url)
  x1 <- as.data.frame(list[1])
  x2 <- as.data.frame(list[2])
  x3 <- as.data.frame(list[3])
  names(x1) <- c("number", "Athlete.Name", "GP", "Avg", "PA", "AB", "R", "H", "RBI", "2B", "3B", "HR", "GS")
  names(x2) <- c("number", "Athlete.Name", "GP", "SF", "SH.B", "BB", "K", "HBP", "ROE", "FC", "LOB", "OBP", "SLG", "OPS")
  names(x3) <- c("number", "Athlete.Name", "GP", "SB", "SBA")
  x <- left_join(x1, x2, by = "Athlete.Name") %>% left_join(x3, by = "Athlete.Name")
  x <- x[,-c(14:15, 27:28)]
  names(x) <- c("number", "Athlete.Name", "GP", "Avg", "PA", "AB", "R", "H", "RBI", "2B", "3B", "HR", "GS", "SF", "SH.B", "BB", "K", "HBP", "ROE", "FC", "LOB", "OBP", "SLG", "OPS", "SB", "SBA")
  x[] <- lapply(x, as.character)
  x[,c(3:26)] %<>% lapply(as.numeric)
  x[is.na(x)] <- 0

  x_team <- x[,-(1:2)] %>% summarise_each(funs(sum(.,na.rm = FALSE)))
  x_team$BABIP <- with(x_team, round((H-HR)/(AB-K-HR-SF),3))
  x_team$Avg <- with(x_team, round(H/AB,3))
  x_team$OBP <- with(x_team, round((H+BB+HBP)/(AB+BB+HBP+SF),3))
  x_team$SLG <- with(x_team, round(((H-`2B`-`3B`-HR) + (`2B`*2) + (`3B`*3) + (HR*4))/AB, 3))
  x_team$OPS <- with(x_team, OBP+SLG)
  x_team$Kper <- with(x_team, round(K/PA, 3))
  x_team$BBper <- with(x_team, round(BB/PA, 3))
  x_team$SBper <- with(x_team, round(SB/SBA, 3))
  x_team
}

# Pitching

maxprep_pitching_ind <- function(url) {
  list <- readHTMLTable(url)
  
  if (length(list) == 7) {
    
    x1 <- as.data.frame(list[5])
    x2 <- as.data.frame(list[6])
    x3 <- as.data.frame(list[7])
    names(x1) <- c("number", "Athlete.Name", "ERA", "W", "L", "Wper", "APP", "GS", "CG", "SO", "SV", "NH", "PG")
    names(x2) <- c("number", "Athlete.Name", "IP", "H", "R", "ER", "BB", "K", "2B", "3B", "HR", "BF", "AB")
    names(x3) <- c("number", "Athlete.Name", "OBA", "OBP", "WP", "HBP", "SF", "SH.B", ".P", "BK", "PO", "SB")
    x <- left_join(x1, x2, by = "Athlete.Name") %>% left_join(x3, by = "Athlete.Name")
    x <- x[,-c(14,26)]
    names(x) <- c("number", "Athlete.Name", "ERA", "W", "L", "Wper", "APP", "GS", "CG", "SO", "SV", "NH", "PG", "IP", "H", "R", "ER", "BB", "K", "2B", "3B", "HR", "BF", "AB", "OBA", "OBP", "WP", "HBP", "SF", "SH.B", ".P", "BK", "PO", "SB") 
    x[] <- lapply(x, as.character)
    x$Team <- filter(baseball_var_lu_table, stats_url == url) %>% .$school
    x$city_state <- filter(baseball_var_lu_table, stats_url == url) %>% .$city_state
    x[,c(3:34)] %<>% lapply(as.numeric)
    x[is.na(x)] <- 0
    x$BABIP <- with(x, round((H-HR)/(AB-K-HR-SF),3))
    x$SLG <- with(x, round(((H-`2B`-`3B`-HR) + (`2B`*2) + (`3B`*3) + (HR*4))/AB, 3))
    x$OPS <- with(x, OBP+SLG)
    x$Kper <- with(x, round(K/BF, 3))
    x$BBper <- with(x, round(BB/BF, 3))
    x$Kper_BBper <- with(x, round(Kper - BBper,3))
    x <- select(x, Team, city_state, everything())
    x
  }
  
  else {
    x1 <- as.data.frame(list[4])
    x2 <- as.data.frame(list[5])
    x3 <- as.data.frame(list[6])
    names(x1) <- c("number", "Athlete.Name", "ERA", "W", "L", "Wper", "APP", "GS", "CG", "SO", "SV", "NH", "PG")
    names(x2) <- c("number", "Athlete.Name", "IP", "H", "R", "ER", "BB", "K", "2B", "3B", "HR", "BF", "AB")
    names(x3) <- c("number", "Athlete.Name", "OBA", "OBP", "WP", "HBP", "SF", "SH.B", ".P", "BK", "PO", "SB")
    x <- left_join(x1, x2, by = "Athlete.Name") %>% left_join(x3, by = "Athlete.Name")
    x <- x[,-c(14,26)]
    names(x) <- c("number", "Athlete.Name", "ERA", "W", "L", "Wper", "APP", "GS", "CG", "SO", "SV", "NH", "PG", "IP", "H", "R", "ER", "BB", "K", "2B", "3B", "HR", "BF", "AB", "OBA", "OBP", "WP", "HBP", "SF", "SH.B", ".P", "BK", "PO", "SB") 
    x[] <- lapply(x, as.character)
    x$Team <- filter(baseball_var_lu_table, stats_url == url) %>% .$school
    x$city_state <- filter(baseball_var_lu_table, stats_url == url) %>% .$city_state
    x[,c(3:34)] %<>% lapply(as.numeric)
    x[is.na(x)] <- 0
    x$BABIP <- with(x, round((H-HR)/(AB-K-HR-SF),3))
    x$SLG <- with(x, round(((H-`2B`-`3B`-HR) + (`2B`*2) + (`3B`*3) + (HR*4))/AB, 3))
    x$OPS <- with(x, OBP+SLG)
    x$Kper <- with(x, round(K/BF, 3))
    x$BBper <- with(x, round(BB/BF, 3))
    x$Kper_BBper <- with(x, round(Kper - BBper,3))
    x <- select(x, Team, city_state, everything())
    x
  }
}

maxprep_pitching_team <- function(url) {
  list <- readHTMLTable(url)
  
  if (length(list) == 7) {
  
  x1 <- as.data.frame(list[5])
  x2 <- as.data.frame(list[6])
  x3 <- as.data.frame(list[7])
  names(x1) <- c("number", "Athlete.Name", "ERA", "W", "L", "Wper", "APP", "GS", "CG", "SO", "SV", "NH", "PG")
  names(x2) <- c("number", "Athlete.Name", "IP", "H", "R", "ER", "BB", "K", "2B", "3B", "HR", "BF", "AB")
  names(x3) <- c("number", "Athlete.Name", "OBA", "OBP", "WP", "HBP", "SF", "SH.B", ".P", "BK", "PO", "SB")
  x <- left_join(x1, x2, by = "Athlete.Name") %>% left_join(x3, by = "Athlete.Name")
  x <- x[,-c(14,26)]
  names(x) <- c("number", "Athlete.Name", "ERA", "W", "L", "Wper", "APP", "GS", "CG", "SO", "SV", "NH", "PG", "IP", "H", "R", "ER", "BB", "K", "2B", "3B", "HR", "BF", "AB", "OBA", "OBP", "WP", "HBP", "SF", "SH.B", ".P", "BK", "PO", "SB") 
  x[] <- lapply(x, as.character)
  x[,c(3:34)] %<>% lapply(as.numeric)
  x[is.na(x)] <- 0
  x_team <- x[,-(1:2)] %>% summarise_each(funs(sum(.,na.rm = FALSE)))
  x_team$ERA <- with(x_team, round((ER/IP) * 9, 2))
  x_team$Wper <- with(x_team, round(W/(W+L),3))
  x_team$BABIP <- with(x_team, round((H-HR)/(AB-K-HR-SF),3))
  x_team$OBA <- with(x_team, round(H/AB,3))
  x_team$OBP <- with(x_team, round((H+BB+HBP)/(AB+BB+HBP+SF),3))
  x_team$SLG <- with(x_team, round(((H-`2B`-`3B`-HR) + (`2B`*2) + (`3B`*3) + (HR*4))/AB, 3))
  x_team$OPS <- with(x_team, OBP+SLG)
  x_team$Kper <- with(x_team, round(K/BF, 3))
  x_team$BBper <- with(x_team, round(BB/BF, 3))
  x_team$Kper_BBper <- with(x_team, round(Kper - BBper,3))
  x_team
  }
  
  else {
    x1 <- as.data.frame(list[4])
    x2 <- as.data.frame(list[5])
    x3 <- as.data.frame(list[6])
    names(x1) <- c("number", "Athlete.Name", "ERA", "W", "L", "Wper", "APP", "GS", "CG", "SO", "SV", "NH", "PG")
    names(x2) <- c("number", "Athlete.Name", "IP", "H", "R", "ER", "BB", "K", "2B", "3B", "HR", "BF", "AB")
    names(x3) <- c("number", "Athlete.Name", "OBA", "OBP", "WP", "HBP", "SF", "SH.B", ".P", "BK", "PO", "SB")
    x <- left_join(x1, x2, by = "Athlete.Name") %>% left_join(x3, by = "Athlete.Name")
    x <- x[,-c(14,26)]
    names(x) <- c("number", "Athlete.Name", "ERA", "W", "L", "Wper", "APP", "GS", "CG", "SO", "SV", "NH", "PG", "IP", "H", "R", "ER", "BB", "K", "2B", "3B", "HR", "BF", "AB", "OBA", "OBP", "WP", "HBP", "SF", "SH.B", ".P", "BK", "PO", "SB") 
    x[] <- lapply(x, as.character)
    x[,c(3:34)] %<>% lapply(as.numeric)
    x[is.na(x)] <- 0
    x_team <- x[,-(1:2)] %>% summarise_each(funs(sum(.,na.rm = FALSE)))
    x_team$ERA <- with(x_team, round((ER/IP) * 9, 2))
    x_team$Wper <- with(x_team, round(W/(W+L),3))
    x_team$BABIP <- with(x_team, round((H-HR)/(AB-K-HR-SF),3))
    x_team$OBA <- with(x_team, round(H/AB,3))
    x_team$OBP <- with(x_team, round((H+BB+HBP)/(AB+BB+HBP+SF),3))
    x_team$SLG <- with(x_team, round(((H-`2B`-`3B`-HR) + (`2B`*2) + (`3B`*3) + (HR*4))/AB, 3))
    x_team$OPS <- with(x_team, OBP+SLG)
    x_team$Kper <- with(x_team, round(K/BF, 3))
    x_team$BBper <- with(x_team, round(BB/BF, 3))
    x_team$Kper_BBper <- with(x_team, round(Kper - BBper,3))
    x_team
    }
}

library(tidyverse)
library(rvest)
library(RSelenium)
library(lubridate)
library(here)

# functions --------

trim_right <- function(x) {
  gsub("\\s+$", "", x)
}

# START SELENIUM SERVER FIRST BEFORE RUNNING THIS
# in terminal: "java -jar selenium-server-standalone-2.53.1.jar -port 5556"

# opens the browers
browser <- remoteDriver(port = 5556, browser = "phantomjs")
browser$open(silent = FALSE)

# each page has 'load more' clicks needed to view all the data
# but each year has different moutn of clicks needed
# if you try to click more than is needed, it will return an error
# so this data frame and function are reponsible for managing the amount of
# 'lod more' clicks and scraping all the data

site_years <- data.frame(
  year = 1998:2018,
  load_clicks = c(rep(2, 3), rep(1, 5), rep(2, 5), rep(3, 8))
)

scrape <- function(year, clicks) {
  
  url <- paste0(
    "https://www.flashscore.com/football/usa/mls-", 
    year, 
    "/results/"
  )
  
  browser$navigate(url)
  
  # find the moad more button
  load_more <- browser$findElement(
    using = "css selector", 
    "#tournament-page-results-more a"
  )
  
  # click the load more button the appropriate amount of times
  for (i in seq_along(1:clicks)) {
    load_more$clickElement()
    Sys.sleep(10)
  }
  
  # read the data from the table after it has all been loaded
  page_text <- read_html(browser$getPageSource()[[1]]) %>% 
    html_nodes(".bold , .padl , .padr , .time") %>% 
    html_text()
  
  # get the dates to start the first row
  dates <- grepl("[0-9]+\\.[0-9]+\\. [0-9]+:[0-9]+", page_text)
  
  tibble(
    season = year,
    # each line starts with a date
    date = page_text[dates],
    home_team = page_text[which(dates) + 1],
    # sometimes home team is repeated, sometimes not
    col3 = page_text[which(dates) + 2],
    col4 = page_text[which(dates) + 3],
    # each line ends with a score
    # score format looks like "1 : 1"
    score = page_text[grepl("[0-9]+\\s:\\s[0-9]+", page_text)]
  )
}

mls <- map2_dfr(
  site_years$year, site_years$load_clicks, 
  ~scrape(year = .x, clicks = .y)
)

# clean up
games <- mls %>% 
  filter(!grepl("MLS All-Stars", home_team)) %>% 
  separate(date, into = c("date", "time"), sep = " ") %>% 
  mutate(
    home_team = home_team,
    away_team = if_else(col3 == home_team, col4, col3),
    date = dmy(paste0(date, ".", season)),
    penalties = if_else(grepl("\\([0-9]+\\s:\\s[0-9]+\\)", score), 1, 0),
    score = gsub("\\([0-9]+\\s:\\s[0-9]+\\)", "", score)
  ) %>% 
  # remove trailing white space (trimws not working b/c encoding)
  mutate_at(vars(home_team, away_team), trim_right) %>% 
  separate(score, into = c("home_final", "away_final")) %>% 
  select(-col3, -col4, -time) %>% 
  arrange(date)

# teams table

teams <- games %>% 
  distinct(home_team) %>% 
  rename(team = home_team) %>% 
  mutate(
    season = case_when(
      team %in% c("Miami", "Chicago Fire") ~ 1998,
      team %in% c("Los Angeles Galaxy", "Real Salt Lake") ~ 2005,
      team == "Houston Dynamo" ~ 2006,
      team == "Toronto FC" ~ 2007,
      team == "Seattle Sounders" ~ 2009,
      team == "Philadelphia Union" ~ 2010,
      team %in% c("Vancouver Whitecaps", "Portland Timbers") ~ 2011,
      team == "Montreal Impact" ~ 2012,
      team %in% c("New York City", "Orlando City") ~ 2015,
      team %in% c("Atlanta United", "Minnesota") ~ 2017,
      team == "Los Angeles FC" ~ 2018,
      TRUE ~ 1998
    ),
    elo = case_when(
      team %in% c("Miami", "Chicago Fire") ~ 1300,
      season != 1998 ~ 1300,
      TRUE ~ 1500
    ),
    date = ymd(paste0(season, "0301"))
  )

write_csv(games, here::here("data/processed/mls_2010-2018.csv"))

write_csv(teams, here::here("data/processed/mls_teams.csv"))

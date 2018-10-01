library(tidyverse)
library(rvest)
library(RSelenium)
library(lubridate)
library(here)

# functions --------


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
  year = 2010:2018,
  load_clicks = c(2, rep(3, 8))
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
mls_tidy <- mls %>% 
  filter(home_team != "MLS All-Stars") %>% 
  separate(date, into = c("date", "time"), sep = " ") %>% 
  mutate(
    home_team = trimws(home_team),
    away_team = trimws(if_else(col3 == home_team, col4, col3)),
    date = dmy(paste0(date, ".", season)),
    penalties = if_else(grepl("\\([0-9]+\\s:\\s[0-9]+\\)", score), 1, 0),
    score = gsub("\\([0-9]+\\s:\\s[0-9]+\\)", "", score)
  ) %>% 
  separate(score, into = c("home_final", "away_final")) %>% 
  select(-col3, -col4) %>% 
  arrange(date)

write_csv(mls_tidy, here::here("data/processed/mls_2010-2018.csv"))

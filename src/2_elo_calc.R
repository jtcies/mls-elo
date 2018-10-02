library(tidyverse)
library(here)
library(lubridate)
library(elo)

# params ------------

# k_value = seq(20, 50, by = 10)
# 
# home_bonus = seq(0, 50, by = 10)
# 
# carry_value = seq(.5, .9, by = 0.1)
# 
# expand.grid(k_value, home_bonus, carry_value)

k_value <- 35

home_bonus <- 0

carry_value <- 0.9

# functions -------------
elo_calc_in_season <- function(games, teams, k_value, home_bonus) {
  
  for (i in seq_len(nrow(games))) {
    # function for creating a running elo
    game <- games[i, ]
    
    # extract most recent elo adding in home/away adjustment
    elo_home <- teams[teams$team == game$home_team, "elo"][[1]]
    elo_away <- teams[teams$team == game$away_team, "elo"][[1]]
    
    elo_update <- elo.calc(
      score(game["home_final"], game["away_final"]),
      elo_home + home_bonus,
      elo_away,
      k = k_value
    )
    # reshape elo update
    new_elo <- elo_update %>% 
      gather() %>% 
      rename(elo = value) %>% 
      mutate(
        team = c(game$home_team, game$away_team),
        date = game$date + 1,
        season = game$season
      ) %>% 
      select(date, team, elo, season)
    
    # bind elo update to teams data for running list
    teams <- bind_rows(new_elo, teams)
  }
  return(teams)
}

carry_over <- function(teams, carry_value) {
  # at the start of every new season, carry over 75% of elo
  # rec by 538, start new season on 10/01
  new_season <- teams %>% 
    filter(season == season[[1]]) %>% # don't include expansions til they join
    distinct(team, .keep_all = TRUE) %>% 
    mutate(
      elo = (carry_value * elo) + ((1 - carry_value) * 1500),
      season = season + 1, # add one for start of new season
      date = ymd(paste0(season, "0301"))
    )
  bind_rows(new_season, teams)
}

elo_calc <- function(games, teams, k_value, home_bonus, carry_value) {
  
  seasons <- unique(games$season)
  
  for (j in seq_len(length(seasons))){
    # run the calculation within each season
    season_games <- games[games$season == seasons[[j]], ]
    teams <- elo_calc_in_season(season_games, teams, k_value = k_value, 
                                home_bonus = home_bonus)
    # then apply the carryover
    teams <- carry_over(teams, carry_value = carry_value)
  }
  return(teams)
}

fill_elo <- function(data) {
  # function to fill in missing elo
  data %>% 
    tidyr::complete(date = full_seq(date, period = 1), team) %>% 
    arrange(date) %>% 
    fill(elo) %>% 
    mutate(elo = if_else(month(date) %in% 1:2, NA_real_, elo))
}

# import ---------------

games <- "data/processed/mls_2010-2018.csv" %>% 
  here::here() %>% 
  read_csv()

teams <- "data/processed/mls_teams.csv" %>% 
  here::here() %>% 
  read_csv()

# run functions ---------------------

# this will take a few minutes to run
running_elo <- elo_calc(games, teams, k_value = k_value, 
                        home_bonus = home_bonus, carry_value = carry_value)

# fill in missing dates
complete_elo <- running_elo %>% 
  split(.$team) %>% 
  map_dfr(fill_elo) %>% 
  mutate(season = year(date)) %>% 
  filter(date <= today())


# write -----------

write_csv(complete_elo, here::here("data/processed/complete_elo.csv"))
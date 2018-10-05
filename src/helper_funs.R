library(tidyverse)
library(lubridate)
library(elo)


elo_calc_in_season <- function(games, teams, k_value, home_bonus) {
  # elo update after each game in one season
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
    arrange(team, date) %>% 
    fill(elo) %>% 
    mutate(elo = if_else(month(date) %in% 1:2, NA_real_, elo))
}

accuracy_count <- function(games, complete_elo) {
  
  game_prob <- function(games, complete_elo, game_num) {
    
    game <- games[game_num, ]
    
    home_elo <- complete_elo[complete_elo$team == game$home_team & complete_elo$date == game$date, ][["elo"]]
    away_elo <- complete_elo[complete_elo$team == game$away_team & complete_elo$date == game$date, ][["elo"]]
    
    win_prob <- elo.prob(home_elo, away_elo)
    
    game$home_win_prob <- win_prob
    
    game
  }
  
  win_prob <- 1:nrow(games) %>% 
    map_dfr(~ game_prob(games, complete_elo, .x)) %>% 
    mutate(
      correct_pred = case_when(
        home_win_prob > 0.5 & home_final > away_final ~ 1,
        home_win_prob == 0.5 & home_final == away_final ~ 1,
        home_win_prob < 0.5 & home_final < away_final ~ 1,
        TRUE ~ 0
      )
    )
  sum(win_prob$correct_pred) / nrow(win_prob)
}

elo_run <- function(k_value, home_bonus, carry_value, initial_elos) {
  elo.run(
    score(home_final, away_final) ~ adjust(home_team, home_bonus) +
      away_team + regress(season, 1500, carry_value) + 
      k(k_value),
    data = games,
    initial.elos = initial_elos
  )
}

k_fun <- function(k_value, goal_diff) {
  
  if (goal_diff < 2) {
    k_value
  } else if(goal_diff == 2) {
    k_value * 1.5
  } else if(goal_diff == 3) {
    k_value * 1.75
  } else if(goal_diff >= 4) {
    k_value + ((0.75 + (goal_diff - 3)/8) * k_value)
  }
   
}
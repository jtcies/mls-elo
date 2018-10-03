library(tidyverse)
library(here)

source(here::here("src/helper_funs.R"))

# params ---------
# from grid search

k_value = 18
home_bonus = 100
carry_value = 0.45
initial_elo = NULL

# import ---------------

games <- "data/processed/mls_2010-2018.csv" %>% 
  here::here() %>% 
  read_csv()

teams <- "data/processed/mls_teams.csv" %>% 
  here::here() %>% 
  read_csv()

# run functions ---------------------

elo_results <- elo_run(k_value = k_value, home_bonus = home_bonus,
                       carry_value = carry_value, initial_elos = initial_elo)

games_elo <- as.data.frame(elo_results)

# tidy -----------

games_date <- games %>% 
  select(season, date)

# running elo 

home <- games_elo %>% 
  select(
    team = team.A,
   elo_after = elo.A
  ) %>% 
  bind_cols(games_date)

away <- games_elo %>% 
  select(
    team = team.B,
    elo_after = elo.B  
  ) %>% 
bind_cols(games_date)

complete_elo <- bind_rows(home, away) %>% 
  group_by(team) %>% 
  arrange(date) %>% 
  mutate(
    elo_before = lag(elo_after),
    elo_before = if_else(is.na(elo_before), 1500, elo_before)
  ) %>% 
  ungroup()


# win prob for each game

home_win_prob <- games_elo %>% 
  select(
    team = team.A,
    win_prob = p.A,
    win = wins.A
  ) %>% 
  bind_cols(games_date)

away_win_prob <- games_elo %>% 
  mutate(
    win_prob = 1 - p.A,
    win = 1 - wins.A
  ) %>% 
  select(
    team = team.B,
    win_prob,
    win
  ) %>% 
  bind_cols(games_date)

win_prob <- bind_rows(home_win_prob, away_win_prob) %>% 
  arrange(date) %>% 
  mutate(
    upset_win = case_when(
      win_prob < 0.5 & win == 1 ~ 1,
      TRUE ~ 0
    ),
    upset_loss = case_when(
      win_prob > 0.5 & win == 0 ~ 1,
      TRUE ~ 0
    )
  )

# visualize -------------

ggplot(complete_elo, aes(date, elo_before, group = team)) +
  geom_line(alpha = 0.2)

win_prob %>% 
  group_by(team) %>% 
  summarise(mean_upset_loss = mean(upset_loss)) %>% 
  arrange(desc(mean_upset_loss))


# write -----------


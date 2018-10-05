library(tidyverse)
library(here)

source(here::here("src/helper_funs.R"))

# params ---------
# from grid search
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
k_value = 24
home_bonus = 100
carry_value = 0.4

# import ---------------

games <- "data/processed/mls_2010-2018.csv" %>% 
  here::here() %>% 
  read_csv()

teams <- "data/processed/mls_teams.csv" %>% 
  here::here() %>% 
  read_csv()

initial_elos <- teams$elo
names(initial_elos) <- teams$team

games$k_col <- map_dbl(
  abs(games$home_final - games$away_final), 
  ~k_fun(k_value, .x)
)

# run functions ---------------------

elo_results <- elo_run(k_value = games$k_col, home_bonus = home_bonus,
                       carry_value = carry_value, initial_elos = NULL)

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
  mutate(elo_before = lag(elo_after)) %>% 
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
      win_prob < 0.5 & win == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    upset_loss = case_when(
      win_prob > 0.5 & win == 0 ~ 1,
      win_prob > 0.5 & win == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    big_loss = case_when(
      win_prob > 0.6 & win == 0 ~ 1,
      win_prob > 0.6 & win == 1 ~ 0,
      TRUE ~ NA_real_
    )
  )

# visualize -------------

ggplot(complete_elo, aes(date, elo_before, group = team)) +
  geom_line(alpha = 0.2)

win_prob %>% 
  group_by(team) %>% 
  summarise(
    mean_upset_loss = mean(upset_loss, na.rm = TRUE),
    mean_upset_win = mean(upset_win, na.rm = TRUE),
    mean_big_loss = mean(big_loss, na.rm = TRUE)
  ) %>% 
  arrange(desc(mean_upset_loss)) %>% 
  as.data.frame()

win_prob %>% 
  group_by(team) %>% 
  summarise(mean_upset_loss = mean(upset_loss, na.rm = TRUE)) %>% 
  ggplot(aes(reorder(team, mean_upset_loss), mean_upset_loss)) +
    geom_point(size = 3) +
    coord_flip()

u_big_loss <- win_prob %>% 
  filter(team == "Philadelphia Union", upset_loss == 1) %>% 
  arrange(desc(win_prob))

games_elo %>% 
  bind_cols(games_date) %>% 
  filter(team.A == "Philadelphia Union" | team.B == "Philadelphia Union") %>% 
  right_join(u_big_loss, by = "date") %>% 
  select(date, team.A, team.B, u_win_prob = win_prob) %>% 
  arrange(desc(u_win_prob))

complete_elo %>% 
  filter(team == "Philadelphia Union") %>% 
  ggplot(aes(date, elo_before)) +
    geom_line()

# write -----------

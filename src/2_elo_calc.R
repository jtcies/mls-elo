library(tidyverse)
library(here)

source(here::here("src/helper_funs.R"))

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
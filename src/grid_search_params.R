library(here)
library(pROC)

source(here::here("src/helper_funs.R"))

# params ------------

k_value <- seq(10, 70, by = 5)

home_bonus <- seq(0, 200, by = 10)

carry_value <- seq(.5, .9, by = 0.1)

params <- expand.grid(
  k_value = k_value, 
  home_bonus = home_bonus
  #carry_value = carry_value
  )


# find ideal params -----------

grid_search <- function(k_value) {
  
  dat <- elo.run(score(home_final, away_final) ~ home_team + away_team,
                 data = games,
                 k = k_value)
  
  
  pROC::auc(dat)
  
}


# import ---------------

games <- "data/processed/mls_2010-2018.csv" %>% 
  here::here() %>% 
  read_csv()

teams <- "data/processed/mls_teams.csv" %>% 
  here::here() %>% 
  read_csv()


# run -----------

param_acc <- map(k_value, grid_search)

params_final <- bind_cols(params, auc = unlist(param_acc))

# visualize --------------

write_csv(params_final, here::here("output/param_grid_search_results.csv"))

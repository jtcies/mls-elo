library(here)

source(here::here("src/helper_funs.R"))

# params ------------

k_value <- seq(20, 50, by = 10)

home_bonus <- seq(0, 30, by = 10)

carry_value <- seq(.5, .9, by = 0.1)

params <- expand.grid(k_value = k_value, 
                      home_bonus = home_bonus, 
                      carry_value = carry_value)


# find ideal params -----------

grid_search <- function(k_value, home_bonus, carry_value) {
  
  # this will take a few minutes to run
  running_elo <- elo_calc(games, teams, k_value = k_value, 
                          home_bonus = home_bonus, carry_value = carry_value)
  
  # fill in missing dates
  complete_elo <- running_elo %>% 
    fill_elo() %>% 
    mutate(season = year(date)) %>% 
    filter(date <= today())
  
  accuracy_count(games, complete_elo)
  
}

# import ---------------

games <- "data/processed/mls_2010-2018.csv" %>% 
  here::here() %>% 
  read_csv()

teams <- "data/processed/mls_teams.csv" %>% 
  here::here() %>% 
  read_csv()


# run -----------

param_acc <- pmap(params, grid_search)

params_final <- bind_cols(params, acc = unlist(param_acc))

# visualize --------------

ggplot(params_final, aes(k_value, acc)) +
  geom_line() +
  facet_grid(home_bonus ~ carry_value )

write_csv(params_final, here::here("output/param_grid_search_results.csv"))

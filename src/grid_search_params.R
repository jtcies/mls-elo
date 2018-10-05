# this script I'm trying to find the ideal paramters for
# k, home bonus, carry over percentage between season, and initial elos

library(here)
library(pROC)

source(here::here("src/helper_funs.R"))

# params ------------

k_value <- seq(10, 50, by = 2)

home_bonus <- seq(0, 200, by = 10)

carry_value <- seq(.1, .9, by = 0.05)

params <- expand.grid(
  k_value = k_value,
  home_bonus = home_bonus,
  carry_value = carry_value
)

# find ideal params -----------

# all params together
grid_search <- function(k_value, home_bonus, carry_value, initial_elos) {
  
  dat <- elo_run(k_value = k_value, home_bonus = home_bonus, 
                 carry_value = carry_value, initial_elos = initial_elos)
  
  pROC::auc(dat)
}


# import ---------------

games <- "data/processed/mls_2010-2018.csv" %>% 
  here::here() %>% 
  read_csv() 

teams <- "data/processed/mls_teams.csv" %>% 
  here::here() %>% 
  read_csv() %>% 
  arrange(team)


initial_elos <- teams$elo
names(initial_elos) <- teams$team

# run -----------

# gird all params

param_acc <- pmap(params, grid_search, initial_elos = initial_elos)

params_final <- bind_cols(params, auc = unlist(param_acc))

params_final %>% 
  arrange(desc(auc)) %>% 
  head(20)

# k only

best_k <- map(k_value, grid_search, initial_elos = NULL,
              carry_value = 1, home_bonus = 0)

best_k <- data.frame(k_value, auc = unlist(best_k))

# home bonus

best_bonus <- map(home_bonus, grid_search, initial_elos = NULL,
                  k_value = 24, carry_value = 1)

best_bonus <- data.frame(home_bonus, auc = unlist(best_bonus))

# carry value

best_carry <- map(carry_value, grid_search, initial_elos = NULL,
                  k_value = 24, home_bonus = 0)

best_carry <- data.frame(carry_value, auc = unlist(best_carry))

# visualize --------------

ggplot(params_final, aes(x = k_value, y = home_bonus, fill = auc)) +
  geom_raster()

ggplot(best_k, aes(k_value, auc)) +
  geom_line()

ggplot(best_bonus, aes(home_bonus, auc)) +
  geom_line()

ggplot(best_carry, aes(carry_value, auc)) +
  geom_line()


# write ----------

write_csv(params_final, here::here("output/param_grid_search_results.csv"))

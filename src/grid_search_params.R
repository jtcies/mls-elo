library(here)
library(pROC)

source(here::here("src/helper_funs.R"))

# params ------------

k_value <- seq(10, 50, by = 2)

home_bonus <- seq(0, 150, by = 5)

carry_value <- seq(.1, .9, by = 0.05)

params <- expand.grid(
  k_value = k_value,
  home_bonus = home_bonus,
  carry_value = carry_value
)

# find ideal params -----------

# all params together
grid_search <- function(k_value, home_bonus, carry_value) {
  
  dat <- elo.run(score(home_final, away_final) ~ adjust(home_team, home_bonus) +
                   away_team + regress(season, 1500, carry_value),
                 data = games,
                 k = k_value)
  
  pROC::auc(dat)
}

# k alone
search_k <- function(k_value) {
  dat <- elo.run(score(home_final, away_final) ~ home_team +
                   away_team,
                 data = games,
                 k = k_value)
  
  pROC::auc(dat)
}

# home bonus alone
search_home_bonus <- function(home_bonus) {
  dat <- elo.run(score(home_final, away_final) ~ adjust(home_team, home_bonus) +
                   away_team,
                 data = games,
                 k = 18)
  
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

param_acc <- pmap(params, grid_search)

params_final <- bind_cols(params, auc = unlist(param_acc))

params_final %>% 
  arrange(desc(auc)) %>% 
  head(20)

# k only

best_k <- map(k_value, search_k)

best_k <- data.frame(k_value, auc = unlist(best_k))

# home bonus

best_bonus <- map(home_bonus, search_home_bonus)

best_bonus <- data.frame(home_bonus, auc = unlist(best_bonus))

# visualize --------------

ggplot(params_final, aes(x = k_value, y = home_bonus, fill = auc)) +
  geom_raster()

ggplot(best_k, aes(k_value, auc)) +
  geom_line()

ggplot(best_bonus, aes(home_bonus, auc)) +
  geom_line()

  
# write ----------

write_csv(params_final, here::here("output/param_grid_search_results.csv"))

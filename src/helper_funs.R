library(tidyverse)

accuracy_count <- function() {
  
  game_prob <- function(games, ratings, game_num) {
    
    game <- games[game_num, ]
    
    home_elo <- ratings[ratings$team == game$home_team & ratings$date == game$date, ][["elo"]]
    away_elo <- ratings[ratings$team == game$away_team & ratings$date == game$date, ][["elo"]]
    
    win_prob <- elo.prob(home_elo, away_elo)
    
    game$home_win_prob <- win_prob
    
    game
  }
  
  win_prob <- 1:nrow(games) %>% 
    map_dfr(~ game_prob(games, complete_elo, .x)) 
  
  win_prob %>% 
    mutate(
      correct_pred = case_when(
        home_win_prob > 0.5 & home_final > away_final ~ 1,
        home_win_prob == 0.5 & home_final == away_final ~ 1,
        home_win_prob < 0.5 & home_final < away_final ~ 1,
        TRUE ~ 0
      )
    ) %>% 
    count(correct_pred)
}

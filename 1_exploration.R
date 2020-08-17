library(tidyverse)

extrafont::loadfonts(device = "win")
theme_set(theme_minimal(base_size = 16, base_family = "Tw Cen MT"))

df <- readxl::read_xlsx(here::here("data","2020-08-04_scores.xlsx")) %>% 
  janitor::clean_names()

# check if any player is recorded twice for a single game
df %>% 
  group_by(
    game_id,
    player
  ) %>% 
  tally() %>% 
  filter(n > 1)

d <- df %>% 
  mutate(
    id = paste(game_id, player, sep = "-")
  ) %>% 
  pivot_longer(
    cols = starts_with("hole"), 
    names_to = "hole", 
    names_prefix = "hole_",
    values_to = "score") %>% 
  mutate(hole = as.numeric(hole)) %>% 
  nest(-id) %>% 
  # get total for each player for each game
  mutate(
    total = map_dbl(data, ~ sum(.$score))
  ) %>% 
  unnest() %>% 
  group_by(hole) %>% 
  mutate(
    median_score = median(score, na.rm = T),
    mean_score = mean(score, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(game_id) %>% 
  mutate(
    winner = total == min(total, na.rm = T)
  )

d_game <- d %>% 
  group_by(year, game_id, player) %>% 
  summarize(
    total = sum(score, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(game_id) %>% 
  mutate(
    winner = total == min(total)
  )

write_csv(d, here::here("data","full-golf-scores.csv"))
write_csv(d_game, here::here("data","full-golf-games.csv"))

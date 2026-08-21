pak::pkg_install("FantasyFootballAnalytics/ffanalytics")
library(ffanalytics)
library(tidyverse)
my_scrape <- scrape_data(src = c("CBS", "ESPN", "FantasySharks","FFToday",
                                 "Walterfootball"),
                         pos = c("QB", "RB", "WR", "TE", "K"),
                         season = NULL, # NULL grabs the current season
                         week = NULL) # NULL grabs the current week

summary <- projections_table(my_scrape) |>
  add_player_info() |>
  add_adp() |>
  mutate(Player.Name = paste(first_name, last_name)) |>
  filter(avg_type == "average") |>
  select(Player.Name, age, )

cbs <- cbs_draft() |>
  arrange(adp) |>
  mutate(cbs_rank = as.numeric(1:n())) |>
  select(id, player, cbs_adp = adp, cbs_rank)
espn <- espn_draft() |>
  arrange(adp) |>
  mutate(espn_rank = as.numeric(1:n())) |>
  select(id, player, espn_adp = adp, espn_rank)
ffc <- ffc_draft() |>
  arrange(adp) |>
  mutate(ffc_rank = as.numeric(1:n())) |>
  select(id, player, ffc_adp = adp, ffc_rank)
mfl <- mfl_draft() |>
  arrange(adp) |>
  mutate(mfl_rank = as.numeric(1:n())) |>
  select(id, mfl_adp = adp, mfl_rank)
yahoo <- yahoo_draft() |>
  arrange(adp) |>
  mutate(yahoo_rank = as.numeric(1:n())) |>
  select(id, yahoo_adp = adp, yahoo_rank)

draft_data <- summary |>
  left_join(cbs) |>
  left_join(espn) |>
  left_join(ffc) |>
  #left_join(mfl) |>
  left_join(yahoo)
draft_data <- mutate(draft_data, grand_average_rank = rowMeans(select(draft_data,
                                                          ends_with("rank")),
                                                   na.rm = TRUE)) |>
  select(id:team, grand_average_rank, points:yahoo_rank) |>
  arrange(grand_average_rank)

write_csv(draft_data, "draft_data.csv")

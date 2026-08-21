library(readr)
library(dplyr)
library(ffanalytics)
library(stringr)

league <- tibble::tibble(
  team =  c("Like a touchdown in your mouth",
    "Jeff'd it up!",
    "Nickeled and Danny Dimed",
    "Chasing the Dream",
    "Bacon Egbuka and Cheese",
    "Daebo",
    "Billups and Rozier Let it Ride",
    "The Rattler Snakes",
    "The Sequined Barkeys",
    "Yevich's Bookworms",
    "Porta-johns",
    "MICHAEL's Frontline Precision",
    "Vrabel's Indiscretions",
    "Fins Up!"),
  standings = c(9,7,1,4,2,8,3,5,6,11,10,12,NA,NA),
  playoffs = c(F,T,T,T,T,T,T,T,T,F,F,F,NA,NA)) |>
  arrange(standings)

draft_order <- function(df){
  losers <- filter(df, !playoffs) |>
    arrange(desc(standings)) |>
    mutate(draft_position = 1:n()) |>
    select(draft_position, team)
  not_losers <- filter(df, playoffs | is.na(playoffs)) |>
    mutate(draft_position = sample(1:n(), n())+4) |>
    select(draft_position, team)
  arrange(bind_rows(losers, not_losers), draft_position)
}

# draft_order(league)

draft_2026 <- readr::read_csv("draft_2026.csv")

make_draft <- function(first_round, rounds = 4){
  all_rounds <- first_round |>
    mutate(round = 1)
  for(i in 1:rounds){
    if(i > 1){
      if(i%%2 == 0){
        all_rounds <- bind_rows(all_rounds, arrange(mutate(first_round, round = i),
                                                  desc(draft_position)))
      } else {
        all_rounds <- bind_rows(all_rounds, arrange(mutate(first_round, round = i),
                                                    draft_position))
      }
    }
  }
  mutate(all_rounds, overall_position = 1:n()) |>
    mutate(.by = round, round_position = 1:n()) |>
    select(team, round, round_position, overall_position)
}

full_draft_2026 <- make_draft(draft_2026, 13) |>
  left_join(read_csv("keepers.csv"))

View(full_draft_2026)

# https://s3-us-west-1.amazonaws.com/fftiers/out/weekly-ALL.csv
boris_chen_tier <- read_csv("https://s3-us-west-1.amazonaws.com/fftiers/out/weekly-ALL.csv") |>
  rename(chen_rank = Rank)

# https://fantasyfootballanalytics.net/2016/06/ffanalytics-r-package-fantasy-football-data-analysis.html
my_scrape <- scrape_data(src = c("CBS", "ESPN", "FantasySharks","FFToday",
                                 "Walterfootball"),
                         pos = c("QB", "RB", "WR", "TE", "K"),
                         season = NULL, # NULL grabs the current season
                         week = NULL) # NULL grabs the current week

summary <- projections_table(my_scrape) |>
  add_player_info() |>
  mutate(Player.Name = paste(first_name, last_name)) |>
  filter(avg_type == "average") |>
  select(Player.Name, age, team, proj_points = points, proj_points_floor = floor,
         proj_point_ceiling = ceiling)

draft_data <- left_join(summary, boris_chen_tier)

# https://www.fantasypros.com/nfl/real-time-adp/
fantasy_pros <- read_csv("FantasyPros_Real-Time_ADP_Redraft-Half-PPR_All_14team_2026-08-20.csv",
                         skip = 2) |>
  mutate(Name = trimws(sub("\\s+[A-Z]{3}\\s*\\(\\d+\\)$", "", Name))) |>
  mutate(Name = str_remove(Name, "\\s+[A-Z]{2}\\s*\\(\\d+\\)$")) |>
  select(Name, position_rank = POS.RK, real_time_adp = `REAL-TIME`,
         round_rank = `PICK NUM.`, yahoo_rank = YAHOO, sleeper_rank = SLEEPER)

draft_data <- left_join(draft_data, fantasy_pros, by = c("Player.Name" = "Name")) |>
  arrange(yahoo_rank)

write_csv(draft_data, "draft_data.csv")

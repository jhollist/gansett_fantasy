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

draft_order(league)

x <- readr::read_csv("draft_2026.csv")

make_draft <- function(first_round, rounds = 4){
  all_rounds <- first_round |>
    mutate(round = 1)
  for(i in 1:rounds){
    if(i > 1){
      all_rounds <- bind_rows(all_rounds, arrange(mutate(first_round, round = i),
                                                desc(draft_position)))
    }
  }
  mutate(all_rounds, draft_position = 1:n()) |>
    select(team, round, selection = draft_position)
}

make_draft(x, 13) |> View()


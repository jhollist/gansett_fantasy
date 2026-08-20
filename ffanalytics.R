pak::pkg_install("FantasyFootballAnalytics/ffanalytics")
library(ffanalytics)
my_scrape <- scrape_data(src = c("CBS", "NFL", "NumberFire"),
                         pos = c("QB", "RB", "WR", "TE", "DST"),
                         season = NULL, # NULL grabs the current season
                         week = NULL) # NULL grabs the current week
all_players <- bind_rows(my_scrape[-5])
cbs <- cbs_draft()
espn <- espn_draft()
mfl_draft()

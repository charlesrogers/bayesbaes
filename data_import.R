devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
library(tidyverse)

# Load data from FBREF
serieA_2019 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2019, tier = "1st")
serieA_2020 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2020, tier = "1st")
serieA_2021 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2021, tier = "1st")
serieA_2022 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2022, tier = "1st")
serieA_2023 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2023, tier = "1st")

# Load data from Transfermarkt
## https://jaseziv.github.io/worldfootballR/articles/extract-transfermarkt-data.html



# Merge Data from FBREF and Transfermarkt

mapped_players <- player_dictionary_mapping()
dplyr::glimpse(mapped_players)
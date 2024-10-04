# install packages
if (!require(RSQLite) | !require(DBI)) {
  install.packages(c("RSQLite", "DBI"))
}

library(tidyverse)
library(RSQLite)
library(DBI)
# Create a SQLite database
## isa.db <- dbConnect(RSQLite::SQLite(), "isa.sqlite")

# Open connection
isa.db <- dbConnect(RSQLite::SQLite(), "isa.sqlite")


# Write to table
# This code writes the data from the 5 seasons of the Serie A league (2019 to 2023)
# to a table named "table.games" in the SQLite database "isa.sqlite".
# The data is appended to the table, meaning that if the table already exists,
# the new data is added to the existing table.
# The rbind() function is used to combine the data from the 5 seasons into a single data frame.
dbWriteTable(isa.db, "season_games", rbind(serieA_2019, serieA_2020, serieA_2021, serieA_2022, serieA_2023), APPEND = TRUE)

# Add new data to same table
## dbWriteTable(isa.db, "season_games", rbind(#), APPEND = TRUE)

# create new table
## dbExecute(isa.db, "CREATE TABLE IF NOT EXISTS transfer_data (id INTEGER PRIMARY KEY)")

# Query the database
df.seasons <- dbGetQuery(isa.db, "SELECT * FROM season_games")
## df.transfer_data <- dbGetQuery(isa.db, "SELECT * FROM transfer_data")

# Close connection
dbDisconnect(isa.db)


df.seasons %>%
  head(1000)%>%
write.csv(., "df.seasons.csv", row.names = FALSE)
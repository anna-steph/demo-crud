# Define database connections

# Libraries and params ----------------------------------------------------

.libPaths("Rlibrary/")

library(pool)
library(DBI)
library(RSQLite)

# Database connections ----------------------------------------------------

message("redfin pool")

redfin_pool <- pool::dbPool(
  RSQLite::SQLite(),
  dbname = "data/redfin_demo.sqlite3"
)

onStop(function() {
  poolClose(redfin_pool)
})

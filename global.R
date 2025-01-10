message("libPaths")

.libPaths("Rlibrary/")

# Libraries ------------------------------------------------------------------

message("Libraries")

library(shiny)
library(DBI)
#library(RPostgreSQL)
library(shinythemes)
library(shinyjs)
library(htmlwidgets)
library(shinycssloaders)
library(readxl)
library(httr)
#library(odbc)
library(tidyr)
library(dplyr)
library(dbplyr)
library(pool)
library(memoise)
library(stringr)
library(ggplot2)
library(data.table)
library(RSQLite)
library(rhandsontable)

# Parameters and reference tables ----------------------------------------

dash_name <- "redfin-shiny-demo"

# Options
options(scipen = 999) # turns off scientific notation

message("Functions")

files <- list.files("R/", full.names = TRUE)
sapply(files, source)

message("References")

# Load readmes
readmes <- c("dt", "rhandson", "dist", "logs")

purrr::walk(
  .x = readmes,
  .f = ~ assign(paste0({{.}}, "_readme"),
                includeMarkdown(paste0("metadata/", {{.}}, "_readme.md")),
                envir = .GlobalEnv)
)

message("Flat files and lookup tables")

# City input selection from db
city_choices <- redfin_pool %>%
  tbl("redfin_demo") %>%
  select(city) %>%
  collect() %>%
  distinct(city) %>%
  arrange(city)

# Property_type input selection
prop_choices <- redfin_pool %>%
  tbl("redfin_demo") %>%
  select(property_type) %>%
  collect() %>%
  distinct(property_type) %>%
  arrange(property_type)

message("Folder locations")

logpath <- paste0(getwd(), "/", "logs/")

message("Default city")
default_city <- "Houston"


#' Redfin csv to sqlite
#' 
#' Convert a Redfin sample csv to a sqlite db
#' 
#' Assumes Redfin's downloadable csv format hasn't changed
#' 
#' Citation: Data provided by Redfin, a national real estate brokerage.
#' For more info, see https://www.redfin.com/news/data-center/
#' 
#' Assist from https://stackoverflow.com/questions/66047880/creating-an-sqlite-db-in-r-from-an-csv-file-why-is-the-db-file-0kb-and-contains
#' 
#' Dependencies: dplyr
#' 
#' @param redfin_csv string; path to redfin csv - drop this in data/
#' @param outpath string; path to output db, defaults to current dir
#' @param db_name string; db name
redfin_csv_to_sqlite <- function(redfin_csv,
                             outpath = getwd(),
                             db_name = "redfin_demo") {

  data <- read.csv(redfin_csv) %>%
    filter(!(is.na(MLS.))) %>%
    mutate(SOLD.DATE = as.Date(SOLD.DATE, "%B-%d-%Y"))

  # varnames cleanup
  name_string <- gsub("\\.", "_", tolower(names(data)))
  names_v1 <- gsub("_$", "", name_string)
  names_v2 <- gsub("x__", "dollars_per_", names_v1)
  data_names <- gsub("^url([a-zA-Z]|[0-9]|[[:punct:]])+", "url", names_v2)

  names(data) <- data_names
  clean_name <- "clean_redfin.csv"

  write.csv(data, file = paste0("data/", clean_name), row.names = F)

  # system call to turn csv into sqlite db
  call <- paste0("sqlite3 -csv ",
                 paste0(outpath, "/data/", db_name),
                 ".sqlite3 '.import ",
                 paste0(outpath, "/data/", clean_name),
                 " ", db_name, "'")

  system(call)

  message(paste0(db_name, " sqlite database created"))

}

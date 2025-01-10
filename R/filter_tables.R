# Table filter functions

#' Format ui inputs as list for server ---------------------------------------
#'
#' Note ui input numbers will be formatted as characters
#'
#' @param x user-specified inputs from the ui
#' @return a formatted list of inputs
format_input <- function(x) {
  c(paste0(as.character(x, collapse = ",")))
}

#' Solution to table filter if-else combinations madness --------------------
#' Adapted from
#' https://community.rstudio.com/t/applying-multiple-filters-for-a-reactive-table/4932/4
#'
#' This function allows the table output to accommodate filters without creating
#' a separate table to match each combination of filters.
#'
#' Original code used != "" as the condition, but conditional panels won't
#' return "" if not selected. Use == TRUE for conditional panels.
#' For non-conditional checkbox, use !(is.null())
#'
#' @param condition checks whether users have specified values in an input
#' function. If users don't specify values, the ui will return a null, "", or
#' FALSE value, depending on the input function.
#'
#' @param success the input value string from the ui, which filters the table,
#' otherwise allows the full table to be returned.
#'
#' @return a conditional filter for the table
conditional <- function(condition, success) {
  if (condition) success else TRUE
}

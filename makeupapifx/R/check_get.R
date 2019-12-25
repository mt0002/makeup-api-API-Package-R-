#' status
#'
#' Get the connection status of makeup-api API.
#'
#' This function allows the user to check the connection with the makeup-api API.
#'
#' @return Use this function to get connection status.
#' @keywords GET, endpoint, API
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' check_get()
#' @export

check_get <- function() {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  GET(endpoint)
}


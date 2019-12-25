#' interaCos
#'
#' Obtain interactive dataframe of the makeup-api API.
#'
#' This function generates an interactive makeup-api API dataframe.
#'
#' @return Use this function to get an interactive dataframe.
#' @keywords interactive, dataframe, endpoint, API
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @import DT
#' @examples
#' interaCos()
#' @export

interaCos <- function() {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  datatable(df)
}

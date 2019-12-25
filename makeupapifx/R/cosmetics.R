#' cosmetics
#'
#' Get the complete, original dataframe of makeup-api API.
#'
#' This function allows the user to obtain the oiginal dataframe the makeup-api API. Columns with NA values have been removed.
#'
#' @return Use this function to get original dataframe.
#' @keywords dataframe, endpoint, API, makeup
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' cosmetics()
#' @export

cosmetics <- function() {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  df
}

#' typeCos
#'
#' Get the complete, original dataframe of makeup-api API.
#'
#' This function allows the user to obtain the oiginal dataframe the makeup-api API. Columns with NA values have been removed.
#'
#' @param x Character Vector. User inputs desired makeup product type. This function allows the user to obtain a dataframe that contains all makeup products of a specific product type the user inputs.  The string should be wrapped in quotations marks. Set to lipstick as default.
#' @return Use this function to get a dataframe for a type  of makeup product.
#' @keywords dataframe, endpoint, API, makeup, input
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' typeCos()
#' @export


typeCos <- function(x = "lipstick") {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  filter(df, df$`Product Type`== x)
}

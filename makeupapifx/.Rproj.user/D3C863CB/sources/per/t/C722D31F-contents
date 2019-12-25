#' brands
#'
#' Obtain dataframe that displays makeup products of a specific brand.
#'
#' This function allows the user to obtain a dataframe that contains all makeup products of a specific brand the user inputs.
#'
#' @param x Character Vector. User inputs desired makeup brand. The input should be wrapped in quotations marks. Set to nyx as default.
#' @return Use this function to get a dataframe for a specific brand.
#' @keywords dataframe, endpoint, API, makeup
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' brands()
#' @export


brands <- function(x = "nyx") {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  price_df <<- df %>%
    filter(Brand == x)
  price_df[order(price_df$Price), ]
}

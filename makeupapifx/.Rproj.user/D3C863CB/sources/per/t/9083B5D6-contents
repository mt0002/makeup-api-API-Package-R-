#' price
#'
#' Obtain a dataframe based on desired price.
#'
#' This function allows the user to obtain a dataframe for a specific price range through input of desired price range.
#' The user will be able to enter the lower-bound and upper-bound price points of their desired price range. The resulting dataframe will be sorted in ascending order.
#'
#' @param x Numerical  argument. Enter desired lower-bound price point. The resulting dataframe will display products greater than or equal to this numerical value. Default value is 5.
#' @param y Numerical  argument. Enter desired upper-bound price point. The resulting dataframe will display products less than or equal to this numerical value. Default value is 50.
#' @return Use this function to get dataframe based on price limits.
#' @keywords dataframe, endpoint, API, price, numeric
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' price()
#' @export

price <- function(x = 5, y = 50) {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  df$Price <- as.numeric(df$Price)
  price_df <- df %>%
    filter(Price >= x) %>%
    filter(Price <= y)
  price_df[order(price_df$Price), ]
}


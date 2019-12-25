#' lookCos
#'
#' Looks up makeup products for a certin brand.
#'
#' This function allows the user to obtain a dataframe based on the product type and brand of makeup they want.
#'
#' @param y Character Vector. User inputs desired makeup product type. The input should be wrapped in quotations marks. Set to lipstick as default.
#' @param z Character Vector. User inputs desired makeup brand. The string should be wrapped in quotations marks. Set to nyx as default.
#' @return Use this function to get a dataframe taht contains type of makeup product from a brand.
#' @keywords dataframe, endpoint, API, makeup, input
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' lookCos()
#' @export



lookCos <- function(y = "lipstick", z = "nyx"){
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  price_df <- df %>%
    filter(`Product Type` == y) %>%
    filter(Brand ==  z)
  price_df[order(price_df$Price), ]
}

#' searchCos
#'
#' Search for desired makeup product from makeup-api API through three conditions.
#'
#' This function shows the user a dataframe that contains products from three conditions that the user sets.
#'
#' @param price Logical statement. This is set to TRUE by default. The default setting allows user to
#' input a price limit for the resulting dataframe. Setting this statement to FALSE would prompt the function to not include price as a condition for the resulting dataframe.
#' @param type Logical statement. This is set to TRUE by default. The default setting allows user to input a makeup product type for the resulting dataframe. The resulting dataframe would show only makeup products of this product type. Setting this statement to FALSE would prompt the function to not include makeup product type as a condition for the resulting dataframe.
#' @param brand Logical statement. This is set to TRUE by default. The default setting allows user to input a makeup brand for the resulting dataframe. The resulting dataframe would show only makeup products of this makeup brand. Setting this statement to FALSE would prompt the function to not include makeup brand as a condition for the resulting dataframe.
#' @return Use this function to search for makeup products.
#' @keywords dataframe, endpoint, API, logical, ifelse, input
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' searchCos()
#' @export

searchCos <- function(price = TRUE, type = TRUE, brand = TRUE){
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  df$Price <- as.numeric(df$Price)
  if (price == TRUE & type == TRUE & brand == TRUE) {
    x <- readline(prompt = "Enter Your Upper Price Limit: ")
    y <- readline(prompt = "Enter Desired Product Type: ")
    z <- readline(prompt = "Enter Desired Brand: ")
    price_df <- df %>%
      filter(Price < as.numeric(x)) %>%
      filter(`Product Type` == as.character(y)) %>%
      filter(Brand == as.character(z))
    price_df[order(price_df$Price), ]
  }else if (price == FALSE & type == TRUE & brand == TRUE){
    x <- readline(prompt = "Enter Desired Product Type: ")
    y <- readline(prompt = "Enter Desired Brand: ")
    price_df <- df %>%
      filter(`Product Type` == as.character(x)) %>%
      filter(Brand == as.character(y))
    price_df[order(price_df$Price), ]
  }else if (price == TRUE & type == FALSE & brand == TRUE){
    x <- readline(prompt = "Enter Your Upper Price Limit: ")
    y <- readline(prompt = "Enter Desired Brand: ")
    price_df <- df %>%
      filter(Price < as.numeric(x)) %>%
      filter(Brand == as.character(y))
    price_df[order(price_df$Price), ]
  }else if (price == TRUE & type == TRUE & brand == FALSE){
    x <- readline(prompt = "Enter Your Upper Price Limit: ")
    y <- readline(prompt = "Enter Desired Product Type: ")
    price_df <- df %>%
      filter(Price < as.numeric(x)) %>%
      filter(`Product Type` == as.character(y))
    price_df[order(price_df$Price), ]
  }else if(price == TRUE & type == FALSE & brand == FALSE){
    x <- readline(prompt = "Enter Your Upper Price Limit: ")
    price_df <- df %>%
      filter(Price < as.numeric(x))
    price_df[order(price_df$Price), ]
  }else if (price == FALSE & type == TRUE & brand == FALSE){
    y <- readline(prompt = "Enter Desired Product Type: ")
    df %>%
      filter(`Product Type` == as.character(y))
    price_df[order(price_df$Price), ]
  }else if(price == FALSE & type == FALSE & brand == TRUE){
    z <- readline(prompt = "Enter Desired Brand: ")
    price_df <- df %>%
      filter(Brand == as.character(x))
    price_df[order(price_df$Price), ]
  }
}


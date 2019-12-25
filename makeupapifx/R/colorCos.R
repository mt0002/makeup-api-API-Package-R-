#' colorCos
#'
#' Obtain dataframe of colors of a makeup product from makeup-api API.
#'
#' This function allows the user to input a makeup product type and makeup brand to obtain a dataframe from which the user would then select the product ID to render another dataframe that shows the corresponding product ID's available colors.
#'
#' @param x Character Vector. This argument allows user to input a makeup product type the user is interested in. The string should be wrapped in quotations marks. Set to lipstick as default.
#' @param y Character Vector. This argument allows user to input a makeup brand the user is interested in. The string should be wrapped in quotations marks. Set to nyx as default.
#' @return Use this function to get dataframe of colors for a makeup product.
#' @keywords color, endpoint, API, makeup, lists
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' colorCos()
#' @export

colorCos <- function(x = "lipstick", y = "nyx") {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  new_df <- df %>%
    filter(`Product Type` == x) %>%
    filter(Brand == y)
  print(new_df)
  Sys.sleep(10)
  z <- readline(prompt = "Enter Product ID That You Want to Explore the Color of: ")
  final_df <- new_df %>%
    filter(ID == as.character(z))
  final_df$`Product Color`
}

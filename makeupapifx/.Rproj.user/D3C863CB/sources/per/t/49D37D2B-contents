#' texture
#'
#' Search makeup-api API based on description
#'
#' This function allows the user to input a desired description they want from a makeup product out of the makeup-api API.
#'Specifically, this function is designed to allow the user to input a texture of a makeup product such as "matte" or "glossy"
#'and have the ouput be a dataframe that contains products with that description.
#'
#'
#' @param x Character Vector. Input a texture or trait of a makeup product you are interested in. The string should be wrapped in quotations marks. Set to "matte" as default.
#' @return Use this function to get dataframe based on  trait of makeup product.
#' @keywords character, endpoint, API, texture, description, dataframe
#' @import stringr
#' @import httr
#' @import dplyr
#' @import jsonlite
#' @import tidyverse
#' @examples
#' texture()
#' @export

texture <- function(x = "matte") {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint)
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  df[str_detect(df$Description, x), ]
}


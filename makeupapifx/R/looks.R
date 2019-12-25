#' looks
#'
#' Obtain dataframe for suggested products to create desired look.
#'
#' This function allows user to define a look that they want. 
#' The resulting dataframe will be one that randomly selected products that are suitable for the desired look.
#' The resulting dataframe will consist of an eye, lip, and foundation makeup product. 
#'
#' @param smokey Logical Vector. Set to FALSE as default. When set to TRUE, the function will generate a dataframe that contains an eye, lip, a nd face  makeup product that works for a smokey makeup look.
#' @param natural Logical Vector. Set to FALSE as default. When set to TRUE, the function will generate a dataframe that contains an eye, lip, a nd face  makeup product that works for a natural makeup look.
#' @param glam Logical Vector. Set to FALSE as default. When set to TRUE, the function will generate a dataframe that contains an eye, lip, a nd face  makeup product that works for a glam makeup look.
#' @return Use this function to get connection status.
#' @keywords character, endpoint, API, texture, description, dataframe
#' @examples
#' texture()
#' @export

looks <- function(smokey = FALSE, natural = FALSE, glam = FALSE) {
  endpoint <- "http://makeup-api.herokuapp.com/api/v1/products.json"
  df <- jsonlite::fromJSON(endpoint) 
  colnames(df) <- c("ID", "Brand", "Product Name", "Price", "Price Sign", "Currency", "Image Link", "Product Link", "Website Link", "Description", "Rating", "Category", "Product Type", "Tag List", "Created At", "Updated   At", "Product API URL", "API Featured Image", "Product Colors")
  df <- df[!is.na(df$Price), ]
  eye <- df %>%
    filter(`Product Type` == "eyeshadow") 
  lip <- df %>%
    filter(`Product Type` == "lipstick")
  face <- df %>%
    filter(`Product Type` == "foundation")
  smokey_eye <- eye[str_detect(eye$Description, "dark"), ]
  xs <- sample(unique(smokey_eye$ID), 1)
  smokeye <- subset(smokey_eye, ID %in% xs)
  smokey_lip <- lip[str_detect(lip$Description, "matte"), ]
  ls <- sample(unique(smokey_lip$ID), 1)
  smokelip <- subset(smokey_lip, ID %in% ls)
  smokey_face <- face[str_detect(face$Description, "matte"), ]
  fs <- sample(unique(smokey_face$ID), 1)
  smokeface <- subset(smokey_face, ID %in% fs)
  natural_eye <- eye[str_detect(eye$Description, "nude"), ]
  xn <- sample(unique(natural_eye$ID), 1)
  natureye <- subset(natural_eye, ID %in% xn)
  nature_lip <- lip[str_detect(lip$Description, "nude"), ]
  ln <- sample(unique(nature_lip$ID), 1)
  naturelip <- subset(nature_lip, ID %in% ln)
  natural_face <- face[str_detect(face$Description, "nude"), ]
  fn <- sample(unique(natural_face$ID), 1)
  natureface <- subset(natural_face, ID %in% fn)
  glam_eye <- eye[str_detect(eye$Description, "glitter"), ]
  xg <- sample(unique(glam_eye$ID), 1)
  glameye <- subset(glam_eye, ID %in% xg)
  glam_lip <- lip[str_detect(lip$Description, "nude"), ]
  lg <- sample(unique(glam_lip$ID), 1)
  glamlip <- subset(glam_lip, ID %in% lg)
  glam_face <- face[str_detect(face$Description, "matte"), ]
  fg <- sample(unique(glam_face$ID), 1)
  glamface <- subset(glam_face, ID %in% fg)
  if (smokey == TRUE) {
    rbind(smokeye, smokelip, smokeface)
  } else if (natural == TRUE) {
    rbind(natureye, naturelip, natureface)
  } else if (glam == TRUE) {
    rbind(glameye, glamlip, glamface)
  }
}
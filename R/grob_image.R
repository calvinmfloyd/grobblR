#' Converts a raw .png file to a grob, with flexible aesthetics.
#'
#' @param img_path The local path to the raw .png file.
#' @param aes_list The list outputted by \code{\link{ga_list}} which gives the image grob its aesthetics.
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @return A grob of the raw .png file.
#' @export

grob_image <- function(img_path, aes_list, height = numeric(), width = numeric()){

  stopifnot(!length(height) == 0, !length(width) == 0)

  raw_png <- png::readPNG(normalizePath(file.path(img_path)))
  edit_dims <- ifelse(
    length(aes_list$maintain_aspect_ratio) == 0,
    TRUE,
    aes_list$maintain_aspect_ratio)

  if(edit_dims){

    img_hw_ratio <- dim(raw_png)[1]/dim(raw_png)[2]
    img_wh_ratio <- dim(raw_png)[2]/dim(raw_png)[1]

    height_adj <- ifelse(height >= width, width*img_hw_ratio, height)
    width_adj <- ifelse(width >= height, height*img_wh_ratio, width)

  } else {

    height_adj <- height
    width_adj <- width
  }

  grid::rasterGrob(
    raw_png,
    height = grid::unit(height_adj, "mm"),
    width = grid::unit(width_adj, "mm"))

}

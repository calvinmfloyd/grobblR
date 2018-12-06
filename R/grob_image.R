#' Converts a raw .png file to a grob, with flexible aesthetics.
#'
#' @param img_path The local path to the raw .png file.
#' @param aes_list An list which contains aesthetic parameters for the image grob. Possible aesthetic elements for .png image files are :
#' \itemize{
#' \item \code{maintain_aspect_ratio} - A TRUE/FALSE value which indicates whether the aspect ratio of the image should be maintained. Default is FALSE - meaning the image will be stretched to fit the designated grid area.
#' \item \code{hjust} - A numeric value between 0 and 1 which determines the horizontal justification of the image within the designated grid area. A value of 0 indicates shifting the image all the way to the left, and a value of 1 indicates shifting the image all the way to the right. Default is 0.5.
#' \item \code{vjust} - A numeric value between 0 and 1 which determines the vertical justification of the image within the designated grid area. A value of 0 indicates shifting the image all the way to the bottom, and a value of 1 indicates shifting the image all the way to the top. Default is 0.5.
#' }
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @return A grob of the raw .png file.
#' @export

grob_image <- function(img_path, aes_list, height = numeric(), width = numeric()){

  stopifnot(!length(height) == 0, !length(width) == 0)

  # def_vals <- list(
  #   hjust = 0.5,
  #   vjust = 0.5)
  #
  # for(val_name in names(def_vals)){
  #   if(length(aes_list[[val_name]]) == 0){
  #     aes_list[[val_name]] <- def_vals[[val_name]]
  #   }
  # }

  # for(slot_name in names(aes_list)[names(aes_list) %in% names(def_vals)]){
  #   if(aes_list[[slot_name]] < 0 | aes_list[[slot_name]] > 1) stop(sprintf("%s argument must be between 0 and 1.", val_name))
  # }

  raw_png <- png::readPNG(normalizePath(file.path(img_path)))
  edit_dims <- ifelse(length(aes_list$maintain_aspect_ratio) == 0, FALSE, aes_list$maintain_aspect_ratio)

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

#' Converts a raw .png file to a grob, with flexible aesthetics.
#'
#' @param img_path The local path to the raw .png file.
#' @param aes_list The list outputted by \code{\link{ga_list}} which gives the image grob its aesthetics.
#' @param height A numeric value designating the total height of the matrix grob in mm.
#' @param width A numeric value designating the total width of the matrix grob in mm.
#' @param units The units of the given height and width for the grob. Options are 'mm', 'cm' or 'inches', with the default of 'mm'.
#' @return A grob of the raw .png file.
#' @export

grob_image = function(img_path,
                      aes_list,
                      height = numeric(),
                      width = numeric(),
                      units = c('mm', 'cm', 'inches')) {

  stopifnot(!length(height) == 0, !length(width) == 0)
  units = match.arg(units)
  
  raw_png = png::readPNG(normalizePath(file.path(img_path)))
  edit_dims = ifelse(
    length(aes_list$maintain_aspect_ratio) == 0,
    TRUE,
    aes_list$maintain_aspect_ratio
    )
  aspect_ratio_multiplier = ifelse(
    length(aes_list$aspect_ratio_multiplier) == 0,
    1,
    aes_list$aspect_ratio_multiplier
    )

  if (edit_dims) {

    img_height_width_ratio = dim(raw_png)[1]/dim(raw_png)[2]
    img_width_height_ratio = dim(raw_png)[2]/dim(raw_png)[1]
    
    if (height >= width) {
      
      width_adj = width
      height_adj = width*img_height_width_ratio
      
    } else if (height < width) {
      
      height_adj = height
      width_adj = height*img_width_height_ratio
      
    }

  } else {

    height_adj = height
    width_adj = width
    
  }

  grid::rasterGrob(
    raw_png,
    height = grid::unit(height_adj*aspect_ratio_multiplier, units = units),
    width = grid::unit(width_adj*aspect_ratio_multiplier, units = units)
    )

}

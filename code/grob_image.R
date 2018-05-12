#' Converts a raw .png file to a grob, with flexible aesthetics. Used within
#' the GrobblR::convert_to_grob() function.
#'
#' @param img_path The local path to the raw .png file.
#' @param gi_obj An object of class "grob_image" which contains aesthetic parameters for
#' the image grob. Created within the GrobblR::convert_to_grob() function.
#' @param tot_height A numeric value designating the total height of the matrix grob in mm.
#' @param tot_width A numeric value designating the total width of the matrix grob in mm.
#' @return A grob of the raw .png file.
#' @export

grob_image <- function(img_path, gi_obj, tot_height = numeric(), tot_width = numeric()){
  
  stopifnot(!length(tot_height) == 0, !length(tot_width) == 0)
  
  def_vals <- list(
    hjust = 0.5,
    vjust = 0.5)
  
  for(val_name in names(def_vals)){
    if(length(slot(gi_obj, val_name)) == 0){
      slot(gi_obj, val_name) <- def_vals[[val_name]]
    }
  }
  
  for(slot_name in slotNames(gi_obj)[slotNames(gi_obj) %in% names(def_vals)]){
    if(slot(gi_obj, slot_name) < 0 | slot(gi_obj, slot_name) > 1) stop(sprintf("%s argument must be between 0 and 1.", val_name))
  }
  
  raw_png <- readPNG(normalizePath(file.path(img_path)))
  edit_dims <- ifelse(length(gi_obj@maintain_aspect_ratio) == 0, FALSE, gi_obj@maintain_aspect_ratio)
  if(edit_dims){

    img_hw_ratio <- dim(raw_png)[1]/dim(raw_png)[2]
    img_wh_ratio <- dim(raw_png)[2]/dim(raw_png)[1]

    height_adj <- ifelse(img_hw_ratio >= 1, tot_height, tot_width*img_hw_ratio)
    width_adj <- ifelse(img_hw_ratio >= 1, tot_height*img_wh_ratio, tot_width)
     
  } else {
    
    height_adj <- tot_height
    width_adj <- tot_width
    
  }
  
  rasterGrob(
    raw_png,
    height = unit(height_adj, "mm"),
    width = unit(width_adj, "mm"),
    hjust = gi_obj@hjust,
    vjust = gi_obj@vjust)
  
}
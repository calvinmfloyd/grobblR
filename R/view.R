
#' View a grob with a given width and height.
#' @param grob An object outputted by one of the following functions:
#' \itemize{
#' 
#' \item \code{\link{grob_matrix}}
#' \item \code{\link{grob_image}}
#' \item \code{\link{grob_row}}
#' \item \code{\link{grob_col}}
#' \item \code{\link{grob_layout}}
#' 
#' }
#' @param height The numeric height the user wishes to view the grob in.
#' 
#' @param width The numeric width the user wishes to view the grob in.
#' 
#' @param units The units the provided height and width are in.
#' 
#' @export
#' 
#' @examples 
#' 
#' data.frame(
#'   v2 = c(15, 4, 16, 11),
#'   v2 = c(10, 30, 3, 10)
#'   ) %>%
#'   grob_matrix() %>%
#'   view_grob()
#' 

view_grob = function(grob,
                     height = 100,
                     width = 100,
                     units = c('mm', 'cm', 'inches')) {
  
  units = match.arg(units)
  is_grob_object = any(is(grob) %in% c('grob_matrix_object', 'grob_image_object'))
  is_grob_layout = any(is(grob) %in% c('grob_layout'))
  is_grob_row = is(grob, 'grob_row')
  is_grob_col = is(grob, 'grob_col')

  if (!any(is_grob_object, is_grob_layout, is_grob_row, is_grob_col)) {
    
    accepted_functions = c('grob_layout()', 'grob_matrix()', 'grob_image()', 'grob_row()', 'grob_col()')
    error_msg = glue::glue("
      Please provide view_grob() with an object outputted by one of: {paste(accepted_functions, collapse = ', ')}.
      ")
  
    stop(error_msg, call. = FALSE)

  }
  
  if (is_grob_object) {

    height = units_convert(x = height, from_units = units, to_units = 'mm')
    width = units_convert(x = width, from_units = units, to_units = 'mm')
    
    gc = grob_col(grob, width = width)
    gc$height = height
    
    gridExtra::grid.arrange(gc$grob)
     
  } else if (any(is_grob_row, is_grob_col, is_grob_layout)) {
    
    height = units_convert(x = height, from_units = units, to_units = 'mm')
    width = units_convert(x = width, from_units = units, to_units = 'mm')
    
    grob$height = height
    grob$width = width
    grob$units = 'mm'
    gridExtra::grid.arrange(grob$grob)
    
  } else {
    
    return(invisible())
    
  }
  

}
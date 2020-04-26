
#' View Grob
#' 
#' View an grob outputted by one of the \code{grob_} functions with a given width and
#' height.
#' 
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
#' @param height The numeric height in millimeters the user wishes to view the grob in.
#' 
#' @param width The numeric width in millimeters the user wishes to view the grob in.
#' 
#' @export
#' 
#' @details Plotted with \code{\link[gridExtra]{grid.arrange}}.
#' 
#' @examples 
#' 
#' df = data.frame(
#'   x = c(15, 4, 16, 11),
#'   y = c(10, 30, 3, 10)
#'   ) 
#'   
#' df %>%
#'   grob_matrix() %>%
#'   view_grob()
#'   
#' gg = ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = x, y = y)) +
#'   ggplot2::geom_line(color = 'red')
#'   
#' gg %>%
#'   grob_image() %>%
#'   view_grob()
#' 

view_grob = function(grob,
                     height = NA_real_,
                     width = NA_real_) {
  
  default_height = 100
  default_width = 100
  is_grob_object = any(methods::is(grob) %in% c('grob_matrix_object', 'grob_image_object'))
  is_grob_layout = any(methods::is(grob) %in% c('grob_layout'))
  is_grob_row = methods::is(grob, 'grob_row')
  is_grob_col = methods::is(grob, 'grob_col')
  
  if (!any(is_grob_object, is_grob_layout, is_grob_row, is_grob_col)) {
    
    accepted_functions = c('grob_layout()', 'grob_matrix()', 'grob_image()', 'grob_row()', 'grob_col()')
    error_msg = glue::glue("
      Please provide view_grob() with an object outputted by one of: {paste(accepted_functions, collapse = ', ')}.
      ")
  
    stop(error_msg, call. = FALSE)

  }
  
  if (is_grob_object) {

    height = ifelse(is.na(height), default_height, height)
    width = ifelse(is.na(width), default_width, width)
    
    gc = grob_col(grob, width = width)
    gc$height = height
    gc$grob_layout_location = 'grob-object'
    
    gridExtra::grid.arrange(gc$grob)

  } else if (any(is_grob_row, is_grob_col, is_grob_layout)) {
    
    location = dplyr::case_when(
      is_grob_layout ~ 'grob-layout',
      is_grob_row ~ 'grob-row',
      is_grob_col ~ 'grob-column',
      )
    
    height = dplyr::case_when(
      !is.na(height) ~ height,
      !is.na(grob$height) ~ grob$height,
      TRUE ~ default_height
      )
    
    width = dplyr::case_when(
      !is.na(width) ~ width,
      !is.na(grob$width) ~ grob$width,
      TRUE ~ default_width
      )

    grob$height = height
    grob$width = width
    grob$grob_layout_location = location
    gridExtra::grid.arrange(grob$grob)
    
  }
  

}
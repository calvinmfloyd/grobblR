
#' Initialize a grob matrix object, to be used within \code{\link{grob_col}}.
#' @param x Either a data.frame, a matrix or a vector.
#' 
#' @return An R6 object of the grob matrix class.
#' 
#' @export
#' 
#' @examples 
#' 
#' data.frame(
#'   v1 = c(15, 4, 16, 11),
#'   v2 = c(10, 30, 3, 10)
#'   ) %>%
#'   grob_matrix() %>%
#'   view_grob()
#' 

grob_matrix = function(x) {
  
  if (!any(is.data.frame(x), is.matrix(x), is.vector(x))) {
    
    error_msg = glue::glue("
      The object passed through grob_matrix() must either be a \\
      data.frame, matrix or a vector.
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  initial_type = dplyr::case_when(
    is.data.frame(x) ~ 'data.frame',
    is.matrix(x) ~ 'matrix',
    is.vector(x) ~ 'vector'
    )
    
  if (initial_type == 'vector') {
      
    initial = convert_to_matrix(x)
    
  } else {
      
    initial = x
    
  }
  
  grob_object = grob_matrix_object$new(
    initial = initial,
    type = 'matrix'
    )
  
  test = initial

  if (nrow(test) > 0) {
    
    test = data.frame(test, stringsAsFactors = TRUE)
    test[['grobblR_group']] = 'cells'
  
  }
    
  grob_object$test = test
  grob_object$current = convert_to_matrix(initial)
  grob_object = column_names_to_row(grob_object)
  
  return(grob_object)

}

#' Initialize a grob text object, to be used within \code{\link{grob_col}}.
#' @param x A single character string.
#' 
#' @return An R6 object of the grob matrix class.
#' 
#' @export
#' 
#' @examples 
#' 

grob_text = function(x) {
  
  if (!(is.character(x) & length(x) == 1)) {
    
    error_msg = glue::glue("
      The object passed through grob_text() must be a single character string.
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  initial = convert_to_matrix(x)
  
  grob_object = grob_matrix_object$new(
    initial = initial,
    type = 'text'
    )
  
  test = initial

  if (nrow(test) > 0) {
    
    test = data.frame(test, stringsAsFactors = TRUE)
    test[['grobblR_group']] = 'cells'
  
  }
  
  grob_object$current = initial
  grob_object$test = test
  
  return(grob_object)

}

#' Initialize a grob image object, to be used within \code{\link{grob_col}}.
#' @param x Either a ggplot object or a file path to png image.
#' 
#' @return An R6 object of the grob image class.
#' 
#' @export
#' 
#' @examples 
#' 
#'

grob_image = function(x) {
  
  is_ggplot = ggplot2::is.ggplot(x)
  is_existing_png = FALSE
  
  if (is.character(x)) {
    
    is_existing_png = file.exists(x) & (tools::file_ext(x) %in% 'png')
    
  }
  
  if (!any(is_ggplot, is_existing_png)) {
    
    error_msg = glue::glue("
      The object passed through grob_image() must either be a \\
      ggplot object or a file path to a png image.
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  grob_object = grob_image_object$new(initial = x)
  
  return(grob_object)

}

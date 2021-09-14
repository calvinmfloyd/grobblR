
#' Grob Matrix
#' 
#' Initialize a grob matrix object, to be used within \code{\link{grob_col}}.
#' 
#' Learn more in \code{vignette("grob_matrix")}
#' 
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

    # - If a matrix is passed through, and it doesn't have column names, 
    # we will first convert it to a data.frame (to give it default column names)
    # without warnings, in order for it to conform to the column name standards
    # of tibble::as_tibble()
    if (initial_type == 'matrix' & is.null(colnames(test))) {
      
      test = tibble::as_tibble(as.data.frame(test, stringsAsFactors = FALSE))
    
    # - Otherwise, send it straight through to be converted to a tibble  
    } else {
      
      # - Suppressing messages due to potential name repair messages from tibble
      test = suppressMessages({
        test %>%
        tibble::as_tibble(.name_repair = "unique")
      })
      
    }
    
    test[['grobblR_group']] = 'cells'
  
  }
    
  grob_object$test = test
  grob_object$current = convert_to_matrix(initial)
  grob_object = column_names_to_row(grob_object)
  return(grob_object)

}

#' Grob Text
#' 
#' Initialize a grob text object, to be used within \code{\link{grob_col}}.
#'
#' @param x A single character string.
#' 
#' @return An R6 object of the grob matrix class.
#' 
#' @export
#' 
#' @examples 
#' 
#' "The quick brown fox jumps over the lazy dog" %>%
#'   grob_text() %>%
#'   view_grob()
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
    
    test = tibble::as_tibble(as.data.frame(test, stringsAsFactors = FALSE))
    test[['grobblR_group']] = 'cells'
  
  }
  
  grob_object$current = initial
  grob_object$test = test
  
  return(grob_object)

}

#' Grob Image
#' 
#' Initialize a grob image object, to be used within \code{\link{grob_col}}.
#' 
#' @param x Either a \code{ggplot} object, a file path to .png image or a URL
#' to a .png image.
#' 
#' @return An R6 object of the grob image class.
#' 
#' @export
#' 
#' @examples 
#' 
#' gg = data.frame(x = c(5, 14, 6, 10), y = c(3, 30, 17, 7)) %>%
#'   ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y)) +
#'   ggplot2::geom_line(color = 'red') 
#'   
#' gg %>%
#'   grob_image() %>%
#'   view_grob()
#'

grob_image = function(x) {
  
  is_ggplot = ggplot2::is.ggplot(x)
  is_existing_png_file_path = FALSE
  is_existing_png_url = FALSE
  
  if (is.character(x)) {
    
    is_existing_png_file_path = file.exists(x) & (tools::file_ext(x) %in% "png")
    is_valid_png_url = check_is_valid_url(x = x) & grepl(".png", x)
    is_existing_png_url = FALSE
    
    if (is_valid_png_url) {
      
      is_existing_png_url = RCurl::url.exists(url = x)
      
    }
    
  }
  
  if (!any(is_ggplot, is_existing_png_file_path, is_existing_png_url)) {
    
    error_msg = glue::glue("
      The object passed through grob_image() must either be a \\
      ggplot object, an existing URL to a .png image or an existing local file \\
      path to a .png image.
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  grob_object = grob_image_object$new(initial = x)
  
  return(grob_object)

}

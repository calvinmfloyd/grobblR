
#' Add an Aesthetic
#' 
#' Add an aesthetic to a grob matrix object.
#' 
#' @param grob_object The R6 object outputted by either \code{\link{grob_matrix}}
#' or \code{\link{grob_text}}.
#' 
#' @param aesthetic The matrix aesthetic the user wishes to add.
#' 
#' @param group The group of the grob matrix object the user wants to add the 
#' aesthetic to. 
#' 
#' For objects initialized by \code{\link{grob_matrix}}, the user can add an aesthetic to the 'cells', the
#' 'column_names' or the 'column_headings'. If the user is passing through an object 
#' initialized by \code{\link{grob_text}}, then only 'cells' will be accepted.
#' 
#' @param value A single value or a matrix of values the user wants to apply to the group of matrix / text 
#' elements for the given aesthetic.
#' 
#' If a matrix of values is supplied, then the matrix must be of the same dimensions
#' as the chosen subset of the matrix / text.
#' 
#' @return The R6 object of the grob matrix class with its aesthetics properties altered.
#' 
#' @export
#' 
#' @details 
#' 
#' Accepted aesthetics:
#' 
#' \describe{
#' 
#' \item{Matrix / Text}{
#' 
#' \itemize{
#' 
#' \item \code{background_alpha}
#' \item \code{background_color}
#' \item \code{border_color}
#' \item \code{border_sides}
#' \item \code{border_width}
#' \item \code{font_face}
#' \item \code{group_elements}
#' \item \code{replace_na}
#' \item \code{round_rect_radius}
#' \item \code{text_align}
#' \item \code{text_cex}
#' \item \code{text_font}
#' \item \code{text_color}
#' \item \code{text_just}
#' \item \code{text_v_align}
#' \item \code{text_v_just}
#' \item \code{text_rot}
#' 
#' }
#' }
#' 
#' }
#' 
#' To see descriptions of the aesthetics above, see the documentation of \code{\link{ga_list}}.
#' 
#' @examples
#' 
#' df = data.frame(var1 = c(5, 14, 6, 10), var2 = c(3, 30, 17, 7))
#' df %>%
#'   grob_matrix() %>% 
#'   add_aesthetic(aesthetic = 'text_color', value = 'red', group = 'cells') %>%
#'   view_grob()
#' 

add_aesthetic = function(grob_object,
                         aesthetic,
                         value = NULL,
                         group = c('cells', 'column_names', 'column_headings')) {
  
  group = match.arg(group)
  is_grob_matrix = methods::is(grob_object, 'grob_matrix_object')
  
  # - Checking to make sure it's a valid grob object
  if (!is_grob_matrix) {
    
     error_msg = glue::glue("
       Please provide an object initialized by grob_matrix() or grob_text() in add_aesthetic().
       ")
    
    stop(error_msg, call. = FALSE)
     
  }
  
  aesthetic = check_aesthetic(aesthetic = aesthetic, type = 'matrix', location = 'add_aesthetic()')
  group = check_group(group = group, test = grob_object$test, location = 'add_aesthetic()')
  value = convert_to_matrix(value)
  value = check_matrix_aesthetic_value(
    value = value,
    df = grob_object$test,
    group = group,
    location = "add_aesthetic()",
    type = aesthetic
    )
  
  if (!is.null(grob_object$aesthetic_list[[aesthetic]])) {
    
    mat = grob_object$aesthetic_list[[aesthetic]]
    
  # - If the aesthetic is NULL within the aesthetic list, then we will initialize
  # the aesthetic matrix with NA's.
  } else {
  
    mat = aes_matrix(df = grob_object$current, value = NA)
    
  }

  mat[grob_object$test[['grobblR_group']] %in% group,] = value
  
  grob_object$current_aesthetic = aesthetic
  grob_object$current_group = group
  grob_object$last_edit = "aesthetic"
  grob_object$aesthetic_list[[aesthetic]] = mat

  return(grob_object)
  
}

check_aesthetic = function(aesthetic, type, location) {
  
  valid_aesthetics = matrix_aesthetics
  
  if (!aesthetic %in% valid_aesthetics) {
    
    error_msg = glue::glue("
      aesthetic parameter within {location} must be one of: \\
      {paste(valid_aesthetics, collapse = ', ')}
      ")
    
    stop(error_msg, call. = FALSE)
  
  }
  
  return(aesthetic)
  
}

check_group = function(group, test, location) {
  
  valid_groups = unique(test[['grobblR_group']])
    
  if (!group %in% valid_groups) {
    
    error_msg = glue::glue("
      group within {location} must be one of: \\
      {paste(valid_groups, collapse = ', ')}. Attempted to apply an aesthetic to \\
      the {group}.
      ")
    
    stop(error_msg, call. = FALSE)

  }
  
  return(group)
  
}

check_matrix_aesthetic_value = function(value,
                                        df,
                                        group,
                                        location,
                                        type) {
  
  dim_of_df = dim(df[df[["grobblR_group"]] %in% group, !colnames(df) %in% "grobblR_group"])
  dim_of_value = dim(value)
  
  if (!all(dim_of_df == dim_of_value) & !all(dim_of_value == 1)) {
    
    error_msg = glue::glue("
      The value provided in {location} for '{type}' of the {group} must either be \\
      of the dimensions {dim_of_df[1]}x{dim_of_df[2]} or a single value.
      The inputted dimensions are {dim_of_value[1]}x{dim_of_value[2]}.
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  return(value)

}


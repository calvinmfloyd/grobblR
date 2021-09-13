#' Take a data.frame/matrix and insert its column names as the 
#' first row of the resulting matrix.
#' @param df The data.frame/matrix.
#' @return A matrix of the initial data.frame/matrix with its column
#' names as the first row.

column_names_to_row = function(df) {

  aesthetic_list_unedited = purrr::every(.x = df$aesthetic_list, .p = is.null)
  
  if (!aesthetic_list_unedited) {

    stop(
      call. = FALSE,
      "The user must convert column names to rows before making any aesthetic changes."
      )

  }
  
  if (length(colnames(df$initial)) > 0) {
  
    mat = df$current
    column_names = colnames(mat)
    mat = mat %>% tibble::as_tibble(.name_repair = "minimal")
    mat_w_column_names = rbind(column_names, mat)
    df$current = mat_w_column_names
    df$test = add_extra_row_to_df(df = df$test, row_name_label = 'column_names')
    df$column_names_to_row = df$column_names_to_row + 1
    
  }
  
  return(df)
  
}

add_extra_row_to_df = function(df, row_name_label) {
  
  if (nrow(df) > 0) {
    
    df = rbind(NA, df)
    df[['grobblR_group']][1] = row_name_label
    
  }
  
  return(df)

}


#' Add column headings to grob matrix
#' 
#' Add column headings onto an object initialized by \code{\link{grob_matrix}}.
#' 
#' @param mat The grob matrix object the column headings will be added onto.
#' 
#' @param headings The headings to be added onto the initial matrix, 
#' in a list with each heading a separate element. The list must have
#' the same amount of elements as the \code{heading_cols} parameter.
#' 
#' @param heading_cols Which column positions of the initial matrix the \code{headings} 
#' will be placed above, in a list with each heading's column positions a separate element. 
#' The list must have the same amount of elements as the \code{headings} parameter.
#' 
#' Can either be numeric indices, or column names of the initial data frame / matrix
#' passed through \code{\link{grob_matrix}}.
#' 
#' Default is an empty list. If unaltered, the function will assume the user
#' wants to apply \code{headings} to all columns of the \code{\link{grob_matrix}} - 
#' in which case only one \code{headings} is allowed.
#' 
#' @return The initial grob matrix object with column headings inserted into the
#' appropriate areas.
#' 
#' @details The user must add column headings \strong{before} adding or altering
#' any aesthetics.
#' 
#' @export
#' 
#' @examples 
#' 
#' data.frame(var1 = c(5, 14, 6, 10), var2 = c(3, 30, 17, 7)) %>%
#'   grob_matrix() %>% 
#'   add_column_headings(c('HEADING')) %>%
#'   view_grob()
#' 

add_column_headings = function(mat, headings = list(), heading_cols = list()) {
  
  if (!is.list(headings) & !is.list(heading_cols)) {
    
    stop(
      call. = FALSE,
      "Both headings and heading_cols must be lists within add_column_headings()."
      )
    
  }
  
  # - If no heading_cols are provided then it will be assumed that the user wants
  # to apply a single heading above all the columns
  no_heading_cols_provided = length(heading_cols) == 0
  if (no_heading_cols_provided) {
    
    if (length(headings) != 1) {
      
      error_msg = glue::glue("
        In add_column_headings(), if heading_cols is left as an empty list, only a list \\
        with a length of one within headings is allowed.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
      
    heading_cols = list(1:ncol(mat$current))
      
  }
  
  same_length_check = length(headings) != length(heading_cols)
  nonzero_length_check = length(headings) > 0 & length(heading_cols) > 0
  
  if (same_length_check & nonzero_length_check) {
  
    stop(
      call. = FALSE,
      "Both headings and heading_cols must be the same, non-zero length within add_column_headings()."
      )
    
  }
  
    
  if (mat$type != 'matrix') {
    
    error_msg = glue::glue("
      Only an object initialized with grob_matrix() can be passed through \\
      add_column_headings().
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  if (length(mat$aesthetic_list) > 0) {
    
    error_msg = glue::glue("
      Please do not alter matrix aesthetics before adding column headings with \\
      add_column_headings().
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  mat$test = add_extra_row_to_df(df = mat$test, row_name_label = 'column_headings')

  # - Users will have the option to pass in column indices or actual column
  # names, so we will loop through the headings and evaluate each time whether
  # the user passed in column indices or column names.
  column_headings = rep(" ", ncol(mat$current))
  for (i in 1:length(headings)) {
    
    is_column_index = is.numeric(heading_cols[[i]])
    
    if (is_column_index) {
      
      which_indices = heading_cols[[i]]
      
      if (!all(dplyr::between(x = abs(which_indices), left = 1, right = ncol(mat$initial)))) {
        
        error_msg = glue::glue("
          If providing numeric column indices in add_column_headings(), please make sure they are all between \\
          1 and {ncol(mat$initial)}.
          ")
        
        stop(error_msg, call. = FALSE)

      }

      
    } else {
    
      which_indices = which(colnames(mat$initial) %in% heading_cols[[i]])
  
      if (length(which_indices) == 0) {
        
        warning_msg = glue::glue("
          None of the inputted column names '{paste(heading_cols[[i]], collapse = \"', '\")}' \\
          in add_column_headings() match the column names of the initial data frame / matrix.
          ")
        
        warning(warning_msg, call. = FALSE)
        
      }

    
    }
    
    column_headings[which_indices] = headings[[i]]
  
  }
  
  mat$current = rbind(column_headings, mat$current)
  mat$column_headings_added = mat$column_headings_added + 1
  
  return(mat)
  
}

#' Alter column names of a grob matrix
#' 
#' Alter column names of an object initialized by \code{\link{grob_matrix}}.
#' 
#' @param mat The grob matrix object the column names will be edited in.
#' 
#' @param column_names The replacement column names, 
#' in a list with each column name a separate element. The list must have
#' the same amount of elements as the \code{column_name_cols} parameter.
#' 
#' @param column_name_cols Which column positions of the initial data frame / matrix the \code{column_names} 
#' will replace, in a list with each column name's column positions a separate element. 
#' The list must have the same amount of elements as the \code{column_names} parameter.
#' 
#' Can either be numeric indices, or column names of the initial data frame / matrix
#' passed through \code{\link{grob_matrix}}.
#' 
#' Default is an empty list. If unaltered, the function will assume the user
#' wants to apply \code{column_names} to all columns of the \code{\link{grob_matrix}} - 
#' in which case only one \code{column_names} is allowed.
#' 
#' @param group_elements A boolean argument on whether like, adjacent column names 
#' should be grouped together.
#' 
#' @details The user can only use this function if the initial data frame / matrix
#' passed through \code{\link{grob_matrix}} had column names to begin with.
#' 
#' The underlying column names will be unaffected. So, if the user wants to use 
#' \code{\link{alter_at}} afterwards, he/she should select the original column names
#' and not the replacements from this function.
#' 
#' @return The initial grob matrix object with column names edited in the appropriate
#' areas.
#' 
#' @export
#' 
#' @examples 
#' 
#' data.frame(var1 = c(5, 14, 6, 10), var2 = c(3, 30, 17, 7)) %>%
#'   grob_matrix() %>% 
#'   alter_column_names(c('COLUMN NAME')) %>%
#'   view_grob()
#' 

alter_column_names = function(mat,
                              column_names = list(),
                              column_name_cols = list(),
                              group_elements = TRUE) {
  
  
  if (!is.list(column_names) & !is.list(column_name_cols)) {
    
    stop(
      "Both column_names and column_name_cols must be lists within alter_column_names().",
      call. = FALSE
      )
    
  }
  
  # - If no column_name_cols are provided then it will be assumed that the user wants
  # to apply a single column name to all the columns
  no_column_name_cols_provided = length(column_name_cols) == 0
  if (no_column_name_cols_provided) {
    
    if (length(column_names) != 1) {
      
      error_msg = glue::glue("
        In alter_column_names(), if column_name_cols is left as an empty list, only a list \\
        with a length of one within column_names is allowed.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
      
    column_name_cols = list(1:ncol(mat$current))
      
  }
  
  same_length_check = length(column_names) != length(column_name_cols)
  nonzero_length_check = length(column_names) > 0 & length(column_name_cols) > 0
  
  if (same_length_check & nonzero_length_check) {
  
    stop(
      "Both column_names and column_name_cols must be the same, non-zero length within alter_column_names().",
      call. = FALSE
      )
    
  }
    
  if (mat$type != 'matrix') {
    
    error_msg = glue::glue("
      Only an object initialized with grob_matrix() can be passed through \\
      alter_column_names().
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  if (is.null(colnames(mat$initial))) {
    
    error_msg = glue::glue("
      Please initially pass a data frame / matrix with column names through \\
      grob_matrix() if utilizing alter_column_names().
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  if (is.null(mat$aesthetic_list[["group_elements"]])) {
  
    mat$aesthetic_list[["group_elements"]] = aes_matrix(
      df = mat$current,
      value = NA
      )
  
  }
  
  column_name_row = which(mat$test[["grobblR_group"]] %in% "column_names")
  
  # - Users will have the option to pass in column indices or actual column
  # names, so we will loop through the headings and evaluate each time whether
  # the user passed in column indices or column names.
  for (i in 1:length(column_names)) {
    
    is_column_index = is.numeric(column_name_cols[[i]])
    
    if (is_column_index) {
      
      which_indices = column_name_cols[[i]]
      
      if (!all(dplyr::between(x = abs(which_indices), left = 1, right = ncol(mat$initial)))) {
        
        error_msg = glue::glue("
          If providing numeric column indices in alter_column_names(), please make sure they are all between \\
          1 and {ncol(mat$initial)}.
          ")
        
        stop(error_msg, call. = FALSE)

      }

    } else {
    
      which_indices = which(colnames(mat$initial) %in% column_name_cols[[i]])
      
      if (length(which_indices) == 0) {
        
        warning_msg = glue::glue("
          None of the inputted column names '{paste(column_name_cols[[i]], collapse = \"', '\")}' \\
          in alter_column_names() match the column names of the initial data frame / matrix.
          ")
        
        warning(warning_msg, call. = FALSE)
        
      }
      
    }
  
    mat$current[column_name_row, which_indices] = column_names[[i]]
    mat$aesthetic_list[["group_elements"]][column_name_row, which_indices] = group_elements
    
  
  }

  return(mat)
  
}



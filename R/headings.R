#' Take a data.frame/matrix and insert its column names as the 
#' first row of the resulting matrix.
#' @param df The data.frame/matrix.
#' @return A matrix of the initial data.frame/matrix with its column
#' names as the first row.
#' @export 

column_names_to_row = function(df) {

  if ('grob_matrix_object' %in% is(df)) {
  
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
      mat = data.frame(mat, stringsAsFactors = FALSE)
      colnames(mat) = NULL
      
      mat_w_column_names = rbind(column_names, mat)
      df$current = mat_w_column_names
      df$test = add_extra_row_to_df(df = df$test, row_name_label = 'column_names')
      df$column_names_to_row = df$column_names_to_row + 1
      
    }
    
    return(df)
    
  } else {
  
    mat = convert_to_matrix(df)
    grob_col_names = colnames(mat)
    colnames(mat) = NULL
    mat_w_column_names = rbind(grob_col_names, mat)
    return(mat_w_column_names)
    
  }
  
}

add_extra_row_to_df = function(df, row_name_label) {
  
  if (nrow(df) > 0) {
    
    df = rbind(NA, df)
    df[['grobblR_group']][1] = row_name_label
    
  }
  
  return(df)

}


#' Add column headings onto a matrix. Intended to be used with
#' the group_elements aesthetic within \code{\link{grob_col}}.
#' @param mat The matrix the column headings will be added onto.
#' @param headings The headings to be added onto the initial matrix, 
#' in a list with each heading a separate element. The list must have
#' the same amount of elements as the \code{heading_cols} parameter.
#' @param heading_cols Which column positions of the initial matrix the \code{headings} 
#' will be placed above, in a list with each heading's column positions a separate element. 
#' The list must have the same amount of elements as the \code{headings} parameter.
#' @return The initial matrix with column headings inserted into the 
#' first row.
#' @export

add_column_headings = function(mat, headings, heading_cols) {
  
  if (!is.list(headings) & !is.list(heading_cols)) {
    
    stop(
      call. = FALSE,
      "Both column headings and heading_cols must be lists within add_column_headings()."
      )
    
  }
  
  if (length(headings) != length(heading_cols)) {
  
    stop(
      call. = FALSE,
      "Both column headings and heading_cols must be the same length within add_column_headings()."
      )
    
  }
  
  # - For the new aesthetic object process
  if ('grob_matrix_object' %in% is(mat)) {
      
    mat$test = add_extra_row_to_df(df = mat$test, row_name_label = 'column_headings')

    # - Users will have the option to pass in column indices or actual column
    # names, so we will loop through the headings and evaluate each time whether
    # the user passed in column indices or column names.
    column_headings = rep(" ", ncol(mat$current))
    for (i in 1:length(headings)) {
      
      is_column_index = is.numeric(heading_cols[[i]])
      
      if (is_column_index) {
        
        column_headings[heading_cols[[i]]] = headings[[i]]
        
      } else {
      
        which_indices = which(colnames(mat$initial) %in% heading_cols[[i]])
        column_headings[which_indices] = headings[[i]]
      
      }
    
    }
    
    mat$current = rbind(column_headings, mat$current)
    mat$column_headings_added = mat$column_headings_added + 1
    
    return(mat)
  
  # - For the old process of adding column headings
  } else {
 
    grob_column_heading = rep(" ", ncol(mat))
    for (i in 1:length(headings)) {
      
      is_column_index = is.numeric(heading_cols[[i]])
      
      if (is_column_index) {
        
        column_headings[heading_cols[[i]]] = headings[[i]]
        
      } else {
      
        which_indices = which(colnames(mat@initial) %in% heading_cols[[i]])
        column_headings[which_indices] = headings[[i]]
      
      }
    
    }
    
    mat_w_column_headings = rbind(grob_column_heading, mat)
    return(mat_w_column_headings)
    
  }
  
}

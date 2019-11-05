#' Take a data.frame/matrix and insert its column names as the 
#' first row of the resulting matrix.
#' @param df The data.frame/matrix.
#' @return A matrix of the initial data.frame/matrix with its column
#' names as the first row.
#' @export 

column_names_to_row = function(df) {
  
  mat = convert_to_matrix(df)
  col_names = colnames(mat)
  colnames(mat) = NULL
  mat_w_column_names = rbind(col_names, mat)
  rownames(mat_w_column_names) = NULL
  mat_w_column_names

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

add_column_headings = function(mat, headings = list(), heading_cols = list()) {
  
  if (!is.list(headings) & !is.list(heading_cols)) {
    
    stop("Both headings and heading_cols must be lists within add_column_headings().")
    
  }
  
  if (length(headings) != length(heading_cols)) {
  
    stop("Both headings and heading_cols must be the same length within add_column_headings().")
    
  }
 
  num_elements = length(headings)
  column_heading = rep(" ", ncol(mat))
  
  for (i in 1:num_elements) {
    
    column_heading[heading_cols[[i]]] = headings[[i]]
  
  }
  
  mat_w_column_headings = rbind(column_heading, mat)
  rownames(mat_w_column_headings) = NULL
  return(mat_w_column_headings)
  
}

#' Add row headings onto a matrix. Intended to be used with
#' the \code{group_elements} aesthetic within \code{\link{grob_col}} and \code{\link{ga_list}}.
#' @param mat The matrix the column headings will be added onto.
#' @param headings The headings to be added onto the initial matrix, 
#' in a list with each heading a separate element. The list must have
#' the same amount of elements as the \code{heading_rows} parameter.
#' @param heading_rows Which row positions of the initial matrix the \code{headings} 
#' will be placed to the left of, in a list with each heading's row positions a separate element. 
#' The list must have the same amount of elements as the \code{headings} parameter.
#' @return The initial matrix with row headings inserted into the 
#' first column.
#' @export

add_row_headings = function(mat, headings = list(), heading_rows = list()) {
  
  if (!is.list(headings) & !is.list(heading_rows)) {
    
    stop("Both headings and heading_rows must be lists within add_row_headings().")
    
  }
  
  if (length(headings) != length(heading_rows)) {
  
    stop("Both headings and heading_rows must be the same length within add_row_headings().")
    
  }
 
  num_elements = length(headings)
  row_heading = rep(" ", nrow(mat))
  
  for (i in 1:num_elements) {
    
    row_heading[heading_rows[[i]]] = headings[[i]]
  
  }
  
  mat_w_row_headings = cbind(row_heading, mat)
  colnames(mat_w_row_headings) = NULL
  return(mat_w_row_headings)
  
}


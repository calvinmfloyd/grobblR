#' Create a matrix based off the dimensions of a data.frame/matrix and a single 
#' value to make up its cells. Designed to be used as an aesthetic matrix within
#' \code{\link{ga_list}}.
#'
#' @param df A data.frame/matrix the resulting matrix will get its dimensions from.
#' @param value The single value that will make up the cells of the resulting matrix.
#' @param column_names A TRUE/FALSE value indicating if the resulting aesthetic
#' matrix is intended to be used for the column names.
#' @return A matrix based on the dimensions of \code{df} and \code{value}.
#' @examples 
#' df = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' aes_matrix(df, 'white')
#' @export

aes_matrix = function(df, value, column_names = FALSE) {
  
  value = check_value(value = value)
  if (column_names) {
    
    df = matrix(1, nrow = 1, ncol = ncol(df), dimnames = list(NULL, colnames(df)))
    
  }
  
  matrix(
    data = value,
    nrow = nrow(df),
    ncol = ncol(df),
    dimnames = list(rownames(df), colnames(df))
    )
  
}

#' Alter the values at specific rows of a matrix.
#'
#' @param mat The matrix the user wishes to alter cells of.
#' @param value The single value that will replace specific cells of the matrix.
#' @param rows The rows the user wishes to alter. Can be numeric row positions, or 
#' the user can also input:
#' \itemize{
#' \item 'odd' - Only alter odd numbered rows.
#' \item 'even' - Only alter even numbered rows.
#' \item 'first' - Only alter the first row.
#' \item 'last' - Only alter the last row.
#' }
#' Also, the user can provide the row name of the column they wish to alter.
#' @return A matrix with the desired rows altered.
#' @examples 
#' df = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' mat = aes_matrix(df, 'white')
#' alter_rows(mat = mat, value = 'red', rows = c(1, 2))
#' @export

alter_rows = function(mat, value, rows = NULL) {
  
  value = check_value(value = value)
  rows = check_rows(mat = mat, rows = rows)
  
  mat[rows, 1:ncol(mat)] = value
  mat

}

#' Alter the values at specific columns of a matrix.
#'
#' @param mat The matrix the user wishes to alter cells of.
#' @param value The single value that will replace specific cells of the matrix.
#' @param columns The columns the user wishes to alter. Can be numeric column
#' positions, or the user can input:
#' \itemize{
#' \item 'odd' - Only alter odd numbered columns.
#' \item 'even' - Only alter even numbered columns.
#' \item 'first' - Only alter the first column.
#' \item 'last' - Only alter the last column.
#' }
#' Also, the user can provide the column name of the column they wish to alter.
#' @return A matrix with the desired columns altered.
#' @examples 
#' df = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' mat = aes_matrix(df, 'white')
#' alter_columns(mat = mat, value = 'red', columns = 1)
#' @export

alter_columns = function(mat, value, columns = NULL) {
  
  value = check_value(value = value)
  columns = check_columns(mat = mat, columns = columns)
  
  mat[1:nrow(mat), columns] = value
  mat
  
}

#' Alter the values at specific row-column combinations of a matrix.
#'
#' @param mat The matrix the user wishes to alter cells of.
#' @param value The single value that will replace specific cells of the matrix.
#' @param rows The rows the user wishes to alter. See \code{\link{alter_rows}}
#' for information on special inputs.
#' @param columns The columns the user wishes to alter. See \code{\link{alter_columns}}
#' for information on special inputs.
#' @return A matrix with the desired cells altered.
#' @examples 
#' df = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' mat = aes_matrix(df, 'white')
#' alter_cells(mat = mat, value = 'red', rows = c(1,2), columns = 1)
#' @export

alter_cells = function(mat, value, rows = NULL, columns = NULL) {
  
  value = check_value(value = value)
  rows = check_rows(mat = mat, rows = rows)
  columns = check_columns(mat = mat, columns = columns)
  
  mat[rows, columns] = value
  mat
}

get_even_indicator = function() {
  'even'
}

get_odd_indicator = function() {
  'odd'
}

get_last_indicator = function() {
  'last'
}

get_first_indicator = function() {
  'first'
}

check_value = function(value) {
  
  if (length(value) != 1) {
    
    stop('Please provide a value of length 1.', call. = FALSE)
    
  }
  
  return(value)

}

check_rows = function(mat, rows = NULL) {
  
  even_indicator = get_even_indicator()
  odd_indicator = get_odd_indicator()
  last_indicator = get_last_indicator()
  first_indicator = get_first_indicator()
  
  allowable_strings = c(
    even_indicator,
    odd_indicator,
    last_indicator,
    first_indicator,
    rownames(mat)
    )
  
  nr = nrow(mat)
  
  if (!rows %in% allowable_strings && (any(rows > nr) || any(rows < 1))) {
    
    stop(
      call. = FALSE,
      paste0(
        'Make sure the row numbers to be altered are not outside the ',
        'dimensions of the initial data.frame/matrix.'
        )
      )
    
  }
  
  if (is.null(rows)) {
    
    return(1:nr)
    
  } else if (length(rows) == 0) {
    
    return(integer(0))
    
  } else {
    
    row_positions = c()
    
    for (row in rows) {
    
      if (row %in% even_indicator) {
        
        row_positions = c(row_positions, seq(2, nr, 2))
        
      } else if (row %in% odd_indicator) {
        
        row_positions = c(row_positions, seq(1, nr, 2))
        
      } else if (row %in% last_indicator) {
        
        row_positions = c(row_positions, nr)
        
      } else if (row %in% first_indicator) {
        
        row_positions = c(row_positions, 1)
      
      } else if (row %in% rownames(mat)) {
        
        row_positions = c(row_positions, which(rownames(mat) %in% row))
        
      } else {
        
        row_positions = c(row_positions, row)
        
      }
      
    }
    
    return(row_positions)
    
  }
  
}

check_columns = function(mat, columns = NULL) {
  
  even_indicator = get_even_indicator()
  odd_indicator = get_odd_indicator()
  last_indicator = get_last_indicator()
  first_indicator = get_first_indicator()
  
  allowable_strings = c(
    even_indicator,
    odd_indicator,
    last_indicator,
    first_indicator,
    colnames(mat)
    )
  
  nc = ncol(mat)
  
  if (!columns %in% allowable_strings && (any(columns > nc) || any(columns < 1))) {
    
    stop(
      call. = FALSE,
      paste0(
        'Make sure the column numbers to be altered are not outside the ',
        'dimensions of the initial data.frame/matrix.'
        )
      )
    
  }

  if (is.null(columns)) {
    
    return(1:nc)
    
  } else if (length(columns) == 0) {
    
    return(integer(0))
    
  } else {
    
    column_positions = c()
    
    for (column in columns) {
    
      if (column %in% even_indicator) {
        
        column_positions = c(column_positions, seq(2, nc, 2))
        
      } else if (column %in% odd_indicator) {
        
        column_positions = c(column_positions, seq(1, nc, 2))
        
      } else if (column %in% last_indicator) {
        
        column_positions = c(column_positions, nc)
        
      } else if (column %in% first_indicator) {
        
        column_positions = c(column_positions, 1)
      
      } else if (column %in% colnames(mat)) {
        
        column_positions = c(column_positions, which(colnames(mat) %in% column))
        
      } else {
        
        column_positions = c(column_positions, column)
        
      }
      
    }
    
    return(column_positions)
    
  }
  
  
}

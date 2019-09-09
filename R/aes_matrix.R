#' Create a matrix based off the dimensions of a data.frame/matrix and a single 
#' value to make up its cells.
#'
#' @param df A data.frame the resulting matrix will get its dimensions from.
#' @param value The single value that will make up the cells of the resulting matrix.
#' @return A matrix based on the dimensions of \code{df} and \code{value}.
#' @examples 
#' df = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' aes_matrix(df, 'white')
#' @export

aes_matrix = function(df, value) {
  
  value = check_value(value = value)

  matrix(value, nrow = nrow(df), ncol = ncol(df))
  
}

#' Alter the values at specific rows of a matrix.
#'
#' @param mat The matrix the user wishes to alter cells of.
#' @param value The single value that will replace specific cells of the matrix.
#' @param row_numbers The row numbers the user wishes to alter.
#' @return A matrix with the desired rows altered.
#' @examples 
#' df = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' mat = aes_matrix(df, 'white')
#' alter_rows(mat = mat, value = 'red', row_numbers = c(1, 2))
#' @export

alter_rows = function(mat, value, row_numbers = NULL) {
  
  value = check_value(value = value)
  row_numbers = check_row_numbers(mat = mat, row_numbers = row_numbers)
  
  mat[row_numbers, 1:ncol(mat)] = value
  mat

}

#' Alter the values at specific columns of a matrix.
#'
#' @param mat The matrix the user wishes to alter cells of.
#' @param value The single value that will replace specific cells of the matrix.
#' @param column_numbers The row numbers the user wishes to alter.
#' @return A matrix with the desired columns altered.
#' @examples 
#' df = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' mat = aes_matrix(df, 'white')
#' alter_columns(mat = mat, value = 'red', column_numbers = 1)
#' @export

alter_columns = function(mat, value, column_numbers = NULL) {
  
  value = check_value(value = value)
  column_numbers = check_column_numbers(mat = mat, column_numbers = column_numbers)
  
  mat[1:nrow(mat), column_numbers] = value
  mat
  
}

#' Alter the values at specific row-column combinations of a matrix.
#'
#' @param mat The matrix the user wishes to alter cells of.
#' @param value The single value that will replace specific cells of the matrix.
#' @param row_numbers The row numbers the user wishes to alter.
#' @param column_numbers The row numbers the user wishes to alter.
#' @return A matrix with the desired cells altered.
#' @examples 
#' df = data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' mat = aes_matrix(df, 'white')
#' alter_cells(mat = mat, value = 'red', row_numbers = c(1,2), column_numbers = 1)
#' @export

alter_cells = function(mat, value, row_numbers = NULL, column_numbers = NULL) {
  
  value = check_value(value = value)
  row_numbers = check_row_numbers(mat = mat, row_numbers = row_numbers)
  column_numbers = check_column_numbers(mat = mat, column_numbers = column_numbers)
  
  mat[row_numbers, column_numbers] = value
  mat
}


check_value = function(value) {
  
  if (length(value) != 1) {
    
    stop('Please provide a value of length 1.')
    
  }
  
  return(value)

}

check_row_numbers = function(mat, row_numbers = NULL) {
  
  if (is.null(row_numbers)) {
    
    return(1:nrow(mat))
    
  } else if (row_numbers[1] == 'even') {
    
    return(seq(2, nrow(mat), 2))
    
  } else if (row_numbers[1] == 'odd') {
    
    return(seq(1, nrow(mat), 2))
    
  } else if (row_numbers[1] == 'last') {
    
    return(nrow(mat))
    
  } else if (row_numbers[1] == 'first') {
    
    return(1)
    
  } else {
    
    return(row_numbers)
    
  }
  
}

check_column_numbers = function(mat, column_numbers = NULL) {
  
  if (is.null(column_numbers)) {
    
    return(1:ncol(mat))
    
  } else if (column_numbers[1] == 'even') {
    
    return(seq(2, ncol(mat), 2))
    
  } else if (column_numbers[1] == 'odd') {
    
    return(seq(1, ncol(mat), 2))
    
  } else if (column_numbers[1] == 'last') {
    
    return(ncol(mat))
    
  } else if (column_numbers[1] == 'first') {
    
    return(1)
    
  } else {
    
    return(column_numbers)
    
  }
  
}

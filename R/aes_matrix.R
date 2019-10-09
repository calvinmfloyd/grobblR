#' Create a matrix based off the dimensions of a data.frame/matrix and a single 
#' value to make up its cells. Designed to be used as an aesthetic matrix within
#' \code{\link{ga_list}}.
#'
#' @param df A data.frame the resulting matrix will get its dimensions from.
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
  if (column_names) df = matrix(colnames(df), nrow = 1, ncol = ncol(df))
  
  matrix(value, nrow = nrow(df), ncol = ncol(df))
  
}

#' Alter the values at specific rows of a matrix.
#'
#' @param mat The matrix the user wishes to alter cells of.
#' @param value The single value that will replace specific cells of the matrix.
#' @param row_numbers The row numbers the user wishes to alter. The user can also 
#' input:
#' \itemize{
#' \item 'odd' - Only alter odd numbered rows.
#' \item 'even' - Only alter even numbered rows.
#' \item 'first' - Only alter the first row.
#' \item 'last' - Only alter the last row.
#' }
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
#' @param column_numbers The column numbers the user wishes to alter. The user can also 
#' input:
#' \itemize{
#' \item 'odd' - Only alter odd numbered columns.
#' \item 'even' - Only alter even numbered columns.
#' \item 'first' - Only alter the first column.
#' \item 'last' - Only alter the last column.
#' }
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
#' @param row_numbers The row numbers the user wishes to alter. See \code{\link{alter_rows}}
#' for information on special inputs.
#' @param column_numbers The column numbers the user wishes to alter. See \code{\link{alter_columns}}
#' for information on special inputs.
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

check_row_numbers = function(mat, row_numbers = NULL) {
  
  even_indicator = get_even_indicator()
  odd_indicator = get_odd_indicator()
  last_indicator = get_last_indicator()
  first_indicator = get_first_indicator()
  
  allowable_strings = c(
    even_indicator,
    odd_indicator,
    last_indicator,
    first_indicator
    )
  
  nr = nrow(mat)
  
  if (!row_numbers %in% allowable_strings && (any(row_numbers > nr) || any(row_numbers < 1))) {
    
    stop(
      call. = FALSE,
      paste0(
        'Make sure the row numbers to be altered are not outside the ',
        'dimensions of the initial data.frame/matrix.'
        )
      )
    
  }
  
  if (is.null(row_numbers)) {
    
    return(1:nr)
    
  } else if (length(row_numbers) == 0) {
    
    return(integer(0))
    
  } else if (row_numbers[1] == even_indicator) {
    
    return(seq(2, nr, 2))
    
  } else if (row_numbers[1] == odd_indicator) {
    
    return(seq(1, nr, 2))
    
  } else if (row_numbers[1] == last_indicator) {
    
    return(nr)
    
  } else if (row_numbers[1] == first_indicator) {
    
    return(1)
    
  } else {
    
    return(row_numbers)
    
  }
  
}

check_column_numbers = function(mat, column_numbers = NULL) {
  
  even_indicator = get_even_indicator()
  odd_indicator = get_odd_indicator()
  last_indicator = get_last_indicator()
  first_indicator = get_first_indicator()
  
  allowable_strings = c(
    even_indicator,
    odd_indicator,
    last_indicator,
    first_indicator
    )
  
  nc = ncol(mat)
  
  if (!column_numbers %in% allowable_strings && (any(column_numbers > nc) || any(column_numbers < 1))) {
    
    stop(
      call. = FALSE,
      paste0(
        'Make sure the column numbers to be altered are not outside the ',
        'dimensions of the initial data.frame/matrix.'
        )
      )
    
  }

  if (is.null(column_numbers)) {
    
    return(1:nc)
  
  } else if (length(column_numbers) == 0) {
    
    return(integer(0))
    
  } else if (column_numbers[1] == even_indicator) {
    
    return(seq(2, nc, 2))
    
  } else if (column_numbers[1] == odd_indicator) {
    
    return(seq(1, nc, 2))
    
  } else if (column_numbers[1] == last_indicator) {
    
    return(nc)
    
  } else if (column_numbers[1] == first_indicator) {
    
    return(1)
    
  } else {
    
    return(column_numbers)
    
  }
  
}

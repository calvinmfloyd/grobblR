
get_matrix_aes_elements = function() {
  
  aes_list = ga_list()
  stringr::str_replace(
    string = names(aes_list)[grepl('cell_', names(aes_list))],
    pattern = 'cell_',
    replacement = ''
    )
  
}

find_optimal_text_cex = function(mat,
                                 column_widths,
                                 height,
                                 units) {
  
  nc = ncol(mat)
  nr = nrow(mat)
  
  optimal_cvs = sapply(
    X = 1:nc,
    FUN = function(x) {
      
      column_edited = ifelse(mat[,x] %in% '', " ", mat[,x])
      
        cex_val_convergence(
          string = paste(column_edited, collapse = '\n'),
          n_lines = nr,
          height = height,
          units = units,
          width = column_widths[x],
          sep = '\n'
        )$cex_val
      
      }
    )

  optimal_text_cex = ifelse(is.null(optimal_cvs), 1, min(optimal_cvs))  
  
  return(optimal_text_cex)
  
}


#' Create a matrix based off the dimensions of a data.frame/matrix and a single 
#' value to make up its cells. Designed to be used as an aesthetic matrix within
#' \code{\link{ga_list}}.
#'
#' @param df A data.frame/matrix the resulting matrix will get its dimensions from.
#' @param value The single value that will make up the cells of the resulting matrix.
#' @param column_names A TRUE/FALSE value indicating if the resulting aesthetic
#' matrix is intended to be used for the column names.
#' @return A matrix based on the dimensions of \code{df} and \code{value}.

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


check_value = function(value) {
  
  if (length(value) != 1) {
    
    stop('Please provide a value of length 1.', call. = FALSE)
    
  }
  
  return(value)

}


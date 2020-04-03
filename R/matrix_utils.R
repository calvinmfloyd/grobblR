
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
          convergence_limit = 0.025,
          sep = '\n'
        )$cex_val
      
      }
    )

  optimal_text_cex = ifelse(is.null(optimal_cvs), 1, min(optimal_cvs))  
  
  return(optimal_text_cex)
  
}



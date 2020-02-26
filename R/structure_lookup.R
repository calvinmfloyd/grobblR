`%>%` = dplyr::`%>%`
options(stringsAsFactors = FALSE)

get_structure_lookup_df = function(current,
                                   height = NULL,
                                   width = NULL) {
  
  nc = ncol(current)
  nr = nrow(current)
  
  if (all(!is.null(height), !is.null(width))) {
    
    column_widths_p = sapply(
      X = 1:nc,
      FUN = function(x) {
        
        column = current[, x]
        width = max(graphics::strwidth(c(column), units = 'in'))
        return(ifelse(width == 0, 1, width))
        
      })

    default_column_widths_p = column_widths_p/sum(column_widths_p)

  } else {
    
    default_column_widths_p = rep(1, nc)

  }
  
  structure_lookup_df = list(
    
    # C ----
    # > Column Widths Proportions ----
    dplyr::tibble(
      structure = 'column_widths_p',
      theme = 'default',
      value = list(
        matrix(
          data = default_column_widths_p,
          nrow = 1,
          ncol = nc
          )
        )
      ),
    # P ----
    # > Padding Proportions ----
    dplyr::tibble(
      structure = 'padding_p',
      theme = 'default',
      value = list(
        matrix(
          data = 0.05,
          nrow = 1,
          ncol = nc
          )
        )
      )
    
    ) %>%
    dplyr::bind_rows()
  
 return(structure_lookup_df) 
  
}

get_all_matrix_structures = function() {
  
  data.frame() %>%
    get_structure_lookup_df() %>%
    .[['structure']] %>%
    unique()

}

get_matrix_structure = function(grob_matrix_object, structure) {
  
  grob_matrix_object$current %>%
    get_structure_lookup_df() %>%
    dplyr::filter(structure == !!structure)

}



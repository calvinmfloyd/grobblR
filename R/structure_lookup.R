
options(stringsAsFactors = FALSE)

# Matrix ----

get_matrix_structure_lookup_df = function(current,
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
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = default_column_widths_p,
          nrow = 1,
          ncol = nc
          )
        )
      ),
    # N ----
    # > Number of Lines ----
    dplyr::tibble(
      structure = 'n_lines',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(matrix(10000, nrow = 1, ncol = nc))
      ),
    # P ----
    # > Padding Proportions ----
    dplyr::tibble(
      structure = 'padding_p',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 0.05,
          nrow = 1,
          ncol = nc
          )
        )
      ),
    # T ----
    # > Text CEX Addition ----
    dplyr::tibble(
      structure = 'text_cex_addition',
      theme = 'default',
      accepted_classes = list(c('numeric')),
      value = list(
        matrix(
          data = 0,
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
    get_matrix_structure_lookup_df() %>%
    .[['structure']] %>%
    unique()

}

get_matrix_structure = function(grob_matrix_object, structure) {
  
  grob_matrix_object$current %>%
    get_matrix_structure_lookup_df() %>%
    dplyr::filter(structure == !!structure)

}

# Image ----

get_image_structure_lookup_df = function() {
  
  structure_lookup_df = list(
    
    # A ----
    # > Aspect Ratio Multiplier ----
    dplyr::tibble(
      structure = 'aspect_ratio_multiplier',
      accepted_classes = list(c('numeric')),
      theme = 'default',
      value = list(1)
      ),
    # M ----
    # > Maintain Aspect Ratio ----
    dplyr::tibble(
      structure = 'maintain_aspect_ratio',
      accepted_classes = list(c('logical')),
      theme = 'default',
      value = list(TRUE)
      )
    
    ) %>%
    dplyr::bind_rows()
  
 return(structure_lookup_df) 
  
}

get_all_image_structures = function() {
  
  get_image_structure_lookup_df() %>%
    .[['structure']] %>%
    unique()
  
}

get_image_structure = function(structure) {
  
  get_image_structure_lookup_df() %>%
    dplyr::filter(structure == !!structure)

}

# Overall ----

get_structure_lookup_df = function(type,
                                   current = NULL,
                                   height = NULL,
                                   width = NULL) {
  
  if (type %in% c('matrix', 'text')) {
    
    lookup_df = get_matrix_structure_lookup_df(
      current = current,
      height = height,
      width = width
      )
    
  } else if (type %in% 'image') {
    
    
    lookup_df = get_image_structure_lookup_df()
    
  }
  
  return(lookup_df)
  
}




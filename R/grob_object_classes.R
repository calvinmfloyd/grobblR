
`%>%` = dplyr::`%>%`
 
grob_matrix_object_class = R6::R6Class(
  classname = "grob_matrix_object",
  public = list(
    x = data.frame(stringsAsFactors = FALSE),
    current = matrix(),
    initial = data.frame(stringsAsFactors = FALSE),
    test = data.frame(stringsAsFactors = FALSE),
    current_group = NA_character_,
    current_aesthetic = NA_character_,
    structure_list = list(),
    aesthetic_list = list(),
    column_names_to_row = 0,
    column_headings_added = 0,
    height = NULL,
    width = NULL,
    units = NULL,
    theme = 'default',
    initialize = function(x,
                          initial){
      
      self$x = x
      self$initial = initial

    }),
  active = list(
    
    finish_ga_list = function(height = self$height,
                              width = self$width,
                              units = self$units,
                              test = self$test,
                              current = self$current,
                              aesthetic_list = self$aesthetic_list,
                              structure_list = self$structure_list) {
      
      # > Structures ----
      
      structure_lookup_df = get_structure_lookup_df(
        current = current,
        height = height,
        width = width
        )
      
      # - Go through each of the matrix structures, fill in any missing values
      # with default values.
      for (structure in unique(structure_lookup_df[['structure']])) {
        
        default_mat = structure_lookup_df %>%
          dplyr::filter(structure %in% !!structure) %>%
          dplyr::pull(value) %>%
          .[[1]]

        input_mat = structure_list[[structure]]
        
        if (is.null(input_mat)) {
          
          input_mat = matrix(NA, nrow = nrow(default_mat), ncol = ncol(default_mat))
          
        } else {
          
          # - Check to make sure that the dimensions of the inputted matrix
          # match up with what is expected from the default value matrix
          if (!all(dim(structure_list[[structure]]) == dim(default_mat))) {
            
            nr_default = nrow(default_mat)
            nc_default = ncol(default_mat)
            nr_input = nrow(input_mat)
            nc_input = ncol(input_mat)
            
            error_msg = glue::glue("
              The dimensions of {structure} must be 1x1 or {nr_default}x{nc_default}, \\
              not {nr_input}x{nc_input}.
              ")
            
            stop(error_msg, call. = FALSE)
            
          }
          
        }
        
        boolean_matrix = is.na(input_mat)
        input_mat[boolean_matrix] = default_mat[boolean_matrix]
        structure_list[[structure]] = input_mat
        
      }
      
      # > Aesthetics ----
      
      aesthetic_lookup_df = get_aesthetic_lookup_df(
        test = test,
        current = current,
        width = width,
        height = height,
        units = units,
        structure_list = structure_list
        )
      
      test_body_groups = unique(test[['grobblR_group']])

      # - Go through each of the matrix aesthetics, fill in any missing values
      # with default values.
      for (aes in unique(aesthetic_lookup_df[['aesthetic']])) {
        
        default_list = aesthetic_lookup_df %>%
          dplyr::filter(
            aesthetic %in% aes,
            group %in% test_body_groups
            ) %>%
          dplyr::arrange(
            match(
              x = group,
              table = c('column_headings', 'column_names', 'cells')
              )
            ) %>%
          dplyr::pull(value)
        
        default_mat = do.call(rbind, default_list)
      
        input_mat = aesthetic_list[[aes]]
        
        if (is.null(input_mat)) {
          
          input_mat = aes_matrix(df = current, value = NA)
          
        } else {
          
          # - Check to make sure that the dimensions of the inputted matrix
          # match up with what is expected from the default value matrix
          if (!all(dim(aesthetic_list[[aes]]) == dim(default_mat))) {
            
            nr_default = nrow(default_mat)
            nc_default = ncol(default_mat)
            nr_input = nrow(input_mat)
            nc_input = ncol(input_mat)
            
            error_msg = glue::glue("
              The dimensions of {aes} must be {nr_default}x{nc_default}, \\
              not {nr_input}x{nc_input}.
              ")
            
            stop(error_msg, call. = FALSE)
            
          }
          
        }
        
        boolean_matrix = is.na(input_mat)
        input_mat[boolean_matrix] = default_mat[boolean_matrix]
        aesthetic_list[[aes]] = input_mat
        
      }
      
      return(c(aesthetic_list, structure_list))
      
    })
    
  )

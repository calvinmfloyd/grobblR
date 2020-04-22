
# Grob Matrix ----
grob_matrix_object = R6::R6Class(
  classname = "grob_matrix_object",
  public = list(
    initial = tibble::tibble(),
    current = matrix(),
    test = tibble::tibble(),
    type = character(),
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
    initialize = function(initial,
                          type){
      
      self$initial = initial
      self$type = type

    }),
  active = list(
    
    finish_ga_list = function(height = self$height,
                              width = self$width,
                              units = self$units,
                              type = self$type,
                              test = self$test,
                              current = self$current,
                              aesthetic_list = self$aesthetic_list,
                              structure_list = self$structure_list) {
      
      # > Structures ----
      
      structure_lookup_df = get_structure_lookup_df(
        type = type,
        current = current,
        height = height,
        width = width
        )
   
      # - Go through each of the matrix structures, fill in any missing values
      # with default values.
      for (structure in unique(structure_lookup_df[['structure']])) {
        
        default = structure_lookup_df %>% dplyr::filter(structure %in% !!structure)
        default_mat = default[['value']][[1]]
        accepted_classes = default[['accepted_classes']][[1]]

        input_mat = structure_list[[structure]]
        
        if (is.null(input_mat)) {
          
          input_mat = matrix(NA, nrow = nrow(default_mat), ncol = ncol(default_mat))
          
        } else {
          
          # - Check to make sure the class of the inputted matrix is of an 
          # accepted class
          if (!any(is(input_mat[1,1]) %in% accepted_classes)) {
    
            error_msg = glue::glue("
              The class of the {structure} structure input must be one of: \\
              {paste(accepted_classes, collapse = ', ')}
              ")
    
            stop(error_msg, call. = FALSE)
            
          }
          
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
      
      aesthetic_lookup_list = get_matrix_aesthetic_lookup_df(
        test = test,
        current = current,
        type = type,
        width = width,
        height = height,
        units = units,
        structure_list = structure_list
        )
 
      current = aesthetic_lookup_list[['current']]
      self$current = current
      aesthetic_lookup_df = aesthetic_lookup_list[['lookup_df']]
      
      test_body_groups = unique(test[['grobblR_group']])

      # - Go through each of the matrix aesthetics, fill in any missing values
      # with default values.
      for (aesthetic in unique(aesthetic_lookup_df[['aesthetic']])) {
        
        default_list = aesthetic_lookup_df %>%
          dplyr::filter(
            aesthetic %in% !!aesthetic,
            group %in% test_body_groups
            ) %>%
          dplyr::arrange(
            match(
              x = group,
              table = c('column_headings', 'column_names', 'cells')
              )
            )
        
        default_mat = do.call(rbind, default_list[['value']])
        accepted_classes = default_list[['accepted_classes']][[1]]
      
        input_mat = aesthetic_list[[aesthetic]]
        
        if (is.null(input_mat)) {
          
          input_mat = aes_matrix(df = current, value = NA)
          
        } else {
          
          if (type %in% 'text') {
            
            input_mat = matrix(input_mat[1,1], ncol = 1, nrow = nrow(current))
            
          }
          
          # - Check to make sure the class of the inputted matrix is of an 
          # accepted class
          if (!any(is(input_mat[1,1]) %in% accepted_classes)) {
    
            error_msg = glue::glue("
              The class of the {aesthetic} aesthetic input must be one of: \\
              {paste(accepted_classes, collapse = ', ')}
              ")
    
            stop(error_msg, call. = FALSE)
            
          }
          
          # - Check to make sure that the dimensions of the inputted matrix
          # match up with what is expected from the default value matrix
          if (!all(dim(input_mat) == dim(default_mat))) {
            
            nr_default = nrow(default_mat)
            nc_default = ncol(default_mat)
            nr_input = nrow(input_mat)
            nc_input = ncol(input_mat)
            
            error_msg = glue::glue("
              The dimensions of {aesthetic} must be {nr_default}x{nc_default}, \\
              not {nr_input}x{nc_input}.
              ")
            
            stop(error_msg, call. = FALSE)
            
          }
          
        }
        
        boolean_matrix = is.na(input_mat)
        input_mat[boolean_matrix] = default_mat[boolean_matrix]
        
        # - For grob matrix objects, we will use a placeholder (originally set to 'none') 
        # for when we want an empty color. After we add in default values, we will convert
        # any elements of the matrix (if it is a character matrix) that have this
        # placeholder with the functional NA_character_.
        if (is(input_mat[1,1], 'character')) {
          
          input_mat[input_mat %in% get_empty_placeholder()] = NA_character_
          
        }
        aesthetic_list[[aesthetic]] = input_mat
        
      }
      
      return(c(aesthetic_list, structure_list))
      
    })
    
  )

# Grob Image ----
grob_image_object = R6::R6Class(
  classname = "grob_image_object",
  public = list(
    initial = character(),
    structure_list = list(),
    initialize = function(initial){
      
      self$initial = initial

    }),
  active = list(
    
    finish_ga_list = function(structure_list = self$structure_list) {
      
      # > Structures ----
      
      structure_lookup_df = get_structure_lookup_df(type = 'image')
      
      # - Go through each of the matrix structures, fill in any missing values
      # with default values.
      for (structure in unique(structure_lookup_df[['structure']])) {
        
        default = structure_lookup_df %>% dplyr::filter(structure %in% !!structure)
        default_value = default[['value']][[1]]
        accepted_classes = default[['accepted_classes']][[1]]

        input = structure_list[[structure]]
        
        if (is.null(input)) {
          
          input = default_value
          
        } else {
          
          if (!any(is(input) %in% accepted_classes)) {
    
            error_msg = glue::glue("
              The class of the {structure} structure input must be one of: \\
              {paste(accepted_classes, collapse = ', ')}
              ")
    
            stop(error_msg, call. = FALSE)
            
          }
          
        }
        
        structure_list[[structure]] = input
        
      }
      
      return(c(structure_list))
      
    })
    
  )



add_structure = function(grob_object,
                         structure,
                         value) {
  
  if (length(value) == 1) {
    
    value = rep(value, length = ncol(grob_object$current))
    
  }
  
  value = convert_to_matrix(value)
  structure = check_structure(
    grob_object = grob_object,
    structure = structure,
    value = value,
    location = 'add_structure()'
    )
  
  grob_object$structure_list[[structure]] = value

  return(grob_object)
  
}


check_structure = function(grob_object, structure, value, location) {
  
  default_structure = get_matrix_structure(
    grob_matrix_object = grob_object,
    structure = structure
    )
    
  if (nrow(default_structure) == 0) {
    
    valid_structures = get_all_matrix_structures()
    
    error_msg = glue::glue("
      structure parameter within {location} must be one of: \\
      {paste(valid_structures, collapse = ', ')}
      ")
    
    stop(error_msg, call. = FALSE)
  
  }
  
  if (!all(dim(default_structure$value[[1]]) == dim(value)) & ! all(dim(value) == 1)) {
    
    error_msg = glue::glue("
      The structure '{structure}' must have a inputted value within {location} \\
      of 1x1 or {nrow(default_structure)}x{ncol(default_structure)}, not \\
      {nrow(value)}x{ncol(value)}.
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  return(structure)
  
}

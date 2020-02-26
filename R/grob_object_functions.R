
grob_matrix = function(x) {
  
  if (!any(is.data.frame(x), is.matrix(x), is.vector(x))) {
    
    error_msg = glue::glue("
      The object passed through grob_matrix() must either be a \\
      data.frame, matrix or a vector.
      ")
    
    stop(error_msg, call. = FALSE)
    
  }
  
  initial_type = dplyr::case_when(
    is.data.frame(x) ~ 'data.frame',
    is.matrix(x) ~ 'matrix',
    is.vector(x) ~ 'vector'
    )
    
  if (initial_type == 'vector') {
      
    initial = convert_to_matrix(x)
    
  } else {
      
    initial = x
    
  }
  
  aes_object = grob_matrix_object_class$new(
    x = x,
    initial = initial
    )
  
  test = initial
  
  if (nrow(test) > 0) {
    
    test[['grobblR_group']] = 'cells'
  
  }
    
  aes_object$test = test
  aes_object$current = convert_to_matrix(initial)
  
  aes_object = column_names_to_row(aes_object)
  
  return(aes_object)

}

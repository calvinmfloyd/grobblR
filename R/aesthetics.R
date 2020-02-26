
`%>%` = dplyr::`%>%`

#' Add an aesthetic to a grob object class.
#' @param grob_object The R6 grob object class outputted by \code{\link{grob_matrix}}.
#' @param aesthetic The aesthetic the user wishes to add.
#' @param group The group of the grob matrix object the user wants to add the 
#' aesthetic to. For matrices, the user can add an aesthetic to the cells, the
#' column names or the column headings.
#' @param value A single value the user wants to apply to the group of matrix 
#' elements for the given aesthetic.
#' @return The R6 grob object class with its aesthetics properties altered.
#' 
#' @export
#' 
#' @examples
#' 
#' library(dplyr)
#' df = data.frame(var1 = c(5, 14, 6, 10), var2 = c(3, 30, 17, 7))
#' df %>%
#'   grob_matrix() %>% 
#'   add_aesthetic(aesthetic = 'text_color', group = 'cells', value = 'red') %>%
#'   grob_col(width = 100) %>%
#'   .$grob %>%
#'   plot()
#' 

add_aesthetic = function(grob_object,
                         aesthetic,
                         group = c('cells', 'column_names', 'column_headings'),
                         value = NULL) {
  
  group = match.arg(group)
  value = check_value(value = value)
  aesthetic = check_aesthetic(aesthetic = aesthetic, location = 'add_aesthetic()')
  
  if (!is.null(grob_object$aesthetic_list[[aesthetic]])) {
    
    mat = grob_object$aesthetic_list[[aesthetic]]
    
  # - If the aesthetic is NULL within the aesthetic list, then we will initialize
  # the aesthetic matrix with NA's.
  } else {
  
    mat = aes_matrix(df = grob_object$current, value = NA)
    
  }

  mat[grob_object$test[['grobblR_group']] %in% group,] = value
  
  grob_object$current_aesthetic = aesthetic
  grob_object$current_group = group
  grob_object$aesthetic_list[[aesthetic]] = mat

  return(grob_object)
  
}

#' Flexibly alter the aesthetic of a grob matrix object at specific points of
#' the data.frame/matrix.
#' @param grob_object The R6 grob object class outputted by \code{\link{grob_matrix}}.
#' @param .f A quosure style lambda \code{~ fun(.)}, which the user wants to apply
#' to the specific subset of cells.
#' @param ... Logical predicates defined in terms of the variables in the initial 
#' data.frame/matrix, or if the user provides a new data.frame to evaluate via 
#' \code{data}. Multiple conditions are combined with &. Only rows where the condition 
#' evaluates to TRUE are evaluated. 
#' 
#' If no logical predicates provided, then the entire columns will be altered.
#' 
#' @param columns A character vector of column names of the initial data.frame/matrix
#' the user wishes to alter.
#' @param data A separate data.frame/matrix of the same dimensions as the initial 
#' data.frame/matrix which the \code{.f} function will be applied to. Any potential
#' filters will always be applied to the initial data.frame/matrix, regardless if 
#' a separate data.frame/matrix is supplied.
#' @param aesthetic Which aesthetic the user wants to make alterations to. If left 
#' \code{NULL}, the function will look for the most previous altered aesthetic, either
#' via \code{\link{add_aesthetic}}, or a previous application of \code{alter_at}.
#' @param group Which group of elements the user wants to make alterations to. If left 
#' \code{NULL}, the function will look for the most previous altered group, either
#' via \code{\link{add_aesthetic}}, or a previous application of \code{alter_at}.
#' @return The R6 grob object class with its aesthetics properties altered.
#' 
#' @export
#' 
#' @examples 
#' 
#' library(dplyr)
#' df = data.frame(var1 = c(5, 14, 6, 10), var2 = c(3, 30, 17, 7))
#' df %>%
#'   grob_matrix() %>%
#'   add_aesthetic(aesthetic = 'text_color', group = 'cells', value = 'red') %>%
#'   alter_at(
#'     .f = ~ 'blue',
#'     abs(var2 - var1) > 1
#'     ) %>%
#'   grob_col(width = 100) %>%
#'   .$grob %>%
#'   plot()
#' 
#' test_function = function(x) ifelse(x > 15, 2, 1)
#' 
#' df %>%
#'   grob_matrix() %>%
#'   alter_at(
#'     .f = ~ test_function(.),
#'     aesthetic = 'font_face',
#'     group = 'cells'
#'     ) %>%
#'   grob_col(width = 100) %>%
#'   .$grob %>%
#'   plot()
#' 

alter_at = function(grob_object, 
                    .f = NULL,
                    ...,
                    columns = NULL,
                    data = NULL,
                    aesthetic = NULL,
                    group = NULL) {

  if (!is(grob_object, 'grob_matrix_object')) {
    
    error_msg = "A grob_matrix() object must be passed through alter_at()." 
    stop(error_msg, call. = FALSE)
    
  }

  # - If the user does not provide a boolean condition on what rows to alter,
  # then we will assume they want to alter all rows.
  if (missing(...)) {
    
    filter_expression = TRUE
    
  } else {
  
    filter_expression = dplyr::expr(...)
  
  }
  
  # - If no columns are provided, then we will alter all columns.
  if (is.null(columns)) {
    
    columns = colnames(grob_object$initial)
    
  }
  
  # - Figuring out which indices are each of te selected columns, which will
  # help us if the user wants to apply the function to a different data.frame
  which_columns = which(columns %in% colnames(grob_object$initial))
  
  # - If no aesthetic is provided in the function parameters, then it is assumed
  # the user wants to alter the last edited aesthetic either from an add_aesthetic() or
  # a previous alter_at().
  if (!is.null(aesthetic)) {
    
    aesthetic = check_aesthetic(aesthetic = aesthetic, location = 'alter_at()')
    
    if (is.null(grob_object$aesthetic_list[[aesthetic]])) {

      grob_object$aesthetic_list[[aesthetic]] = aes_matrix(
        df = grob_object$current,
        value = NA
        )

    }
    
    grob_object$current_aesthetic = aesthetic
    
  } else {
    
    # - If the user skipped the add_aesthetic() step, and didn't provide an aesthetic
    # within this function, then an error message will pop up.
    if (is.na(grob_object$current_aesthetic)) {
      
      error_msg = glue::glue("
        Please provide an aesthetic to adjust, either within add_aesthetic() \\
        or alter_at().
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
  }
    
  if (!is.null(group)) {
    
    grob_object$current_group = check_group(
      group = group,
      test = grob_object$test,
      location = 'alter_at()'
      )
    
  } else {
    
    # - If the user skipped the add_aesthetic() step, and didn't provide a group
    # within this function, then an error message will pop up.
    if (is.na(grob_object$current_group)) {
      
      error_msg = glue::glue("
        Please provide a group ('cells', 'column_names', 'column_headings') to \\
        adjust, either within add_aesthetic() or alter_at().
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
  }
  
  # - If the user provides data onto which to apply the function to, then we 
  # will make sure the dimensions of the data.frame are correct first.
  if (!is.null(data)) {
    
    initial_dims = dim(grob_object$initial)
    data_dims = dim(data)
    
    if (!all(initial_dims == data_dims)) {
      
      error_msg = glue::glue("
        The dimensions of data parameter in alter_at() must be the same dimensions \\
        as the initial matrix/data.frame/vector. 
        The dimensions of data are {data_dims[1]}x{data_dims[2]}, and the dimensions \\
        of the initial object are {initial_dims[1]}x{initial_dims[2]}.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    # - Mirroring the process the initial object went through (any column headings,
    # converting column names to row).
    if (grob_object$column_names_to_row == 1) {
      
      data = rbind(NA, data)
      
    }
    
    for (i in 1:grob_object$column_headings_added) {
      
      data = rbind(NA, data)
      
    }
    
  } else {
    
    data = grob_object$test
    
  }
  
  boolean_vector = grob_object$test %>%
    dplyr::mutate(
      applied_filter = dplyr::coalesce(eval(filter_expression), FALSE),
      which_to_alter = applied_filter & (grobblR_group %in% grob_object$current_group)
      ) %>%
    dplyr::pull(which_to_alter)
  
  boolean_matrix = aes_matrix(df = grob_object$test, value = FALSE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::select(
      -dplyr::matches('grobblR_group')
      ) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(columns),
      .funs = list(~ boolean_vector)
      ) %>%
    as.matrix()
 
  data_columns = colnames(data)[which_columns]
  applied_function_mat = data %>%
    dplyr::select(
      -dplyr::matches('grobblR_group')
      ) %>%
    dplyr::filter(boolean_vector) %>%
    dplyr::rowwise() %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(data_columns),
      .funs = list(.f)
      ) %>%
    dplyr::ungroup() %>%
    as.matrix()
  
  grob_object$aesthetic_list[[grob_object$current_aesthetic]][boolean_matrix] = applied_function_mat

  return(grob_object)
  
}

check_aesthetic = function(aesthetic, location) {
  
  valid_aesthetics = get_matrix_aesthetics()
    
  if (!aesthetic %in% valid_aesthetics) {
    
    error_msg = glue::glue("
      aesthetic parameter within {location} must be one of: \\
      {paste(valid_aesthetics, collapse = ', ')}
      ")
    
    stop(error_msg, call. = FALSE)
  
  }
  
  return(aesthetic)
  
}

check_group = function(group, test, location) {
  
  valid_groups = unique(test[['grobblR_group']])
    
  if (!group %in% valid_groups) {
    
    error_msg = glue::glue("
      group within {location} must be one of: \\
      {paste(valid_groups, collapse = ', ')}
      ")
    
    stop(error_msg, call. = FALSE)

  }
  
  return(group)
  
}



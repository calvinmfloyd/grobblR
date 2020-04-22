
options(stringsAsFactors = FALSE)

#' Add an Aesthetic
#' 
#' Add an aesthetic to a grob matrix object.
#' 
#' @param grob_object The R6 object outputted by either \code{\link{grob_matrix}}
#' or \code{\link{grob_text}}.
#' 
#' @param aesthetic The matrix aesthetic the user wishes to add.
#' 
#' @param group The group of the grob matrix object the user wants to add the 
#' aesthetic to. 
#' 
#' For objects initialized by \code{\link{grob_matrix}}, the user can add an aesthetic to the 'cells', the
#' 'column_names' or the 'column_headings'. If the user is passing through an object 
#' initialized by \code{\link{grob_text}}, then only 'cells' will be accepted.
#' 
#' @param value A single value the user wants to apply to the group of matrix / text 
#' elements for the given aesthetic.
#' 
#' @return The R6 object of the grob matrix class with its aesthetics properties altered.
#' 
#' @export
#' 
#' @details 
#' 
#' Accepted aesthetics:
#' 
#' \describe{
#' 
#' \item{Matrix / Text}{
#' 
#' \itemize{
#' 
#' \item \code{background_alpha}
#' \item \code{background_color}
#' \item \code{border_color}
#' \item \code{border_sides}
#' \item \code{border_width}
#' \item \code{font_face}
#' \item \code{group_elements}
#' \item \code{round_rect_radius}
#' \item \code{text_align}
#' \item \code{text_cex}
#' \item \code{text_font}
#' \item \code{text_color}
#' \item \code{text_just}
#' \item \code{text_v_align}
#' \item \code{text_v_just}
#' \item \code{text_rot}
#' 
#' }
#' }
#' 
#' }
#' 
#' To see descriptions of the aesthetics above, see the documentation of \code{\link{ga_list}}.
#' 
#' @examples
#' 
#' df = data.frame(var1 = c(5, 14, 6, 10), var2 = c(3, 30, 17, 7))
#' df %>%
#'   grob_matrix() %>% 
#'   add_aesthetic(aesthetic = 'text_color', value = 'red', group = 'cells') %>%
#'   view_grob()
#' 

add_aesthetic = function(grob_object,
                         aesthetic,
                         value = NULL,
                         group = c('cells', 'column_names', 'column_headings')) {
  
  group = match.arg(group)
  value = check_value(value = value)
  
  is_grob_matrix = is(grob_object, 'grob_matrix_object')
  
  # - Checking to make sure it's a valid grob object
  if (!is_grob_matrix) {
    
     error_msg = glue::glue("
       Please provide an object initialized by grob_matrix() or grob_text() in add_aesthetic().
       ")
    
    stop(error_msg, call. = FALSE)
     
  }
  
  aesthetic = check_aesthetic(aesthetic = aesthetic, type = 'matrix', location = 'add_aesthetic()')
  group = check_group(group = group, test = grob_object$test, location = 'add_aesthetic()')
  
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

#' Alter aesthetics at certain areas of a grob matrix
#' 
#' Flexibly alter the aesthetic of a grob matrix object at specific points of
#' the data.frame/matrix.
#' 
#' @param grob_object The R6 grob object class initialized by \code{\link{grob_matrix}}
#' or \code{\link{grob_text}}.
#' 
#' @param .f A quosure style lambda \code{~ fun(.)}, which the user wants to apply
#' to the specific subset of cells.
#' 
#' @param ... Logical predicates defined in terms of the variables in the initial 
#' data.frame/matrix, or if the user provides a new data.frame to evaluate via 
#' \code{data}. Multiple conditions are combined with \code{&}. Only rows where the condition 
#' evaluates to TRUE are evaluated. 
#' 
#' If no logical predicates provided, then the entire columns will be altered.
#' 
#' @param columns A character vector of column names, or numeric column indices,
#' of the initial data.frame/matrix, or \code{data} if it is provided, the user wishes to alter.
#' 
#' @param rows A numeric vector of row indices, of the initial data.frame/matrix, 
#' or \code{data} if it is provided, the user wishes to alter.
#' 
#' @param data A separate data.frame/matrix of the same dimensions as the initial 
#' data.frame/matrix which the \code{.f} function and any filters will be applied to.
#' 
#' Must match the dimensions of the subset of the initial data.frame/matrix the user
#' is attempting to alter.
#' 
#' @param aesthetic Which aesthetic the user wants to make alterations to. If left 
#' \code{NULL}, the function will look for the most previous altered aesthetic, either
#' via \code{\link{add_aesthetic}}, or a previous application of \code{alter_at}.
#' 
#' View the documentation of \code{\link{add_aesthetic}} for a list of accepted 
#' aesthetics.
#' 
#' @param group Which group of elements the user wants to make alterations to. If left 
#' \code{NULL}, the function will look for the most previous altered group, either
#' via \code{\link{add_aesthetic}}, or a previous application of \code{alter_at}.
#' 
#' @return The R6 grob matrix object class with its aesthetics properties altered.
#' 
#' @export
#' 
#' @examples 
#' 
#' df = data.frame(var1 = c(5, 14, 6, 10), var2 = c(3, 30, 17, 7))
#' df %>%
#'   grob_matrix() %>%
#'   add_aesthetic(aesthetic = 'text_color', group = 'cells', value = 'red') %>%
#'   alter_at(
#'     .f = ~ 'blue',
#'     abs(var2 - var1) > 1
#'     ) %>%
#'   view_grob()
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
#'   view_grob()
#' 

alter_at = function(grob_object, 
                    .f = NULL,
                    ...,
                    columns = NULL,
                    rows = NULL,
                    data = NULL,
                    aesthetic = NULL,
                    group = NULL) {

  if (!is(grob_object, 'grob_matrix_object') | !grob_object[['type']] %in% 'matrix') {
    
    error_msg = "An object initialized by grob_matrix() must be passed through alter_at()." 
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
    
    columns = 1:ncol(grob_object$initial)
    
  }

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
    # within this function, then we will default to the "cells"
    if (is.na(grob_object$current_group)) {
      
      grob_object$current_group = "cells"
      
    }
    
  }
  
  # - If the user provides data onto which to apply the function to, then we 
  # will make sure the dimensions of the data.frame are correct first.
  if (!is.null(data)) {
    
    proper_dims = grob_object$test %>%
      dplyr::filter(grobblR_group %in% grob_object$current_group) %>%
      dplyr::select(-grobblR_group) %>%
      dim()
    
    data_dims = dim(data)
    
    if (!all(proper_dims == data_dims)) {
      
      error_msg = glue::glue("
        The dimensions of data parameter in alter_at() must be the same dimensions \\
        as the initial matrix/data.frame/vector. 
        The dimensions of the data parameter are {data_dims[1]}x{data_dims[2]}, and \\
        the valid dimensions of the current matrix are {proper_dims[1]}x{proper_dims[2]}.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    # - Now that we've made sure the dimensions of the data parameter are correct,
    # we will go in and empty rows above and below where needed in order to match
    # the dimensions of the current matrix (with added column names and/or column
    # headings)
    current_nc = ncol(grob_object$current)
    total_rows = 1:nrow(grob_object$test)
    target_rows = which(grob_object$test[['grobblR_group']] %in% grob_object$current_group)
    n_rows_above = sum(total_rows < min(target_rows))
    n_rows_below = sum(total_rows > max(target_rows))
    
    df_above = matrix(NA, nrow = n_rows_above, ncol = current_nc) %>% 
      as.data.frame() %>%
      tibble::as_tibble() %>%
      purrr::set_names(colnames(data))
    
    df_below = matrix(NA, nrow = n_rows_below, ncol = current_nc) %>% 
      as.data.frame() %>%
      tibble::as_tibble() %>%
      purrr::set_names(colnames(data))  
    
    data = rbind(df_above, data, df_below)
    data[['grobblR_group']] = grob_object$test[['grobblR_group']]
    
  } else {
    
    data = grob_object$test
    
  }

    # - If no columns are provided, then we will alter all columns.
  if (is.null(columns)) {
    
    columns = 1:ncol(grob_object$initial)
    
  }

  # - Figuring out which indices are each of te selected columns, which will
  # help us if the user wants to apply the function to a different data.frame
  if (!is.numeric(columns)) {
    
    valid_column_names = colnames(data %>% dplyr::select(-grobblR_group))
    which_columns = which(columns %in% valid_column_names)
    
    if (length(which_columns) == 0) {
      
      error_msg = glue::glue("
        None of the column names provided in alter_at() are valid. Valid column names are: \\
        {glue::glue_collapse(valid_column_names, sep = ', ', width = 100)}
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    
  } else {
    
    # - Checking to make sure the numeric column(s) provided are actually valid
    if (!all(dplyr::between(x = columns, left = 1, right = ncol(grob_object$initial)))) {
      
      error_msg = glue::glue("
        If providing numeric column indices in alter_at(), please make sure they are all between \\
        1 and {ncol(grob_object$initial)}.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    which_columns = columns
    
  }
  
  # - If no rows are provided, then we will alter all rows.
  current_group = grob_object$current_group
  cells_rows = nrow(grob_object$test[grob_object$test[['grobblR_group']] %in% 'cells', ])

  # - Since we are putting column names & column headings into the same data.frame 
  # as the cells, we need to adjust which rows to apply the function to based on 
  # what the current group is.
  row_additions = 0 +
    ifelse(
      test = current_group %in% c('cells'),
      yes = grob_object$column_names_to_row + grob_object$column_headings_added,
      no = 0
      ) +
    ifelse(
      test = current_group %in% c('column_names'),
      yes = grob_object$column_headings_added,
      no = 0
      )
  
  subset = grob_object$test[grob_object$test[['grobblR_group']] %in% current_group, ]
  subset_nrow = nrow(subset)
  
  if (is.null(rows)) {
  
    rows = 1:subset_nrow + row_additions
    
  } else {
    
    # - Checking to make sure the user provided numeric row indices 
    if (!is.numeric(rows)) {
      
      error_msg = glue::glue("
        Please provide numeric row indices to manipulate. Based on the initial data, please \\
        provide any row indices 1 through {subset_nrow} within alter_at().
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    # - Checking to make sure the numeric row indices are actually in range for the 
    # given subset of data.
    if (!all(dplyr::between(x = rows, left = 1, right = subset_nrow))) {
      
      error_msg = glue::glue("
        Please provide numeric row between 1 through {subset_nrow} within alter_at().
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    rows = rows + row_additions
    
  }
  
  boolean_vector = data %>%
    dplyr::mutate(
      applied_filter = dplyr::coalesce(eval(filter_expression), FALSE),
      which_to_alter = (
        applied_filter &
          (grobblR_group %in% grob_object$current_group) &
          (dplyr::row_number() %in% rows)
        )
      ) %>%
    dplyr::pull(which_to_alter)
  
  boolean_matrix = aes_matrix(df = data, value = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      -dplyr::matches('grobblR_group')
      ) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(columns),
      .funs = list(~ boolean_vector)
      ) %>%
    as.matrix()
  
  applied_function_mat = data %>%
    dplyr::select(
      -dplyr::matches('grobblR_group')
      ) %>%
    dplyr::filter(boolean_vector) %>%
    dplyr::rowwise() %>%
    dplyr::mutate_at(
      .vars = which_columns,
      .funs = list(.f)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::select(which_columns) %>%
    as.matrix()
  
  grob_object$aesthetic_list[[grob_object$current_aesthetic]][boolean_matrix] = applied_function_mat

  return(grob_object)
  
}

check_aesthetic = function(aesthetic, type, location) {
  
    
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



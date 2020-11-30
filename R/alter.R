
#' Alter aesthetics / structures at certain areas of a grob matrix
#' 
#' Flexibly alter the aesthetic / structure of a grob matrix object at specific points of
#' the data.frame/matrix.
#' 
#' @param grob_object The R6 grob object class initialized by \code{\link{grob_matrix}}.
#' 
#' @param .f A quosure style lambda \code{~ fun(.)}, which the user wants to apply
#' to the specific subset of cells.
#' 
#' @param ... Logical predicates defined in terms of the variables in the initial 
#' data frame / matrix, or if the user provides a new data.frame to evaluate via 
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
#' Ignored if the user is altering a structure and not an aesthetic.
#' 
#' @param data A separate data.frame/matrix of the same dimensions as the initial 
#' data.frame/matrix which the \code{.f} function and any filters will be applied to.
#' 
#' Must match the dimensions of the subset of the initial data.frame/matrix the user
#' is attempting to alter.
#' 
#' Ignored if the user is altering a structure and not an aesthetic.
#' 
#' @param aesthetic Which aesthetic the user wants to make alterations to. If left 
#' \code{NULL} and \code{structure} is left \code{NULL}, the function will 
#' look for the most previous altered aesthetic, either via 
#' \code{\link{add_aesthetic}}, or a previous application of \code{alter_at}.
#' 
#' View the documentation of \code{\link{add_aesthetic}} for a list of accepted 
#' aesthetics.
#' 
#' @param structure Which structure the user wants to make alterations to. If left 
#' \code{NULL} and \code{aesthetic} is left \code{NULL}, the function will 
#' look for the most previous altered structure, either via 
#' \code{\link{add_structure}}, or a previous application of \code{alter_at}.
#' 
#' View the documentation of \code{\link{add_structure}} for a list of accepted 
#' structures
#' 
#' @param group Which group of elements the user wants to make alterations to. If left 
#' \code{NULL}, the function will look for the most previous altered group, either
#' via \code{\link{add_aesthetic}}, or a previous application of \code{alter_at}.
#' 
#' @return The R6 grob matrix object class with its aesthetic / structure properties altered.
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
#' df %>%
#'   grob_matrix() %>%
#'   add_structure("column_widths_p", 1) %>%
#'   alter_at(
#'     .f = ~ 2,
#'     columns = 1
#'     ) %>%
#'   view_grob()
#'   
#' 

alter_at = function(grob_object, 
                    .f = NULL,
                    ...,
                    columns = NULL,
                    rows = NULL,
                    data = NULL,
                    structure = NULL,
                    aesthetic = NULL,
                    group = NULL) {

  if (!methods::is(grob_object, "grob_matrix_object") | !grob_object[['type']] %in% "matrix") {
    
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

  aesthetic_provided = !is.null(aesthetic)
  structure_provided = !is.null(structure)
  last_edit = grob_object$last_edit
  
  # - Making sure the user didn't provide both an aesthetic and a structure
  if (aesthetic_provided & structure_provided) {

      error_msg = glue::glue("
        Please do not provide both an aesthetic and a structure to alter within \\
        alter_at().
        ")

      stop(error_msg, call. = FALSE)

  }
  
  altering = dplyr::case_when(
    aesthetic_provided | last_edit %in% "aesthetic" ~ "aesthetic",
    structure_provided | last_edit %in% "structure" ~ "structure",
    TRUE ~ NA_character_
    )
  
  if (is.na(altering)) {
    
    error_msg = glue::glue("
      Please provide an aesthetic or structure within alter_at().
      ")
    
  } else if (altering %in% "aesthetic") {
  
    # - If no aesthetic is provided in the function parameters, then it is assumed
    # the user wants to alter the last edited aesthetic either from an add_aesthetic() or
    # a previous alter_at().
    if (aesthetic_provided) {
      
      aesthetic = check_aesthetic(aesthetic = aesthetic, location = 'alter_at()')
      
      if (is.null(grob_object$aesthetic_list[[aesthetic]])) {
  
        grob_object$aesthetic_list[[aesthetic]] = aes_matrix(
          df = grob_object$current,
          value = NA
          )
  
      }
      
      grob_object$last_edit = "aesthetic"
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
    
    # - Only checking the group if an aesthetic is being altered, otherwise it will
    # be ignored
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
    
    current_group = grob_object$current_group
    
  } else if (altering %in% "structure") {
    
    # - If no structure is provided in the function parameters, then it is assumed
    # the user wants to alter the last edited structure either from an add_structure() or
    # a previous alter_at().
    if (structure_provided) {
      
      structure = check_structure_validity(structure = structure, location = 'alter_at()')
      
      if (is.null(grob_object$structure_list[[structure]])) {
  
        grob_object$structure_list[[structure]] = aes_matrix(
          df = grob_object$current,
          value = NA,
          column_names = TRUE
          )
  
      }
      
      grob_object$last_edit = "structure"
      grob_object$current_structure = structure
      
    } else {
      
      # - If the user skipped the add_structure() step, and didn't provide a structure
      # within this function, then an error message will pop up.
      if (is.na(grob_object$current_structure)) {
        
        error_msg = glue::glue("
          Please provide a structure to adjust, either within add_structure() \\
          or alter_at().
          ")
        
        stop(error_msg, call. = FALSE)
        
      }
      
    }
    
    current_group = "cells"
    
  }
  
  # - If the user provides data onto which to apply the function to, then we 
  # will make sure the dimensions of the data.frame are correct first.
  # --> Only allowing if altering an aesthetic
  if (!is.null(data) & altering %in% "aesthetic") {
    
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
    # we will go in and add empty rows above and below where needed in order to match
    # the dimensions of the current matrix (with added column names and/or column
    # headings)
    current_nc = ncol(grob_object$current)
    total_rows = 1:nrow(grob_object$test)
    target_rows = which(grob_object$test[['grobblR_group']] %in% grob_object$current_group)
    n_rows_above = sum(total_rows < min(target_rows))
    n_rows_below = sum(total_rows > max(target_rows))
    
    df_above = matrix(NA, nrow = n_rows_above, ncol = current_nc) %>% 
      as.data.frame(stringsAsFactors = FALSE) %>%
      tibble::as_tibble() %>%
      purrr::set_names(colnames(data))
    
    df_below = matrix(NA, nrow = n_rows_below, ncol = current_nc) %>% 
      as.data.frame(stringsAsFactors = FALSE) %>%
      tibble::as_tibble() %>%
      purrr::set_names(colnames(data))  
    
    data = rbind(df_above, data, df_below)
    data[['grobblR_group']] = grob_object$test[['grobblR_group']]
    
  } else if (altering %in% "aesthetic") {
    
    data = grob_object$test
    
  } else if (altering %in% "structure") {
    
    data = utils::head(grob_object$test[grob_object$test[["grobblR_group"]] %in% "cells",], 1)
    
  }

  # - If no columns are provided, then we will alter all columns.
  if (is.null(columns)) {
    
    columns = 1:ncol(grob_object$initial)
    
  }

  # - Figuring out which indices are each of te selected columns, which will
  # help us if the user wants to apply the function to a different data.frame
  if (!is.numeric(columns)) {
    
    valid_column_names = colnames(data %>% dplyr::select(-grobblR_group))
    which_columns = which(valid_column_names %in% columns)
    
    if (length(which_columns) == 0) {
      
      error_msg = glue::glue("
        None of the column names provided in alter_at() are valid. Valid column names are: \\
        {glue::glue_collapse(valid_column_names, sep = ', ', width = 100)}
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    
  } else {
    
    columns = as.integer(columns)
    
    # - Checking to make sure the numeric column(s) provided are actually valid
    if (!all(dplyr::between(x = abs(columns), left = 1, right = ncol(grob_object$initial)))) {
      
      error_msg = glue::glue("
        If providing numeric column indices in alter_at(), please make sure they are all between \\
        1 and {ncol(grob_object$initial)}.
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    which_columns = (1:ncol(grob_object$initial))[columns]
    
  }

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
  
  if (altering %in% "structure") {
    
    rows = 1
  
  } else if (is.null(rows)) {
  
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
    if (!all(dplyr::between(x = abs(rows), left = 1, right = subset_nrow))) {
      
      error_msg = glue::glue("
        Please provide numeric row between 1 through {subset_nrow} within alter_at().
        ")
      
      stop(error_msg, call. = FALSE)
      
    }
    
    rows = (1:subset_nrow)[rows] + row_additions
    
  }
  
  boolean_vector = data %>%
    dplyr::mutate(
      applied_filter = dplyr::coalesce(eval(filter_expression), FALSE),
      which_to_alter = (
        applied_filter &
          (grobblR_group %in% current_group) &
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
      .vars = dplyr::vars(which_columns),
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
  
  if (altering %in% "aesthetic") {
    
    grob_object$aesthetic_list[[grob_object$current_aesthetic]][boolean_matrix] = applied_function_mat
    
  } else if (altering %in% "structure") {
    
    grob_object$structure_list[[grob_object$current_structure]][boolean_matrix] = applied_function_mat
    
  }

  return(grob_object)
  
}


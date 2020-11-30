
testthat::context(glue::glue("
  Testing various scenarios grob_matrix() must pass.
  "))

# Testing Variables ----

mat = matrix(1:4, nrow = 2, byrow = TRUE)

# - Sections below sorted alphabetically
# add_aesthetic() ----

testthat::test_that(
  desc = glue::glue("
    No errors when passing in a matrix with like column names, and we try to \\
    group them together.
    "),
  code = {
    
    mat_grob_matrix = mat %>%
      as.data.frame() %>%
      purrr::set_names("X", "X") %>%
      grob_matrix() %>%
      add_aesthetic("group_elements", TRUE, "column_names")
      
    testthat::expect_true(methods::is(mat_grob_matrix, "R6"))
    
  })

testthat::test_that(
  desc = glue::glue("
    No errors when passing in a matrix of aesthetics to add_aesthetic().
    "),
  code = {
    
    mat_grob_matrix = mat %>%
      as.data.frame() %>%
      grob_matrix() %>%
      add_aesthetic("text_color", matrix("red", nrow = 2, ncol = 2), "cells")
    
    gl = grob_layout(grob_row(grob_col(mat_grob_matrix)))
      
    testthat::expect_true(grid::is.grob(gl$grob))
    
  })

testthat::test_that(
  desc = glue::glue("
    Errors when passing in a wrongly-dimensioned matrix of aesthetics to add_aesthetic().
    "),
  code = {
    
    testthat::expect_error({
    
      mat %>%
        as.data.frame() %>%
        grob_matrix() %>%
        add_aesthetic("text_color", matrix("red", nrow = 2, ncol = 3), "cells")
      
    })
  
  })

# add_column_headings() ----

testthat::test_that(
  desc = glue::glue("
    Errors when trying to add column headings after altering aesthetics.
    "),
  code = {
    
    testthat::expect_error({

      mat %>%
        as.data.frame() %>%
        grob_matrix() %>%
        alter_at(
          ~ "bold",
          aesthetic = "font_face"
          ) %>%
        add_column_headings(c('HEADING'))

    })
  
  })

testthat::test_that(
  desc = glue::glue("
    Warning if the user tries to add column headings at column name positions that don't exist.
    "),
  code = {
    
    testthat::expect_warning({
    
      mat %>%
        as.data.frame() %>%
        purrr::set_names(c("col1", "col2")) %>%
        grob_matrix() %>%
        add_column_headings(
          headings = list("COLUMN1", "COLUMN2"),
          heading_cols = list(1, "z")
          )
      
    })
  
  })

testthat::test_that(
  desc = glue::glue("
    Error if the user tries to add_column_headings() at numeric column indices that don't exist.
    "),
  code = {
    
    testthat::expect_error({
    
      mat %>%
        as.data.frame() %>%
        purrr::set_names(c("col1", "col2")) %>%
        grob_matrix() %>%
        add_column_headings(
          headings = list("COLUMN1", "COLUMN2"),
          heading_cols = list(20, "col1")
          )
      
    })
  
  })


# add_structure() ----

testthat::test_that(
  desc = glue::glue("
    No errors when passing in a vector of structures to add_structure().
    "),
  code = {
    
    mat_grob_matrix = mat %>%
      as.data.frame() %>%
      grob_matrix() %>%
      add_structure("column_widths_p", c(1, 2))
    
    gl = grob_layout(grob_row(grob_col(mat_grob_matrix)))
      
    testthat::expect_true(grid::is.grob(gl$grob))
    
  })

testthat::test_that(
  desc = glue::glue("
    Errors when passing in a wrong-length vector of structures to add_structure().
    "),
  code = {
    
    testthat::expect_error({
    
      mat %>%
        as.data.frame() %>%
        grob_matrix() %>%
        add_structure("column_widths_p", c(1, 2, 3))
      
    })
    
  })

# alter_at() ----

testthat::test_that(
  desc = glue::glue("
    No errors when passing in a matrix without column names, and altering \\
    individual cells.
    "),
  code = {
    
    mat_grob_matrix = mat %>%
      grob_matrix() %>%
      alter_at(~ "red", columns = 1, aesthetic = "text_color") %>%
      alter_at(~ "blue", columns = 2, rows = 2, aesthetic = "background_color") %>%
      alter_at(~ "white", columns = 2, rows = 2, aesthetic = "text_color")
    
    testthat::expect_true(methods::is(mat_grob_matrix, "R6"))
    
  })

testthat::test_that(
  desc = glue::glue("
    Errors when trying to alter an aesthetic and a structure at the same time.
    "),
  code = {
    
    testthat::expect_error({
    
      mat %>%
        as.data.frame() %>%
        grob_matrix() %>%
        alter_at(
          ~ 2,
          structure = "column_widths_p",
          aesthetic = "font_face"
          )
      
    })
  
  })

testthat::test_that(
  desc = glue::glue("
    The correct aesthetic column is being altered when selecting specific column names.
    "),
  code = {
    
    testthat::expect_true({
    
      gm = mat %>%
        as.data.frame() %>%
        purrr::set_names(c("var1", "var2")) %>%
        grob_matrix() %>%
        alter_at(
          ~ 2,
          columns = "var2",
          aesthetic = "font_face"
          )
      
      all(gm[["aesthetic_list"]][["font_face"]][-1, 2] == 2)
      
    })
  
  })

# alter_column_names() ----

testthat::test_that(
  desc = glue::glue("
    The front facing data frame is altered by alter_column_names() but the \\
    underlying testing data frame is not.
    "),
  code = {
    
    testthat::expect_true({
    
      gm = mat %>%
        as.data.frame() %>%
        purrr::set_names(c("col1", "col2")) %>%
        grob_matrix() %>%
        alter_column_names(
          column_names = list("COLUMN1"),
          column_name_cols = list(1:2)
          )
  
      all(
        gm[["current"]][1,1] == "COLUMN1",
        gm[["current"]][1,2] == "COLUMN1",
        colnames(gm[["test"]])[1] == "col1",
        colnames(gm[["test"]])[2] == "col2"
        )
      
    })
  
  })

testthat::test_that(
  desc = glue::glue("
    Warning if the user tries to alter column names at column name positions that don't exist.
    "),
  code = {
    
    testthat::expect_warning({
    
      mat %>%
        as.data.frame() %>%
        purrr::set_names(c("col1", "col2")) %>%
        grob_matrix() %>%
        alter_column_names(
          column_names = list("COLUMN1", "COLUMN2"),
          column_name_cols = list(1, "z")
          )
      
    })
  
  })

testthat::test_that(
  desc = glue::glue("
    Error if the user tries to alter column names at numeric column indices that don't exist.
    "),
  code = {
    
    testthat::expect_error({
    
      mat %>%
        as.data.frame() %>%
        purrr::set_names(c("col1", "col2")) %>%
        grob_matrix() %>%
        alter_column_names(
          column_names = list("COLUMN1", "COLUMN2"),
          column_name_cols = list(10, "col1")
          )
      
    })
  
  })

testthat::test_that(
  desc = glue::glue("
    Error if the user tries to alter column names of a matrix with no column names initially.
    "),
  code = {
    
    testthat::expect_error({
    
      mat %>%
        grob_matrix() %>%
        alter_column_names(
          column_names = list("COLUMN1", "COLUMN2"),
          column_name_cols = list(10, "col1")
          )
      
    })
  
  })

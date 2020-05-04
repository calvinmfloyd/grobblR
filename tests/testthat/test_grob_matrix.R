
testthat::context(glue::glue("
  Testing various scenarios grob_matrix() must pass, that have errored in \\
  the past.
  "))

# Testing Variables ----

mat = matrix(1:4, nrow = 2, byrow = TRUE)

# Tests ----

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


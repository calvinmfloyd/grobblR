
testthat::context(glue::glue("
  Testing various matrix aesthetics scenarios that have errored in the past.
  "))

# Testing Variables ----

mat = matrix(1:4, nrow = 2, byrow = TRUE)
df = as.data.frame(mat, stringsAsFactors = FALSE)

# Tests ----

testthat::test_that(
  desc = glue::glue("
    No errors when inputting numeric text alignments and justifications.
    "),
  code = {
    
    mat_grob_matrix = df %>%
      grob_matrix() %>%
      add_aesthetic("text_align", 0.9, "cells") %>%
      add_aesthetic("text_just", 0.9, "cells")
    
    gl = grob_layout(grob_row(grob_col(mat_grob_matrix)))

    testthat::expect_true(grid::is.grob(gl$grob))
    
  })

testthat::test_that(
  desc = glue::glue("
    Errors when inputting a text horizontal alignment that doesn't fit our \\
    text aligning conventions.
    "),
  code = {
    
    expect_error({
      
      df %>%
        grob_col(
          aes_list = ga_list(text_align = "leftt")
          ) %>%
        view_grob()
      
    })
    
  })

testthat::test_that(
  desc = glue::glue("
    Errors when inputting a text vertical alignment that doesn't fit our \\
    text aligning conventions.
    "),
  code = {
    
    expect_error({
      
      df %>%
        grob_col(
          aes_list = ga_list(text_v_align = "toppppp")
          ) %>%
        view_grob()
      
    })
    
  })

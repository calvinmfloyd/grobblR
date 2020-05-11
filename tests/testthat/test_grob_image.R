
testthat::context(glue::glue("
  Testing various scenarios grob_image() must pass, that have errored in \\
  the past.
  "))

# Testing Variables ----

url = "https://logos-download.com/wp-content/uploads/2016/04/Sacramento_Kings_logo_white.png"
faulty_url = paste0(url, "aasdfsaf")

# Tests ----

testthat::test_that(
  desc = glue::glue("
    No errors when passing in a valid URL to a .png image.
    "),
  code = {
    
    url_gl = url %>%
      grob_image() %>%
      grob_col() %>%
      grob_row() %>%
      grob_layout()

    testthat::expect_true(grid::is.grob(url_gl$grob))
    
  })

testthat::test_that(
  desc = glue::glue("
    Errors when passing in a faulty URL to a .png image.
    "),
  code = {

    testthat::expect_error({
      
      faulty_url %>%
        grob_image()
      
    })
    
  })

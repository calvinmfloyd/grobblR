
testthat::context(glue::glue("
  Testing various scenarios grob_image() must pass.
  "))

# Testing Variables ----
faulty_url = "https://static.vecteezy.com/system/resources/aasdfsaf"

# Tests ----
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

with_mock_api({
  
  test_that("zz_format is extracting proper values if no origin param is provided", {
    
    # You should get a new fixture for formats-48c24b.json
    # (it's merely a subset containing the first fifty. Actually there should be over 100)
    
    out <- zz_format()
    testthat::expect_output(print(out[1,]), "3g2")
    testthat::expect_output(print(out[20,]), "csv")
    testthat::expect_output(print(out[27,]), "emf")
    testthat::expect_output(print(out[35,]), "gvi")
    testthat::expect_output(print(out[50,]), "msg")

    # General sanity
    testthat::expect_length(out, 1)
    
    testthat::expect_is(out, "data.frame")
    
    
  })
  
  test_that("zz_format is extracting proper values if a origin jpeg is provided", {
    
    out <- zz_format(origin = "jpeg")
    testthat::expect_output(print(out$target[[1]]), "bmp")
    testthat::expect_output(print(out$target[[3]]), "ico")
    testthat::expect_output(print(out$target[[5]]), "pdf")
    testthat::expect_output(print(out$target[[9]]), "thumbnail")
    testthat::expect_equal(print(out$cost[[1]]), 1)
    testthat::expect_equivalent(print(out$cost[[1]]), 1)
    
    # General sanity
    testthat::expect_length(out, 2)
    
    testthat::expect_is(out, "data.frame")
    
  })
  
})


test_that("zz_format throws a proper error if an invalid usr has been passed", {
  
  testthat::expect_error(zz_format(usr = "foo"), "Zamzar responded with: API key was missing or invalid, and a status code of: 401")
  
})


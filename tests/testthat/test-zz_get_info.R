with_mock_api({
  
  test_that("zz_get_info returns proper values", {
    
    info <- zz_get_info()
    testthat::expect_output(print(info$extension), "gif")
    testthat::expect_output(print(info$id), "66741653")
    testthat::expect_output(print(info$created_at), "2020-01-29T15:27:41Z")
    
    testthat::expect_is(info, "data.frame")

  })
  
})


test_that("zz_get_info throws a proper error if an invalid usr has been passed", {
  
  testthat::expect_error(zz_get_info(usr = "foo"),
                         "Zamzar responded with API key was missing or invalid and a status code of: 401")
  
})
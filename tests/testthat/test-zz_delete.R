test_that("An error is thrown if we forget various params", {
  
  expect_error(zzlite::zz_delete(), "Whoops, seems like you forgot to pass an id!")
  
})


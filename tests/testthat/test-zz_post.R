test_that("An error is thrown if we forget various params", {
  
  expect_error(zzlite::zz_post(), "Whoops, please pass a file!")
  
  expect_error(zzlite::zz_post(file = avatar_emf), "Excuse me, but I need to know the extension type; please pass it :-)")

})
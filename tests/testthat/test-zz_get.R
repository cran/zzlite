test_that("An error is thrown if we forget various params", {
            
            expect_error(zzlite::zz_get(), "Whoops, seems like you forgot to pass an id!")
            expect_error(zzlite::zz_get(id = 123), "Whoops, please let me know the format of the file I should get.")

          }
        )
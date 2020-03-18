zz_config <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  
  prod <- list(
    post <- c("https://api.zamzar.com/v1/jobs"),
    get <- c("https://api.zamzar.com/v1/files/")
  )
    
  dev <- list(
    post <- c("https://sandbox.zamzar.com/v1/jobs"),
    get <- c("https://sandbox.zamzar.com/v1/files/")
  )
    
  format <- list(
    format <- c("https://sandbox.zamzar.com/v1/formats")
  )

  assign("prod", prod, envir = zz_config)
  assign("dev", dev, envir = zz_config)
  assign("format", format, envir = zz_config)

}
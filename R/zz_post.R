#' Post file to Zamzar endpoint
#' 
#' Per default `zz_post()` assumes that you're doing development, thus using a 
#' development endpoint. Set parameter `prod` to `TRUE` to change this behavior.  
#' 
#' On differences between endpoints, see: \url{https://developers.zamzar.com/docs#section-Next_steps} and  \url{https://developers.zamzar.com/docs#section-Rate_Limits}
#'
#' Please note that a Zamzar key passed as argument to `usr` takes precedence over a
#' Zamzar key extracted from an `.Renviron`.  
#'
#'
#' @param file The path to the file you want to convert.  
#' 
#' @param extension The file type you want to convert to. E.g., `png`.  
#' 
#' @seealso \code{\link{zz_format}} for a list of formats you can convert to.
#' 
#' @param usr The username/API key you are using. If not set, the function
#' will check if a key exists as a `ZAMZAR_USR` variable  in `.Renviron` and use that.    
#' 
#' See: \url{https://developers.zamzar.com/user}
#' 
#' @param prod Boolean deciding whether to use production or development endpoint.
#' Defaults to `FALSE`.
#' 
#' @param verbose Boolean deciding whether or not verbose status messages
#' should be returned. Defaults to `FALSE`.
#'
#' @export
#' @return A status message indicating either success or failure.
#' 
#' @examples
#' \dontrun{
#' # Per default zz_post uses the development endpoint.
#' zz_post(file = "avatar.emf", extension = "png")
#' 
#' # Setting prod parameter to FALSE is the same as above.
#' zz_post(file = "avatar.emf", extension = "png", prod = FALSE)
#' 
#' # You need to flip prod to TRUE if you want to use the production endpoint.
#' zz_post(file = "avatar.emf", extension = "png", prod = TRUE)
#' 
#' # Remember you can always pass a Zamzar key to the usr parameter if you don't
#' # want to use an .Renviron file.
#' zz_post(file = "avatar.emf", usr = "key", extension = "png", prod = TRUE)
#'  
#' }

zz_post <- function(file = NULL, extension = NULL, usr = NULL, prod = FALSE, verbose = FALSE) {

  if (is.null(file)) {
    stop("Whoops, please pass a file!")
  }
  
  if (is.null(extension)) {
    stop("Excuse me, but I need to know the extension type; please pass it :-)")
  }
  
  usr <- .zz_get_key(usr = usr)
  
  if (prod == FALSE) {
    endpoint <- zz_config[['dev']][[1]]
  } 
  
  if (prod == TRUE) {
    endpoint <- zz_config[['prod']][[1]]
  }
  
  body <- list(source_file = httr::upload_file(path = file),
               target_format = extension)
  
  response <- httr::POST(url = endpoint,
       config = .zz_authenticate(usr),
       body = body,
       .zz_user_agent()
  )
  
  res <- data.frame(status = response[['status_code']],
                    endpoint = response[['url']],
                    date = response[['date']])
  
  if (verbose == TRUE) {
    res <- res
  } else {
    res <- res[['status']]
  }
  
  return(res)
}
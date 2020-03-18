#' Delete file from Zamzar account
#' 
#' Delete file from Zamzar account provided a file id.
#'
#' Please note that a Zamzar key passed as argument to `usr` takes precedence over a
#' Zamzar key extracted from an `.Renviron`.  
#' 
#' 
#' @param id The target id for a file you wish to delete. Most likely returned from ‘zz get info()‘.
#'
#' @param usr The username/API key you are using. If not set, the function
#' will check if a key exists as a `ZAMZAR_USR` variable  in `.Renviron` and use that.    
#' 
#' See: \url{https://developers.zamzar.com/user}
#' 
#' @param verbose Boolean deciding whether or not verbose status messages
#' should be returned. Defaults to `FALSE`.  
#'
#' @export
#' @return A status message indicating either success or failure.
#' 
#' @examples
#' \dontrun{
#' # An example of zz_delete() with a hardcoded file id
#' 
#' zz_get(id = 12345678)
#' }


zz_delete <- function(id = NULL, usr = NULL, verbose = FALSE) {
 
  if (is.null(id)) {
    stop("Whoops, seems like you forgot to pass an id!")
  }
 
  usr <- .zz_get_key(usr = usr)

  endpoint <- zz_config[['prod']][[2]]
  
  response <- httr::DELETE(url = paste0(endpoint, id),
                           config = .zz_authenticate(usr),
                           .zz_user_agent()
  )

  if (!response[['status_code']]  == 200) {
    stop(sprintf("Zamzar responded with a status code of: %d",
                 response[['status_code']])
    )
  } else {

    if (verbose == FALSE) {
      res <- response[['status_code']]
    } else {
      res <- data.frame(id = id,
                        status_code = response[['status_code']],
                        deleted_at = response[['date']],
                        stringsAsFactors = FALSE)
    }
  }
  return(res)
}

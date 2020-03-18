#' Get file from Zamzar account
#' 
#' Get file from Zamzar account provided a file id.
#' Per default `zz_get()` assumes that you're doing development, thus using a 	
#' development endpoint. Set prod boolean to `TRUE` to change this behavior.
#' 
#' On differences between endpoints, see: \url{https://developers.zamzar.com/docs#section-Next_steps} and  \url{https://developers.zamzar.com/docs#section-Rate_Limits}
#'
#' Please note that a Zamzar key passed as argument to `usr` takes precedence over a
#' Zamzar key extracted from an `.Renviron`.  
#' 
#' 
#' @param id The target id for a previously passed file. Most likely
#' returned from `zz_get_info()`.
#'
#' @param usr The username/API key you are using. If not set, the function
#' will check if a key exists as a `ZAMZAR_USR` variable  in `.Renviron` and use that.    
#' 
#' See: \url{https://developers.zamzar.com/user}
#' 
#' @param name The name of the file you are fetching from Zamzar. If a name is
#' not assigned to the file, then the id of the file will be used as file name.
#'
#' @param extension The extension of the file you are fetching from Zamzar. 
#' 
#' @param overwrite Should `zz_get()` overwrite if a file with the same name already
#' exists in directory. Defaults to `FALSE`.
#'
#' @param prod Boolean deciding whether to use a production endpoint or
#' a development endpoint. Defaults to FALSE (That is, development endpoint).
#'
#' @export
#' @return A file written to disk.
#' 
#' @examples
#' \dontrun{
#' # An example of zz_get() utilized with hardcoded arguments
#' zz_get(id = 12345678, usr = "key", name = "my_avatar", extension = "png")
#' 
#' # An example of zz_get() used in conjunction with zz_get_info()
#' # Please note this example assumes a valid key in .Renviron
#' response <- zz_get_info(latest = TRUE)
#' zz_get(id = response$id, extension = response$extension, prod = TRUE)
#' }


zz_get <- function(id = NULL,
                   usr = NULL,
                   name = NULL,
                   extension = NULL,
                   overwrite = FALSE,
                   prod = FALSE) {
  
  if (is.null(id)) {
    stop("Whoops, seems like you forgot to pass an id!")
  }
  
  usr <- .zz_get_key(usr = usr)
  
  if (is.null(extension)) {
    stop("Whoops, please let me know the extension of the file I should get.")
  }
  
  if (is.null(name)) {
    id <- as.character(id)
  }
  
  if (prod == FALSE) {
    endpoint <- zz_config[['dev']][[2]]
  }
  
  if (prod == TRUE) {
    endpoint <- zz_config[['prod']][[2]]
  }
  
  # Concatenating an URL
  url <- .zz_endpoint_content(endpoint = endpoint, id = id)
  
  if (!is.null(name)) {
    id <- name
  }
  
  identifier <- paste0(id, ".", extension)
  
  response <- httr::GET(url,
                        httr::write_disk(identifier, overwrite = overwrite),
                        config = .zz_authenticate(usr),
                        .zz_user_agent()
  )
  
  if (!response$status_code %in% c(200, 201)) {
    stop(sprintf("Zamzar responded with a status code of: %d",
                 response[['status_code']])
    )
  } else {
    message(sprintf("Writing file %s to %s", identifier, getwd()))
  }
  
  # Delete file if status code indicates so 
  if (response[['status_code']] %in% c(404)) {
    unlink(identifier)
  }
  
}
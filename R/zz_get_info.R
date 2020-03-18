#' Get info from Zamzar
#' 
#' Get info on files submitted to Zamzar by account.
#'
#' Per default `zz_get_info()` assumes you want information for the last 
#' submitted file. To get information on all the files that have been submitted
#' within a reasonable time frame, set parameter `latest` to `FALSE`.
#' 
#' Please note: objects returned from `zz_get_info()` doesn't differentiate
#' between development or production endpoint. You have to keep track of 
#' this yourself.  
#'
#' Please note that a Zamzar key passed as argument to `usr` takes precedence over a
#' Zamzar key extracted from an `.Renviron`.  
#'
#'
#' @param usr The username/API key you are using. If not set, the function
#' will check if a key exists as a `ZAMZAR_USR` variable  in `.Renviron` and use that.    
#' 
#' See: \url{https://developers.zamzar.com/user}
#'
#' @param latest Boolean deciding whether or not metadata on all files that 
#' have been submitted within a reasonable time frame should be returned.  
#' 
#' If switched to `FALSE`, metadata on all files that have been submitted to
#' the Zamzar API within a reasonable time frame will be returned.  
#' 
#' Defaults to `TRUE`.
#' 
#' The returned dataframe contains the following columns:  
#' 
#'   * `id`: The unique file identifier assigned to a file by Zamzar. 
#'   * `extension`: The extension representing the format of the file you can download. 
#'   * `created_at`: The time at which the file was created at the Zamzar servers. 
#'
#'@md
#'
#' @export
#' @return A dataframe.
#' 
#' 
#' @examples
#' \dontrun{
#' # Provided a valid token in .Renvirion, a dataframe of metadata for the last 
#' # submitted file will be returned.
#' zz_get_info()
#' 
#' # Same as above, we're just passing the key in a variable.
#' zz_get_info(usr = "key")
#'  
#' # Provided a valid token, will return metadata for all files
#' # submitted to the API within a reasonable time frame.
#' zz_get_info(usr = "key", latest = FALSE)
#' 
#' # Same as above, we're just utilizing .Renviron.
#' zz_get_info(latest = FALSE)
#' }

zz_get_info <- function(usr = NULL, latest = TRUE) {
  
  endpoint <- zz_config[['prod']][[2]]
  
  usr <- .zz_get_key(usr = usr)
  
  response <- httr::GET(url = endpoint,
                      config = .zz_authenticate(usr),
                      .zz_user_agent()
  )
  
  content <- .zz_parse_response(response = response)
  
  if (!response[['status_code']]  == 200) {
    stop(sprintf("Zamzar responded with %s and a status code of: %d",
                 content[['errors']][['message']],
                 response[['status_code']]
                 )
    )
  }
  
  if (isTRUE(content[['paging']][['total_count']] == 0) && isTRUE(content[['paging']][['limit']] == 50)) {
    stop("Seems like Zamzar doesn't store any files submitted by this account!")
  }
  
  if (latest == TRUE) {
    res <- data.frame(id = content[['data']][['id']][[1]],
                      extension = content[['data']][['format']][[1]],
                      created_at = content[['data']][['created_at']][[1]],
                      stringsAsFactors = FALSE)
  } else {
    res <- data.frame(id = content[['data']][['id']],
                      extension = content[['data']][['format']],
                      created_at = content[['data']][['created_at']],
                      stringsAsFactors = FALSE)
  }
  
  return(res)

}
#' Accepted formats from Zamzar
#' 
#' Get dataframe of all the formats accepted by Zamzar. Alternatively, a dataframe of formats
#' you can convert an origin to.
#' 
#' Please note that a Zamzar key passed as argument to `usr` takes precedence over a
#' Zamzar key extracted from an `.Renviron`.  
#' 
#' @param origin The origin format you want to convert from.
#' If a valid argument is passed to `origin`, `zz_format()` returns a dataframe of:  
#' 
#'   * `targets`: The formats your origin can be converted to.
#'   * `costs`: The cost for converting between your origin and a given target.  
#' 
#' See also: \url{https://developers.zamzar.com/formats}  
#'
#' If no argument has been passed to `orign`, a dataframe containing all
#' the accepted formats by the Zamzar API is returned.  
#' 
#' See: \url{https://developers.zamzar.com/formats}
#'
#'
#' @param usr The username/API key you are using. If not set, the function
#' will check if a key exists as a `ZAMZAR_USR` variable  in `.Renviron` and use that.    
#' 
#' See: \url{https://developers.zamzar.com/user}
#'
#'  
#' @md
#' 
#' @export
#' @return Either a dataframe of formats that you can convert to, or a
#' dataframe of accepted origin formats.
#' 
#' @examples
#' \dontrun{
#' # Returns a single column dataframe of all the accepted formats
#' # for the origin param.
#' zz_format(usr = "key")
#' 
#' # Same as above (assuming a valid key in .Renviron).
#' zz_format()
#' 
#' # Returns an error since the origin argument isn't recognized by the Zamzar API.
#' zz_format(origin = "invalid_origin")
#' 
#' # Returns a dataframe of targets that origin can be converted to,
#' # and of the cost of converting to a given target.
#' zz_format(origin = "emf")
#' }

zz_format <- function(origin = NULL, usr = NULL) {
  
  usr <- .zz_get_key(usr = usr)
  
  if (is.null(origin) || origin == "") {
    endpoint <- zz_config[['format']][[1]]
  } else {
    endpoint <- paste0(zz_config[['format']][[1]], "/", origin)
  }
  
  response <- httr::GET(endpoint,
                        config = .zz_authenticate(usr = usr),
                        .zz_user_agent()
                        )
  
  content <- .zz_parse_response(response = response)

  if (!response[['status_code']] %in% c(200, 201)) {
    stop(sprintf("Whoops! Zamzar responded with: %s, and a status code of: %d",
                 content[['errors']][['message']],
                 response[['status_code']])
    )
  }
  
  container <- data.frame(target = content[['data']][['name']],
                          stringsAsFactors = FALSE)
  
  # Checking if we should do paging (more than 50)
  if(length(content[['data']][['name']]) >= 50) {
    container <- .zz_do_paging(content = content,
                               container = container,
                               endpoint = endpoint,
                               usr = usr)
  }


  if (is.null(origin) || origin == "") {
    res <- container
  } else {
    res <- data.frame(target = content[['targets']][['name']],
                      cost = content[['targets']][['credit_cost']],
                      stringsAsFactors = FALSE)
  }
  

  if (response[['status_code']] %in% c(200, 201)) {
    return(res)
  } else {
    stop(sprintf("Whoops! Zamzar responded with: %s, and status code %d.",
                 content[['errors']][['message']],
                 response[['status_code']])
    )
  }
}

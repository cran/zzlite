# .zz_endpoint_content
#' 
#' Auxiliary function
#' 
#' Decorate an endpoint with a content path
#' 
#' @keywords internal
.zz_endpoint_content <- function(endpoint, id) {
  url <- paste0(endpoint, id, "/content")
}

# .zz_get_key
#' 
#' Auxiliary function
#' 
#' Get Zamzar key from .Renviron
#' 
#' @keywords internal
.zz_get_key <- function(usr) {
  if (is.null(usr)) {
    Sys.getenv("ZAMZAR_USR", "")
  } else {
    usr <- usr
  }
}

# .zz_authenticate
#' 
#' Auxiliary function
#' 
#' Wrapper for httr::authenticate
#' 
#' @keywords internal
.zz_authenticate <- function(usr) {
  httr::authenticate(
    user = usr,
    password = "",
    type = "basic"
  )
}

# .zz_parse_response
#' 
#' Auxiliary function
#' 
#' Wrapper for parsing of responses
#' 
#' @keywords internal
.zz_parse_response <- function(response) {
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(content, flatten = TRUE)
}

# .zz_user_agent
#' 
#' Auxiliary wrapper for user agent
#'  
#' @keywords internal
.zz_user_agent <- function() {
  httr::user_agent("zzlite - https://github.com/fkoh111/zzlite")
}

# .zz_do_paging
#' 
#' Auxiliary function
#' 
#' Function that deals with paging
#' 
#' @keywords internal
.zz_do_paging <- function(content, container, endpoint, usr) {
  if (content[['paging']][['total_count']] > length(content[['data']][['name']])) {
    
    storage <- list()

    counter <- ceiling(content[['paging']][['total_count']] / length(content[['data']][['name']]))
    
    for(i in 1:counter) {
      state_last_target <- content[['paging']][['last']]
      
      paged_endpoint <- httr::modify_url(endpoint, query = list(after=state_last_target))
      
      paged_response <- httr::GET(paged_endpoint,
                                  config = .zz_authenticate(usr),
                                  .zz_user_agent()
      )
      
      content <- .zz_parse_response(response = paged_response)
      
      temp <- data.frame(target = content[['data']][['name']],
                         stringsAsFactors = FALSE)
      
      storage[[i]] <- temp
      
    }
    
    storage <- do.call(rbind, storage)
    container <- rbind(container, storage)
  }
}
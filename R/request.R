## Support for easily sending requests to the plumber without running it
plumber_request <- function(plumber, method, path, query = NULL,
                            body = NULL, content_type = NULL) {
  req <- new.env(parent = emptyenv())
  req[["REQUEST_METHOD"]] <- toupper(method)
  req[["PATH_INFO"]] <- path
  req[["QUERY_STRING"]] <- query_string(query)

  req[["rook.input"]] <- list(
    read_lines = function() body,
    read = function() body)

  if (!is.null(body)) {
    ## TODO: could set this in the header too, especially when
    ## starting to work with header inputs more generally.  We'd be
    ## looking to set the HEADERS field with a lower case:
    ## "content-type" = content_type, along with the "content-length"
    ## being a string which is the length.  Deal with that at the same
    ## time as the accept header?
    req$HTTP_CONTENT_TYPE <- request_content_type(body, content_type)
  }

  res <- plumber_response()
  plumber$serve(req, res)
}


## TODO: we might not really need the whole thing here, but this is
## a problem potentially.  The input interface (res) folllows the Rook format
##
## https://www.rplumber.io/docs/routing-and-input.html#input-handling
## but the response format is undocumented
##
## https://www.rplumber.io/docs/rendering-and-output.html#response-object
##
## The format is described and conforms to the Rook interface but this
## class is not exported.
plumber_response <- function() {
  plumber:::PlumberResponse$new()
}


query_string <- function(query) {
  if (is.null(query)) {
    return("")
  }
  assert_named(query)
  pairs <- sprintf("%s=%s", names(query), as.character(query))
  utils::URLencode(paste0("?", paste(pairs, collapse = "&")))
}


## Eventually we might move directly to using httr requests at which
## point this goes away because we'd do perhaps httr::upload_file() or
## something?
request_content_type <- function(body, content_type) {
  if (!is.null(content_type)) {
    return(content_type)
  }
  if (is.raw(body)) {
    return("application/octet-stream")
  } else {
    return("application/json")
  }
}

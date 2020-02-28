## Support for easily sending requests to the plumber without running it
plumber_request <- function(plumber, method, path, query = NULL) {
  req <- new.env(parent = emptyenv())
  req[["REQUEST_METHOD"]] <- toupper(method)
  req[["PATH_INFO"]] <- path
  req[["QUERY_STRING"]] <- query_string(query)
  req[["rook.input"]] <- list(read_lines = function() "")

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
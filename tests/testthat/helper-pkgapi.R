## TODO: required with current plumber?
options(plumber.debug = FALSE)

## TODO: this moves elsewhere
test_call <- function(p, verb, path) {
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- toupper(verb)
  req$PATH_INFO <- path
  req$QUERY_STRING <- ""
  req$rook.input <- list(read_lines = function() "")

  res <- plumber_response()
  p$serve(req, res)
}


get_error <- function(expr) {
  tryCatch(expr, error = identity)
}


## TODO: we might not really need the whole thing here, but this is
## a problem potentially.  The format is described and conforms to
## the Rook interface but this class is not exported.
plumber_response <- function() {
  plumber:::PlumberResponse$new()
}

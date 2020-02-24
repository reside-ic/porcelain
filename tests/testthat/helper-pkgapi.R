## TODO: required with current plumber?
options(plumber.debug = FALSE)

## TODO: this moves elsewhere
test_call <- function(p, verb, path) {
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- toupper(verb)
  req$PATH_INFO <- path
  req$QUERY_STRING <- ""
  req$rook.input <- list(read_lines = function() "")

  ## TODO: we might not really need the whole thing here, but this is
  ## a problem potentially.  The format is described and conforms to
  ## the Rook interface but this class is not exported.
  res <- plumber:::PlumberResponse$new()
  p$serve(req, res)
}


to_json_string <- function(x) {
  as.character(to_json(x))
}

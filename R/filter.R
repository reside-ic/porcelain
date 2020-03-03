pkgapi_filters <- function(req, res) {
  list(query_string = pkgapi_filter_query_string,
       post_body = pkgapi_filter_post_body)
}


pkgapi_filter_query_string <- function(req, res) {
  req$pkgapi_query <- plumber:::parseQS(req$QUERY_STRING)
  plumber::forward()
}


pkgapi_filter_post_body <- function(req, res) {
  ## We should look at req$HTTP_CONTENT_TYPE here and behave
  ## accordingly, because we don't want to send the raw output through
  ## to a text-wanting input!
  req$pkgapi_body <- req[["rook.input"]]$read()
  plumber::forward()
}

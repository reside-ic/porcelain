pkgapi_filters <- function(req, res) {
  list(query_string = pkgapi_filter_query_string,
       post_body = pkgapi_filter_post_body)
}


pkgapi_filter_query_string <- function(req, res) {
  req$pkgapi_query <- plumber:::parseQS(req$QUERY_STRING)
  plumber::forward()
}


pkgapi_filter_post_body <- function(req, res) {
  ## req$pkgapi_body <- ???
  plumber::forward()
}

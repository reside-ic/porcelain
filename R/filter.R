pkgapi_filters <- function(req, res) {
  list(query_string = pkgapi_filter_query_string,
       post_body = pkgapi_filter_post_body)
}


pkgapi_filter_query_string <- function(req, res) {
  req$pkgapi_query <- plumber:::parseQS(req$QUERY_STRING)
  plumber::forward()
}


pkgapi_filter_post_body <- function(req, res) {
  type <- parse_mime(req$HTTP_CONTENT_TYPE)
  input <- req[["rook.input"]]
  if (isTRUE(type$is_text)) {
    value <- paste0(input$read_lines(), collapse = "\n")
  } else {
    value <- input$read()
  }
  req$pkgapi_body <- pkgapi_body(type, value)
  plumber::forward()
}


pkgapi_body <- function(type, value) {
  list(type = type, value = value, provided = length(value) > 0L)
}

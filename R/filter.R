porcelain_filters <- function(req, res) {
  list(query_string = porcelain_filter_query_string,
       post_body = porcelain_filter_post_body)
}


porcelain_filter_query_string <- function(req, res) {
  req$porcelain_query <- plumber:::parseQS(req$QUERY_STRING)
  plumber::forward()
}


porcelain_filter_post_body <- function(req, res) {
  type <- parse_mime(req$HTTP_CONTENT_TYPE)
  input <- req[["rook.input"]]
  if (isTRUE(type$is_text)) {
    value <- paste0(input$read_lines(), collapse = "\n")
  } else {
    value <- input$read()
  }
  req$porcelain_body <- porcelain_body(type, value)
  plumber::forward()
}


porcelain_body <- function(type, value) {
  list(type = type, value = value, provided = length(value) > 0L)
}

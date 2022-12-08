porcelain_filters <- function(req, res) {
  list(query_string = porcelain_filter_query_string,
       post_body = porcelain_filter_post_body,
       metadata = porcelain_filter_metadata)
}


porcelain_filter_query_string <- function(req, res) {
  req$porcelain_query <- tryCatch(
    parse_query(req$QUERY_STRING),
    error = function(e) porcelain::porcelain_stop(e$message))
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


porcelain_filter_metadata <- function(req, res) {
  req$received_time <- now_utc()
  plumber::forward()
}

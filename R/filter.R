porcelain_filters <- function(logger) {
  list(query_string = porcelain_filter_query_string,
       request_id = porcelain_filter_request_id(logger),
       post_body = porcelain_filter_post_body,
       metadata = porcelain_filter_metadata)
}


porcelain_filter_query_string <- function(req, res) {
  req$porcelain_query <- parse_query(req$QUERY_STRING)
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


porcelain_filter_request_id <- function(logger) {
  force(logger)
  function(req, res) {
    request_id <- req$HTTP_X_REQUEST_ID %||% ids::uuid()
    req$REQUEST_ID <- request_id
    res$setHeader("x-request-id", request_id)
    if (!is.null(logger)) {
      logger$add_filter(lgr::FilterInject$new(request_id = request_id),
                        name = LOG_FILTER_REQUEST_ID_NAME)
    }
    plumber::forward()
  }
}

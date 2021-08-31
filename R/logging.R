porcelain_log_preroute <- function(logger) {
  force(logger)
  function(data, req, res) {
    logger$info("request %s %s", req$REQUEST_METHOD, req$PATH_INFO)
    logger_detailed(logger, "trace", req, "request")
  }
}


porcelain_log_postserialize <- function(logger) {
  force(logger)

  safe_body <- function(body) {
    if (is.raw(body)) {
      body <- sprintf("<binary body (%d bytes)>", length(body))
    }
    body
  }

  is_json_error_response <- function(res) {
    res$status >= 400 &&
      identical(res$headers[["Content-Type"]], "application/json")
  }

  function(data, req, res, value) {
    if (is.raw(res$body)) {
      size <- length(res$body)
    } else {
      size <- nchar(res$body)
    }

    logger$info(sprintf("response %s %s => %d (%d bytes)",
                        req$REQUEST_METHOD, req$PATH_INFO, res$status, size))

    if (is_json_error_response(res)) {
      logger_detailed(logger, "error", req, "error",
                      errors = jsonlite::parse_json(res$body)$errors)
    }

    logger_detailed(logger, "trace", req, "response",
                    body = safe_body(value$body))

    value
  }
}


logger_detailed <- function(logger, level, req, ...) {
  ## TODO: we might want to post the incoming body here

  ## TODO: getting a unique request id here would be ideal as then we
  ## could (fairly) easily associate the request and the response,
  ## though I think they'll always be served synchronously so that's
  ## not that much of a drama.

  ## the remote address/port are unlikely to be
  ## interesting as noone should be exposing these APIs
  ## to the internet at large.
  ## remote_addr = req$REMOTE_ADDR,
  ## remote_port = req$REMOTE_PORT,
  logger[[level]](...,
    method = req$REQUEST_METHOD,
    path = req$PATH_INFO,
    query = req$QUERY_STRING,
    headers = as.list(req$HEADERS))
}

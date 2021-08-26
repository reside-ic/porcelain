porcelain_log_preroute <- function(logger) {
  force(logger)
  function(data, req, res) {
    logger$info("request %s %s", req$REQUEST_METHOD, req$PATH_INFO)
    logger$trace("request",
                 ## the remote address/port are unlikely to be
                 ## interesting as noone should be exposing these APIs
                 ## to the internet at large.
                 ## remote_addr = req$REMOTE_ADDR,
                 ## remote_port = req$REMOTE_PORT,
                 method = req$REQUEST_METHOD,
                 path = req$PATH_INFO,
                 query = req$QUERY_STRING,
                 headers = as.list(req$HEADERS))
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

  function(data, req, res, value) {
    if (is.raw(res$body)) {
      size <- length(res$body)
    } else {
      size <- nchar(res$body)
    }
    if (res$status >= 400 &&
        identical(res$headers[["Content-Type"]], "application/json")) {
      dat <- jsonlite::parse_json(res$body)
      for (e in dat$errors) {
        if (!is.null(e$error)) {
          api_log(sprintf("error: %s", e$error))
          api_log(sprintf("error-detail: %s", e$detail))
          if (!is.null(e$trace)) {
            trace <- sub("\n", " ", vcapply(e$trace, identity))
            api_log(sprintf("error-trace: %s", trace))
          }
        }
      }
    }
    logger$info(sprintf("response %d (%d bytes)", res$status, size))
    logger$trace("response",
                 status = value$status,
                 headers = value$headers,
                 body = safe_body(value$body))
    value
  }
}

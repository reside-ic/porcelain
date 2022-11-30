##' Create a json-emitting logger, using the 'lgr' package.
##'
##' @title Create logger
##'
##' @param log_level The level of detail to log to. See
##'   [lgr::get_log_levels()] for possible values; this could be a
##'   string ("off", "info", "all", etc) or an integer level.
##'
##' @param name The name of the logger.  By default we use one derived
##'   from the package name, though this may not always be accurate.
##'
##' @param path Optionally, the path to log into.  If not given then
##'   we log to the console.
##'
##' @return A "Logger" object (see [lgr::Logger])
##' @export
##' @examples
##' logger <- porcelain::porcelain_logger(name = "example")
##' logger$log("info", "hello")
##' logger$log("trace", "silent")
porcelain_logger <- function(log_level = "info", name = NULL, path = NULL) {
  if (is.null(name)) {
    env <- parent.frame()
    name <- package_name(env)
  }

  logger <- lgr::get_logger(name, reset = TRUE)
  logger$set_propagate(FALSE)
  logger$set_threshold(log_level)
  if (is.null(path)) {
    appender <- lgr::AppenderConsole$new(layout = lgr::LayoutJson$new())
  } else {
    appender <- lgr::AppenderJson$new(path)
  }
  logger$add_appender(appender, name = "json")
  logger
}


## Plumber has stages preroute -> postroute -> preserialize -> postserialize
##
## We hook up the first bit of logging up against postroute as by that
## point we have run our filters, which include getting the body read
## and queries parsed. Without this it is not really possible to get
## the body as we can't portably read twice from the rook input (see
## warning in ?seek, which is used internally by rewind())
##
## The filter functions do minimal validation so there should be very
## few requests where logging is dropped here.
porcelain_log_postroute <- function(logger) {
  force(logger)
  function(data, req, res) {
    logger$info("request %s %s", req$REQUEST_METHOD, req$PATH_INFO,
                caller = "postroute")
    logger_detailed(logger, "trace", req, "postroute", "request")
  }
}


porcelain_log_postserialize <- function(logger) {
  force(logger)

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
    now <- now_utc()
    logger$info(sprintf("response %s %s => %d (%d bytes)",
                        req$REQUEST_METHOD, req$PATH_INFO, res$status, size),
                caller = "postserialize",
                endpoint = req$endpoint,
                request_received = req$received_time,
                elapsed_ms = format_difftime_ms(now, req$received_time),
                elapsed = format_difftime(now, req$received_time)
    )

    if (is_json_error_response(res)) {
      logger_detailed(logger, "error", req, "postserialize", "error",
                      errors = jsonlite::parse_json(res$body)$errors)
    }

    logger_detailed(logger, "trace", req, "postserialize", "response",
                    body = describe_body(value$body))

    value
  }
}


logger_detailed <- function(logger, level, req, caller, ...) {
  ## the remote address/port are unlikely to be
  ## interesting as noone should be exposing these APIs
  ## to the internet at large.
  ## remote_addr = req$REMOTE_ADDR,
  ## remote_port = req$REMOTE_PORT,
  logger[[level]](...,
    caller = caller,
    method = req$REQUEST_METHOD,
    path = req$PATH_INFO,
    endpoint = req$endpoint,
    query = req$porcelain_query,
    headers = as.list(req$HEADERS),
    body = describe_body(req$porcelain_body$value))
}


describe_body <- function(body) {
  if (is.raw(body)) {
    body <- sprintf("<binary body (%d bytes)>", length(body))
  }
  body
}

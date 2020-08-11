##' Throw an error from an endpoint.  This function is intended to
##' allow target functions to throw nice errors back through the API.
##'
##' @title Throw an error from an endpoint
##'
##' @param message The human-readable message of the error.  Ignored
##'   if \code{errors} is given.
##'
##' @param code Optional code for the error - if not given, then
##'   \code{ERROR} is used.  Ignored if \code{errors} is given.
##'
##' @param errors A named list of errors - use this to signal multiple
##'   error conditions as key/value pairs
##'
##' @param status_code The HTTP status code to use.  The default (400)
##'   means "bad request" which should be a reasonable catch-all for
##'   bad user data.
##'
##' @return Nothing, as this function throws an error
##' @export
pkgapi_stop <- function(message, code = "ERROR", errors = NULL,
                        status_code = 400L) {
  if (is.null(errors)) {
    assert_scalar_character(message)
    assert_scalar_character(code)
    errors <- set_names(list(message), code)
  }
  pkgapi_error(errors, status_code)
}


pkgapi_error <- function(errors, status_code = 400L) {
  stop(pkgapi_error_object(errors, status_code))
}


pkgapi_error_object <- function(errors, status_code) {
  data <- pkgapi_error_data(errors)
  message <- pkgapi_error_message(data)
  e <- list(message = message, data = data, status_code = status_code)
  class(e) <- c("pkgapi_error", "error", "condition")
  e
}


pkgapi_error_data <- function(errors) {
  assert_named(errors)
  error <- names(errors)
  detail <- unname(errors)
  if (!all(vlapply(detail, function(x) is.null(x) || is.character(x)))) {
    stop("All error details must be character or NULL", call. = FALSE)
  }
  Map(function(e, d) {
    if (!is.null(d)) {
      detail <- jsonlite::unbox(as.character(d))
    } else {
      detail <- NULL
    }
    ret <- list(error = jsonlite::unbox(e),
                detail = detail,
                key = jsonlite::unbox(ids::proquint(n_words = 3)))
    ret <- c(ret, attributes(d))
    },
    error, detail, USE.NAMES = FALSE)
}


pkgapi_error_message <- function(data) {
  error <- vcapply(data, "[[", "error")
  detail <- vcapply(data, function(x) x$detail %||% NA_character_)
  msg <- character(length(error))
  i <- is.na(detail)
  msg[i] <- sprintf("  * %s", error[i])
  msg[!i] <- sprintf("  * %s: %s", error[!i], detail[!i])
  paste0("pkgapi_error:\n", paste(msg, collapse = "\n"))
}


pkgapi_process_error <- function(error) {
  if (inherits(error, "pkgapi_validation_error")) {
    error_data <- pkgapi_error_data(c(VALIDATION_ERROR = error$message))
    status_code <- 500L
  } else if (inherits(error, "pkgapi_error")) {
    error_data <- error$data
    status_code <- error$status_code
  } else {
    error_data <- pkgapi_error_data(c(SERVER_ERROR = error$message))
    status_code <- 500L
  }

  value <- response_failure(error_data)
  content_type <- "application/json"
  body <- to_json_string(value)
  pkgapi_response(status_code, content_type, body,
                  error = error, value = value,
                  headers = NULL)
}

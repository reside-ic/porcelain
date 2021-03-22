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
##'   error conditions as key/value pairs.
##'
##' @param status_code The HTTP status code to use.  The default (400)
##'   means "bad request" which should be a reasonable catch-all for
##'   bad user data.
##'
##' @param ... Additional named args to be included as fields in the
##' error response JSON. The values must be in format ready for
##' serialization to JSON using [jsonlite::toJSON()] i.e. any unboxing
##' using [jsonlite::unbox()] needs to already have been done.
##'
##' @return Nothing, as this function throws an error
##' @export
porcelain_stop <- function(message, code = "ERROR", errors = NULL,
                           status_code = 400L, ...) {
  if (!is.null(errors)) {
    ## Convert from key - value pairs to key - list(detail = value)
    errors <- lapply(errors, function(error) list(detail = error))
  } else {
    assert_scalar_character(message)
    assert_scalar_character(code)
    extra <- list(...)


    content <- list(detail = message)
    extra <- list(...)
    if (length(extra) > 0) {
      assert_named(extra, name = "... args")
      content <- c(content, extra)
    }
    errors <- set_names(list(content), code)
  }
  porcelain_error(errors, status_code)
}


porcelain_error <- function(errors, status_code = 400L) {
  stop(porcelain_error_object(errors, status_code))
}


porcelain_error_object <- function(errors, status_code) {
  data <- porcelain_error_data(errors)
  message <- porcelain_error_message(data)
  e <- list(message = message, data = data, status_code = status_code)
  class(e) <- c("porcelain_error", "error", "condition")
  e
}


porcelain_error_data <- function(errors) {
  assert_named(errors)
  detail_valid <- vlapply(errors, function(error) {
    is.null(error$detail) || is.character(error$detail)
  })
  if (!all(detail_valid)) {
    stop("All error details must be character or NULL", call. = FALSE)
  }
  lapply(names(errors), function(error_name) {
    out <- append(list(error = jsonlite::unbox(error_name)),
                  as.list(errors[[error_name]]))
    out["detail"] <- list(jsonlite::unbox(out$detail))
    out
  })
}

porcelain_error_message <- function(data) {
  error <- vcapply(data, "[[", "error")
  detail <- vcapply(data, function(x) x$detail %||% NA_character_)
  msg <- character(length(error))
  i <- is.na(detail)
  msg[i] <- sprintf("  * %s", error[i])
  msg[!i] <- sprintf("  * %s: %s", error[!i], detail[!i])
  paste0("porcelain_error:\n", paste(msg, collapse = "\n"))
}


porcelain_process_error <- function(error) {
  if (inherits(error, "porcelain_validation_error")) {
    error_data <- porcelain_error_data(list(VALIDATION_ERROR = list(
                                              detail = error$message)))
    status_code <- 500L
  } else if (inherits(error, "porcelain_error")) {
    error_data <- error$data
    status_code <- error$status_code
  } else {
    error_data <- porcelain_error_data(list(SERVER_ERROR = list(
                                              detail = error$message)))
    status_code <- 500L
  }

  value <- response_failure(error_data)
  content_type <- "application/json"
  body <- to_json_string(value)
  porcelain_response(status_code, content_type, body,
                     error = error, value = value,
                     headers = NULL, validated = FALSE)
}

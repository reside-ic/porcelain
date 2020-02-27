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
  Map(function(e, d)
    list(error = jsonlite::unbox(e), detail = jsonlite::unbox(d)),
    error, detail, USE.NAMES = FALSE)
}


pkgapi_error_message <- function(data) {
  error <- vcapply(data, "[[", "error")
  detail <- vcapply(data, "[[", "error")
  msg <- character(length(error))
  i <- is.na(detail)
  msg[i] <- sprintf("  * %s", error[i])
  msg[!i] <- sprintf("  * %s: %s", error[!i], detail[!i])
  paste0("pkgapi_error:\n", paste(msg, collapse = "\n"))
}


pkgapi_process_error <- function(error, validate = FALSE) {
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
  ## pkgapi_validate(body, pkgapi_validator_error, validate)
  pkgapi_response(status_code, content_type, body,
                  error = error, value = value)
}

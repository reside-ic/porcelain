## This one is designed to be user throwable.  The user provides either
##
## pkgapi_error("error") # not implemented
## pkgapi_error(c(error = "detail"))
## pkgapi_error(c(error1 = "detail1", error2 = "detail2"))
##
## It's a bit weird because when throwing the error we need to agree
## what type of error we're going to throw but I think this will work
## ok.
pkgapi_error <- function(errors, status_code = 400L) {
  stopifnot(!is.null(names(errors)), is.character(errors)) # TODO
  error <- names(errors)
  detail <- unname(errors)
  data <- Map(function(e, d)
    list(error = jsonlite::unbox(e), detail = jsonlite::unbox(d)),
    error, detail, USE.NAMES = FALSE)
  msg <- character(length(error))
  i <- is.na(detail)
  msg[i] <- sprintf("  * %s", error[i])
  msg[!i] <- sprintf("  * %s: %s", error[!i], detail[!i])
  message <- paste0("pkgapi_error:\n", paste(msg, collapse = "\n"))

  e <- list(message = message, data = data, status_code = status_code)
  class(e) <- c("pkgapi_error", "error", "condition")
  stop(e)
}


## Then this is what we register in the plumber object to catch errors
## and process them for us.  It will handle both our errors (with
## class 'pkgapi_error') and uncaught errors (with class 'error').
pkgapi_error_handler <- function(req, res, error) {
  pkgapi_process_error(error)
}


pkgapi_process_error <- function(error) {
  if (!inherits(error, "pkgapi_error")) {
    error <- pkgapi_error(c(SERVER_ERROR = error$message), 500L)
  }
  value <- response_failure(error$data)
  ret <- list(errors = error,
              value = value,
              body = to_json(value),
              content_type = "application/json",
              status_code = error$status_code)
  ## pkgapi_validate(ret, validator, validate)
  ret
}


response_failure <- function(errors) {
  list(status = jsonlite::unbox("failure"), errors = errors, data = NULL)
}


## TODO: do we want to capture the general error here or leave it
## to an error handler on the main object?  It seems that
## returning well defined code here is worthwhile.
##
## TODO: this is not quite right because we should skip validation
## too
##
## TODO: this should be done perhaps at the level *above* (another
## wrapper) because otherwise we duplicate it below.
pkgapi_catch <- function(...) {
  ret <- tryCatch({
    data <- handler(...)
    value <- response_success(data)
    json <- to_json(value)
    list(success = TRUE, data = data, value = value, json = json,
         content_type = "")
  }, pkgapi_error = function(e) {
    data <- e
    value <- response_failure(e)
    json <- to_json(value)
    list(success = TRUE, data = data, value = value, json = json)
  })
}

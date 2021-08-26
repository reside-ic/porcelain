## To get a nice generic logger, what do we need to add?
##
## We need to control things like:
##
## * Where is it going? (we can do a lot with lgr to help here)
## * What information are we capturing?


## The simplest thing, we should be able to do:

## pr$log(logger)

## for some object 'logger' and have logging enabled, plus re-arrange
## any existing hooks.

## So for now, write a non-generic one and we'll make it generic later.

porcelain_logger_basic <- R6::R6Class(
  "porcelain_logger",

  public = list(
    initialize = function() {
    },

    log = function(msg) {
      message(paste(sprintf("[%s] %s", Sys.time(), msg), collapse = "\n"))
    }

    begin = function(data, req, res) {
      self$log(sprintf("%s %s", req$REQUEST_METHOD, req$PATH_INFO))
    },

    end = function(data, req, res, value) {
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
      self$log(sprintf("`--> %d (%d bytes)", res$status, size))
    }
  ))


logger_safe <- function(fn) {
  force(fn)
  function(data, req, res, value) {
    fn(data, req, res, value)
    value
  }
}

## TODO: required with current plumber?
options(plumber.debug = FALSE)

get_error <- function(expr) {
  tryCatch(expr, error = identity)
}

##' @importFrom R6 R6Class
##' @importFrom plumber plumber
NULL


cache <- new.env()

.onLoad <- function(...) {
  cache$plumber_1_0_0 <- utils::packageVersion("plumber") >= "0.9.9"
}


## Compatibility:
plumber_base_class <- function() {
  get(plumber_base_class_name(), asNamespace("plumber"))
}


plumber_base_class_name <- function() {
  if (cache$plumber_1_0_0) "Plumber" else "plumber"
}


plumber_path_args <- function(req) {
  if (cache$plumber_1_0_0) {
    req$argsPath
  } else {
    req$args[seq_len(length(req$args) - 2L)]
  }
}

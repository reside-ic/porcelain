`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


system_file <- function(...) {
  system.file(..., mustWork = TRUE)
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


## This should probably be tuneable?
to_json <- function(x) {
  jsonlite::toJSON(x, json_verbatim = TRUE, na = "null", null = "null")
}


to_json_string <- function(x) {
  as.character(to_json(x))
}


is_directory <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

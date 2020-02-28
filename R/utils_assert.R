assert_named <- function(x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
}


assert_is <- function(x, what, name = deparse(substitute(x))) {
  if (!inherits(x, what)) {
    stop(sprintf("'%s' must be a %s", name,
                 paste(what, collapse = " / ")), call. = FALSE)
  }
}


assert_file_exists <- function(x, name = "File") {
  if (!file.exists(x)) {
    stop(sprintf("%s does not exist: %s", name, x), call. = FALSE)
  }
}


assert_is_directory <- function(x, name = "File") {
  assert_file_exists(x, name)
  if (!is_directory(x)) {
    stop(sprintf("%s exists but is not a directory: %s", name, x),
         call. = FALSE)
  }
}


assert_raw <- function(x, name = deparse(substitute(x))) {
  if (!is.raw(x)) {
    stop(sprintf("'%s' must be a raw vector", name), call. = FALSE)
  }
}

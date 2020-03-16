## Validation functions used to deal with inputs
pkgapi_input_validate_logical <- function(x) {
  assert_scalar(x)
  res <- as.logical(x)
  if (is.na(res)) {
    stop(sprintf("Could not convert '%s' into a logical", x))
  }
  res
}


pkgapi_input_validate_integer <- function(x) {
  assert_scalar(x)
  res <- suppressWarnings(as.integer(x))
  if (is.na(res)) {
    stop(sprintf("Could not convert '%s' into an integer", x))
  }
  if (abs(as.numeric(x) - res) > 1e-8) {
    stop(sprintf("Could not convert '%s' into an integer (loses precision)",
                 x))
  }
  res
}


pkgapi_input_validate_numeric <- function(x) {
  assert_scalar(x)
  res <- suppressWarnings(as.numeric(x))
  if (is.na(res)) {
    stop(sprintf("Could not convert '%s' into a numeric", x))
  }
  res
}


pkgapi_input_validate_string <- function(x) {
  ## This will always come in as a string.
  assert_scalar(x)
  x
}


pkgapi_input_validate_basic <- function(type) {
  switch(type,
         logical = pkgapi_input_validate_logical,
         integer = pkgapi_input_validate_integer,
         numeric = pkgapi_input_validate_numeric,
         string  = pkgapi_input_validate_string)
}


pkgapi_input_validate_mime <- function(given, expected) {
  if (is.null(given)) {
    pkgapi_input_error(sprintf(
      "Content-Type was not set (expected '%s')", expected))
  }
  if (given != expected) {
    pkgapi_input_error(sprintf(
      "Expected content type '%s' but was sent '%s'", expected, given))
  }
}


pkgapi_input_validate_expected <- function(given, expected) {
  ## No point checking for additional path paramters; they are not
  ## possible.  Additional headers and cookies will be ignored.
  extra <- setdiff(names(given$query), expected$query)
  if (length(extra) > 0L) {
    pkgapi_input_error(sprintf(
      "Unconsumed query paramter: %s", paste(squote(extra), collapse = ", ")))
  }

  if (isTRUE(given$body$provided) && is.null(expected$body)) {
    pkgapi_input_error(
      "This endpoint does not accept a body, but one was provided")
  }
}

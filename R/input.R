##' Control for query parameters.
##'
##' @title Control for query parameters
##'
##' @param ... Named arguments representing accepted parameters.  The
##'   value of each must be a type.
##'
##' @param .parameters A list of named parameters to accept, instead
##'   of using \code{...} - this interface is considerably easier to
##'   program against if building an API programmatically, avoiding
##'   the use of \code{\link{do.call}}.
##'
##' @export
##'
##' @examples
##' pkgapi::pkgapi_input_query(number = "integer")
pkgapi_input_query <- function(..., .parameters = list(...)) {
  assert_named(.parameters, TRUE)
  nms <- names(.parameters)

  types <- c("logical", "numeric", "integer", "string")

  for (i in seq_along(.parameters)) {
    match_value(.parameters[[i]], types,
                sprintf("The 'type' of parameter %s", nms[[i]]))
  }
  types <- vcapply(.parameters, identity, USE.NAMES = FALSE)
  unname(Map(pkgapi_input, nms, types, MoreArgs = list(where = "query")))
}


pkgapi_input_path <- function(path) {
  data <- parse_plumber_path(path)
  if (is.null(data)) {
    return(NULL)
  }
  apply(parse_plumber_path(path), 1, function(x)
    pkgapi_input(x[[1]], x[[2]], "path"))
}


pkgapi_input <- function(name, type, where, validator = NULL) {
  res <- list(name = name, type = type, where = where)
  if (is.null(validator)) {
    res$validator <- pkgapi_input_validator_basic(type)
  }
  class(res) <- "pkgapi_input"
  res
}


pkgapi_inputs_init <- function(path, inputs_query, args) {
  inputs_path <- pkgapi_input_path(path)
  validate_path <- pkgapi_input_validator_simple(inputs_path, args)

  validate_query <- pkgapi_input_validator_simple(inputs_query, args)

  inputs <- c(inputs_path, inputs_query)
  nms <- vcapply(inputs, "[[", "name")
  if (anyDuplicated(nms)) {
    message("fix duplicated")
    browser()
  }

  function(path, query) {
    c(validate_path(path),
      validate_query(query))
  }
}


pkgapi_input_init <- function(input, args) {
  assert_is(input, "pkgapi_input")
  name <- input$name

  if (!(name %in% names(args))) {
    stop(sprintf(
      "Argument '%s' (used in %s) missing from the target function",
      name, input$where))
  }
  default <- args[[name]]
  input$required <- missing(default)

  input
}


pkgapi_input_validator_logical <- function(x) {
  assert_scalar(x)
  res <- as.logical(x)
  if (is.na(res)) {
    stop(sprintf("Could not convert '%s' into a logical", x))
  }
  res
}


pkgapi_input_validator_integer <- function(x) {
  assert_scalar(x)
  res <- suppressWarnings(as.integer(x))
  if (is.na(res)) {
    stop(sprintf("Could not convert '%s' into an integer", x))
  }
  res
}


pkgapi_input_validator_numeric <- function(x) {
  assert_scalar(x)
  res <- suppressWarnings(as.numeric(x))
  if (is.na(res)) {
    stop(sprintf("Could not convert '%s' into a numeric", x))
  }
  res
}


pkgapi_input_validator_string <- function(x) {
  assert_scalar(x)
  res <- suppressWarnings(as.character(x))
  if (is.na(res)) {
    stop(sprintf("Could not convert '%s' into a string", x))
  }
  res
}


pkgapi_input_validator_basic <- function(type) {
  switch(type,
         logical = pkgapi_input_validator_logical,
         integer = pkgapi_input_validator_integer,
         numeric = pkgapi_input_validator_numeric,
         string  = pkgapi_input_validator_string)
}


pkgapi_input_validator_simple <- function(inputs, args) {
  inputs <- lapply(inputs, pkgapi_input_init, args)
  nms <- vcapply(inputs, "[[", "name")

  throw <- function(msg, ...) {
    pkgapi_error(list(INVALID_QUERY = sprintf(msg, ...)))
  }

  function(query) {
    for (i in inputs) {
      value <- query[[i$name]]
      if (i$required || !is.null(value)) {
        query[[i$name]] <- tryCatch(
          i$validator(value),
          error = function(e)
            throw("Error parsing %s parameter '%s': %s",
                  i$name, i$where, e$message))
      }
    }

    extra <- setdiff(names(query), nms)
    if (length(extra) > 0L) {
      message("Fix message")
      browser()
      throw("Recieved extra query parameters: %s", paste(squote(extra)))
    }

    query
  }
}

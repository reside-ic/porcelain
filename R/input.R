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


##' Control for body parameters.  This might change.  There are
##' several types of HTTP bodies that we want to consider here - the
##' primary ones are a body uploaded in binary, the other is a json
##' object.  In the latter we want to validate the body against a
##' schema (at least if validation is used).  In future we might also
##' support a form input here too.
##'
##' @title Control for body parameters
##'
##' @param name Name of the parameter
##'
##' @export
##' @rdname pkgapi_input_body
pkgapi_input_body_binary <- function(name) {
  assert_scalar_character(name)
  pkgapi_input(name, "binary", "body", assert_raw,
               content_type = "application/octet-stream")
}


##' @inheritParams pkgapi_returning_json
##' @export
##' @rdname pkgapi_input_body
pkgapi_input_body_json <- function(name, schema, root) {
  assert_scalar_character(name)
  validator <- pkgapi_validator(schema, schema_root(root), query = NULL)
  pkgapi_input(name, "json", "body", validator,
               content_type = "application/json")
}


pkgapi_input_path <- function(path) {
  data <- parse_plumber_path(path)
  if (is.null(data)) {
    return(NULL)
  }
  apply(parse_plumber_path(path), 1, function(x)
    pkgapi_input(x[[1]], x[[2]], "path"))
}


pkgapi_input <- function(name, type, where, validator = NULL, ...) {
  res <- list(name = name, type = type, where = where, validator = validator,
              ...)
  if (is.null(validator)) {
    res$validator <- pkgapi_input_validator_basic(type)
  }
  class(res) <- "pkgapi_input"
  res
}


## TODO: duplicate passed query parameters not supported yet (are allowed)
pkgapi_inputs_init <- function(path, inputs_query, inputs_body, args) {
  inputs_path <- pkgapi_input_path(path)
  validate_path <- pkgapi_input_validator_simple(inputs_path, args, "path")
  validate_query <- pkgapi_input_validator_simple(inputs_query, args, "query")
  validate_body <- pkgapi_input_validator_body(inputs_body, args)

  inputs <- c(inputs_path, inputs_query) # TODO: inputs_body not here yet
  nms <- vcapply(inputs, "[[", "name")
  if (anyDuplicated(nms)) {
    message("fix duplicated")
    browser()
  }

  function(path, query, body) {
    c(validate_path(path),
      validate_query(query),
      validate_body(body))
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


pkgapi_input_validator_simple <- function(inputs, args, where) {
  inputs <- lapply(inputs, pkgapi_input_init, args)
  nms <- vcapply(inputs, "[[", "name")

  throw <- function(msg, ...) {
    pkgapi_error(list(INVALID_INPUT = sprintf(msg, ...)))
  }

  function(given) {
    for (i in inputs) {
      value <- given[[i$name]]
      if (i$required || !is.null(value)) {
        given[[i$name]] <- tryCatch(
          i$validator(value),
          error = function(e)
            throw("Error parsing %s parameter '%s': %s",
                  i$where, i$name, e$message))
      }
    }

    extra <- setdiff(names(given), nms)
    if (length(extra) > 0L) {
      throw("Recieved extra %s parameters: %s",
            where, paste(squote(extra), collapse = ", "))
    }

    given
  }
}


pkgapi_input_validator_body <- function(body, args) {
  if (is.null(body)) {
    ## TODO: Probably we should throw if a body *is* provided...
    return(function(body, content_type) NULL)
  }

  ## This mostly does the checking against the target function to make
  ## sure that body is being routed somewhere sensible.
  input <- pkgapi_input_init(body, args)

  throw <- function(msg, ...) {
    pkgapi_error(list(INVALID_INPUT = sprintf(msg, ...)))
  }

  name <- input$name
  required <- input$required
  validator_content <- input$validator
  content_type <- parse_mime(input$content_type)

  function(body) {
    if (required && is.null(body)) { # TODO: or length zero?
      throw("Body was not provided")
    }
    if (body$type$mime != content_type$mime) {
      throw("Expected content type '%s' but was sent '%s'",
            content_type$mime, body$type$mime)
    }
    tryCatch(
      validator_content(body$value),
      error = function(e)
        throw("Invalid body provided: %s", e$message))
    set_names(list(body$value), name)
  }
}

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
  unname(Map(pkgapi_input$new, nms, types, MoreArgs = list(where = "query")))
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
  pkgapi_input$new(name, "binary", "body", assert_raw,
                   content_type = "application/octet-stream")
}


##' @inheritParams pkgapi_returning_json
##' @export
##' @rdname pkgapi_input_body
pkgapi_input_body_json <- function(name, schema, root) {
  assert_scalar_character(name)
  validator <- pkgapi_validator(schema, schema_root(root), query = NULL)
  pkgapi_input$new(name, "json", "body", validator,
                   content_type = "application/json")
}


pkgapi_input_path <- function(path) {
  data <- parse_plumber_path(path)
  if (is.null(data)) {
    return(NULL)
  }
  apply(parse_plumber_path(path), 1, function(x)
    pkgapi_input$new(x[[1]], x[[2]], "path"))
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
  if (abs(as.numeric(x) - res) > 1e-8) {
    stop(sprintf("Could not convert '%s' into an integer (loses precision)",
                 x))
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
  ## This will always come in as a string.
  assert_scalar(x)
  x
}


pkgapi_input_validator_basic <- function(type) {
  switch(type,
         logical = pkgapi_input_validator_logical,
         integer = pkgapi_input_validator_integer,
         numeric = pkgapi_input_validator_numeric,
         string  = pkgapi_input_validator_string)
}


pkgapi_input_validator_simple <- function(inputs, args, where) {
  for (i in inputs) {
    i$check_target_args(args)
  }
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
  throw <- function(msg, ...) {
    pkgapi_error(list(INVALID_INPUT = sprintf(msg, ...)))
  }

  if (is.null(body)) {
    return(function(body) {
      if (body$provided) {
        throw("This endpoint does not accept a body, but one was provided")
      }
    })
  }

  ## This mostly does the checking against the target function to make
  ## sure that body is being routed somewhere sensible.
  input <- body$check_target_args(args)

  name <- input$name
  required <- input$required
  validator_content <- input$validator
  ## TODO: this is ugly, and should be dealt with some other way.
  content_type <- parse_mime(input$data$content_type)

  function(body) {
    if (!body$provided) {
      if (required) {
        throw("Body was not provided")
      } else {
        return(NULL)
      }
    }
    if (is.null(body$type$mime)) {
      throw("Content-Type was not set (expected '%s')", content_type$mime)
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


input_required_args <- function(args) {
  required <- logical(length(args))
  for (i in seq_along(args)) {
    x <- args[[i]]
    required[[i]] <- missing(x)
  }
  names(args)[required]
}


## Classes that hold the required data for input validation.
pkgapi_inputs <- R6::R6Class(
  "pkgapi_inputs",

  private = list(
    process_path = NULL,
    process_query = NULL,
    process_body = NULL
  ),

  public = list(
    initialize = function(path, inputs_query, inputs_body, args) {

      inputs_path <- pkgapi_input_path(path)
      private$process_path <-
        pkgapi_input_validator_simple(inputs_path, args, "path")
      private$process_query <-
        pkgapi_input_validator_simple(inputs_query, args, "query")
      private$process_body <- pkgapi_input_validator_body(inputs_body, args)

      inputs <- c(inputs_path, inputs_query)
      if (!is.null(inputs_body)) {
        inputs <- c(inputs, list(inputs_body))
      }
      nms <- vcapply(inputs, "[[", "name")
      if (anyDuplicated(nms)) {
        i <- nms %in% unique(nms[duplicated(nms)])
        err <- sort(vcapply(inputs[i], function(x)
          sprintf("'%s' (in %s)", x$name, x$where)))
        stop("Duplicated parameter names: ", paste(err, collapse = ", "),
             call. = FALSE)
      }

      msg <- setdiff(input_required_args(args), nms)
      if (length(msg) > 0L) {
        stop("Required arguments to target function missing from inputs: ",
             paste(squote(msg), collapse = ", "),
             call. = FALSE)
      }
    },

    process = function(path, query, body) {
      c(private$process_path(path),
        private$process_query(query),
        private$process_body(body))
    }
  ))


pkgapi_input <- R6::R6Class(
  "pkgapi_input",

  public = list(
    name = NULL,
    type = NULL,
    where = NULL,
    validator = NULL,
    required = NULL,
    data = NULL,

    initialize = function(name, type, where, validator = NULL, ...) {
      self$name <- name
      self$type <- type
      self$where <- where
      self$validator <- validator %||% pkgapi_input_validator_basic(type)
      self$data <- list(...)
    },

    check_target_args = function(args) {
      if (!(self$name %in% names(args))) {
        stop(sprintf(
          "Argument '%s' (used in %s) missing from the target function",
          self$name, self$where))
      }
      default <- args[[self$name]]
      self$required <- missing(default)
      invisible(self)
    }
  ))

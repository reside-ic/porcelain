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
  pkgapi_input_collection$new(names(.parameters), .parameters, "query")
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


## This one gets called internally
pkgapi_input_path <- function(path) {
  parts <- parse_path_parameters(path)
  if (is.null(parts)) {
    return(NULL)
  }
  pkgapi_input_collection$new(parts[, "name"], parts[, "type"], "path")
}


## TODO: I think that content_type and schema probably end up
## throughout this class, not just within the 'data' element, we'll
## follow the swagger spec for what do do here.  I think that the
## general approach is to have a "format" field that implies the
## content type.  This can wait until later.

pkgapi_input <- R6::R6Class(
  "pkgapi_input",

  public = list(
    name = NULL,
    type = NULL,
    where = NULL,
    validator = NULL, # make this private, use a method for access?
    required = NULL,
    default = NULL,
    data = NULL,

    initialize = function(name, type, where, validator = NULL, ...) {
      assert_scalar_character(name)
      assert_scalar_character(type)
      assert_scalar_character(where)
      if (is.null(validator)) {
        validator <- pkgapi_input_validate_basic(type)
      } else {
        assert_is(validator, "function")
      }

      self$name <- name
      self$type <- type
      self$where <- where
      self$validator <- validator

      if (where == "query") {
        types <- c("logical", "numeric", "integer", "string")
        match_value(type, types,
                    sprintf("The 'type' of query parameter %s", nms[[i]]))
      }

      self$data <- list(...)
    },

    bind = function(target) {
      args <- formals(target)
      if (!(self$name %in% names(args))) {
        stop(sprintf(
          "Argument '%s' (used in %s) missing from the target function",
          self$name, self$where))
      }
      default <- args[[self$name]]
      self$required <- missing(default)
      if (!self$required) {
        ## TODO: might need to force a promise here?
        self$default <- default
      }
      invisible(self)
    },

    ## TODO: this is pretty ugly for body; should this move around the
    ## validator or is that just weird?  Might be nicer, can adjust
    ## later
    ##
    ## NOTE: this is basically two functions - pull out into two free fns
    validate = function(given) {
      if (self$where == "body") {
        body <- given[["body"]]
        if (self$required && !isTRUE(body$provided)) {
          pkgapi_input_error("Body was not provided")
        }
        if (isTRUE(body$provided)) {
          pkgapi_input_validate_mime(body$type$mime, self$data$content_type)
          value <- body$value
        } else {
          value <- NULL
        }
      } else {
        value <- given[[self$where]][[self$name]]
        if (self$required && is.null(value)) {
          pkgapi_input_error(sprintf(
            "%s parameter '%s' is missing but required",
            self$where, self$name))
        }
      }
      if (!is.null(value)) {
        value <- tryCatch(
          self$validator(value),
          error = function(e)
            ## NOTE: not a lovely error message for the body
            pkgapi_input_error(sprintf("Error parsing %s parameter '%s': %s",
                                  self$where, self$name, e$message)))
      }
      value
    }
  ))


## This one is just to shepherd things through for now - could be an
## S3 class I think, but we'll probably pop a print method on this at
## some point, and R6 makes that easy
pkgapi_input_collection <- R6::R6Class(
  "pkgapi_input_collection",
  public = list(
    inputs = NULL,
    initialize = function(names, types, where) {
      self$inputs <- unname(Map(pkgapi_input$new, names, types,
                                MoreArgs = list(where = where)))
    }))


pkgapi_inputs <- R6::R6Class(
  "pkgapi_inputs",

  private = list(
    expected = NULL
  ),

  public = list(
    inputs = NULL,

    initialize = function(inputs) {
      ## This is a bit ugly, but flattens out the collections:
      self$inputs <- unlist(recursive = FALSE, lapply(inputs, function(x)
        if (inherits(x, "pkgapi_input_collection")) x$inputs else list(x)))

      expected <- vapply(self$inputs, function(x) c(x$where, x$name),
                         character(2), USE.NAMES = FALSE)
      private$expected <- split(expected[2, ], expected[1, ])

      nms <- vcapply(self$inputs, "[[", "name")
      if (anyDuplicated(nms)) {
        i <- nms %in% unique(nms[duplicated(nms)])
        err <- sort(vcapply(self$inputs[i], function(x)
          sprintf("'%s' (in %s)", x$name, x$where)))
        stop("Duplicated parameter names: ", paste(err, collapse = ", "),
             call. = FALSE)
      }
    },

    bind = function(target) {
      for (i in self$inputs) {
        i$bind(target)
      }

      nms <- vcapply(self$inputs, "[[", "name")
      msg <- setdiff(formals_required(target), nms)
      if (length(msg) > 0L) {
        stop("Required arguments to target function missing from inputs: ",
             paste(squote(msg), collapse = ", "),
             call. = FALSE)
      }

      invisible(self)
    },

    validate = function(given) {
      ret <- vector("list", length(self$inputs))
      names(ret) <- vcapply(self$inputs, "[[", "name")
      for (i in self$inputs) {
        ret[[i$name]] <- i$validate(given)
      }

      ## Validate all are expected:
      pkgapi_input_validate_expected(given, private$expected)

      ret
    }
  ))


pkgapi_input_error <- function(msg) {
  pkgapi_error(list(INVALID_INPUT = msg))
}

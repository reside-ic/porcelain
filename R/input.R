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
  data <- parse_plumber_path(path)
  if (is.null(data)) {
    return(NULL)
  }
  pkgapi_input_collection$new(data[, "name"], data[, "type"], "path")
}

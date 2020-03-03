##' Support for describing and controlling expected return types.  The
##' high-level functions (\code{pkgapi_returning_json} and
##' \code{pkgapi_returning_binary}) should be generally used.
##'
##' @title Support for endpoint return types
##'
##' @param content_type The MIME content type for the endpoint,
##'   e.g. \code{text/plain}, \code{application/json}.
##'
##' @param process A processing function that will convert the output
##'   of the handler function into something of the type
##'   \code{content_type}.  This should be independent of arguments
##'   passed to the endpoint, so practically this is the final stage
##'   of serialisation.
##'
##' @param validate A function that validates the return value and
##'   throws an error if the output is not expected.  This will only
##'   be used if the endpoint is created with \code{validate = TRUE}.
##'
##' @param status_code The HTTP status code that the endpoint will use
##'   on a successful return.  The default of 200 should be
##'   reasonable.
##'
##' @export
pkgapi_returning <- function(content_type, process, validate,
                             status_code = 200L) {
  ## These should be validated, but that should wait until the api
  ## stabilises.
  ret <- list(content_type = content_type,
              status_code = status_code,
              process = process,
              validate = validate)
  class(ret) <- "pkgapi_returning"
  ret
}


##' @param schema The name of the json schema to use
##'
##' @param root The root of the schema directory.  If not provided,
##'   and if \code{target} is in a package, then we'll look in that
##'   package's installed \code{schema} directory.
##'
##' @export
##' @rdname pkgapi_returning
pkgapi_returning_json <- function(schema = NULL, root = NULL,
                                  status_code = 200L) {
  content_type <- "application/json"
  process <- function(data) to_json_string(response_success(data))
  validate <- pkgapi_validator(schema, schema_root(root), query = "data")
  pkgapi_returning(content_type, process, validate, status_code)
}


##' @export
##' @rdname pkgapi_returning
pkgapi_returning_binary <- function(status_code = 200L) {
  content_type <- "application/octet-stream"
  validate <- assert_raw
  process <- identity
  pkgapi_returning(content_type, process, validate, status_code)
}

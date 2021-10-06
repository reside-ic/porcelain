#' @porcelain
#'   GET / => json("numeric")
#'   query a :: numeric
#'   query b :: numeric
add <- function(a, b) {
  jsonlite::unbox(a + b)
}

api <- function(validate = FALSE) {
  api <- porcelain::porcelain$new(validate = validate)
  api$handle_package()
  api
}

#' @porcelain
#'   POST /path => json
#'   body data :: binary
f <- function(data) {}

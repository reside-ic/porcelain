##' @porcelain GET /add/<a:int>/<b:int> => json("numeric")
add <- function(a, b) {
  jsonlite::unbox(a + b)
}

api <- function(validate = FALSE) {
  api <- porcelain::porcelain$new(validate = validate)
  api
}

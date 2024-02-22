add <- function(a, b) {
  jsonlite::unbox(a + b)
}

endpoint_add <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/", add,
    porcelain::porcelain_input_query(a = "numeric", b = "numeric"),
    returning = porcelain::porcelain_returning_json("numeric"))
}

api <- function(validate = FALSE) {
  api <- porcelain::porcelain$new(validate = validate)
  api$handle(endpoint_add())
  api
}

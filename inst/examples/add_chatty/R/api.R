add <- function(a, b) {
  jsonlite::unbox(a + b)
}

endpoint_add <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/", add,
    porcelain::porcelain_input_query(a = "numeric", b = "numeric"),
    returning = porcelain::porcelain_returning_json("numeric"))
}

api <- function(validate = FALSE, log_path = tempfile()) {
  logger <- porcelain::porcelain_logger("info", path = log_path)
  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$handle(endpoint_add())
  api
}

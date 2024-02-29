add <- function(a, b) {
  logger <- lgr::get_logger("add.chatty")
  logger$info("Adding numbers :D")
  jsonlite::unbox(a + b)
}

endpoint_add <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/", add,
    porcelain::porcelain_input_query(a = "numeric", b = "numeric"),
    returning = porcelain::porcelain_returning_json("numeric"))
}

api <- function(validate = FALSE) {
  logger <- porcelain::porcelain_logger("info")
  api <- porcelain::porcelain$new(validate = validate, logger = logger)
  api$handle(endpoint_add())
  api
}

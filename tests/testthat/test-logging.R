test_that("Can log", {
  skip_if_not_installed("lgr")
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  logger <- lgr::get_logger("porcelain/tests/can-log")
  logger$set_propagate(FALSE)
  logger$add_appender(lgr::AppenderJson$new(tmp), name = "json")
  logger$set_threshold("all")

  pr <- porcelain$new(logger = logger)
  pr$handle(endpoint)
  pr$request("GET", "/")

  log <- lapply(readLines(tmp), jsonlite::fromJSON)
  expect_length(log, 4)
})

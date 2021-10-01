test_that("Can parse basic endpoint", {
  expect_equal(
    roxy_parse_string("GET / => json"),
    list(method = "GET", path = "/", inputs = NULL, returning = list("json")))
})


test_that("Parse return type", {
  expect_equal(roxy_parse_returning("json"), list("json"))
  expect_equal(roxy_parse_returning("json()"), list("json"))
  expect_equal(roxy_parse_returning("json(schema)"), list("json", "schema"))
  expect_equal(roxy_parse_returning("json('schema')"), list("json", "schema"))
  expect_equal(roxy_parse_returning("json(schema, status_code = 200)"),
               list("json", "schema", status_code = 200))
})

test_that("Accept inputs", {
  expect_equal(
    roxy_parse_string("GET / => json\nquery x :: int"),
    list(method = "GET", path = "/",
         inputs = list(query = c(x = "int")),
         returning = list("json")))
  expect_equal(
    roxy_parse_string("GET / => json\nquery x :: int\nquery y :: double"),
    list(method = "GET", path = "/",
         inputs = list(query = c(x = "int", y = "double")),
         returning = list("json")))
  expect_equal(
    roxy_parse_string("POST /path => json\nquery x :: int\nbody arg :: json"),
    list(method = "POST", path = "/path",
         inputs = list(query = c(x = "int"),
                       body = c(arg = "json")),
         returning = list("json")))
})

test_that("Parse from roxygen block", {
  text <- paste(
    "#' @porcelain",
    "#'   GET / =>",
    "#'     json(schema)",
    "f <- function() {}",
    sep = "\n")
  block <- roxygen2::parse_text(text)
  expect_equal(
    block[[1]]$tags[[1]]$val,
    list(method = "GET", path = "/", inputs = NULL,
         returning = list("json", "schema")))
})

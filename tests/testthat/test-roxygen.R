test_that("Can parse basic endpoint", {
  expect_mapequal(
    roxy_parse_string("GET / => json", "text", 1),
    list(method = "GET",
         path = "/",
         inputs = NULL,
         returning = list("json")))
})


test_that("Parse return type", {
  expect_equal(roxy_parse_returning("json"), list("json"))
  expect_equal(roxy_parse_returning("json()"), list("json"))
  expect_equal(roxy_parse_returning("json(schema)"), list("json", "schema"))
  expect_equal(roxy_parse_returning("json('schema')"), list("json", "schema"))
  expect_equal(roxy_parse_returning("json(schema, status_code = 200)"),
               list("json", "schema", status_code = 200))
  expect_error(
    roxy_parse_returning("json(schema", "myfile", 100),
    "While processing @porcelain returning argument (myfile:100)",
    fixed = TRUE)
})


test_that("Accept inputs", {
  expect_mapequal(
    roxy_parse_string("GET / => json\nquery x :: int", "<text>", 1),
    list(method = "GET",
         path = "/",
         inputs = list(query = list(x = list("int"))),
         returning = list("json")))
  expect_mapequal(
    roxy_parse_string("GET / => json\nquery x :: int\nquery y :: double",
                      "<text>", 1),
    list(method = "GET",
         path = "/",
         inputs = list(query = list(x = list("int"), y = list("double"))),
         returning = list("json")))
  expect_mapequal(
    roxy_parse_string("POST /path => json\nquery x :: int\nbody arg :: json",
                      "<text>", 1L),
    list(method = "POST", path = "/path",
         inputs = list(query = list(x = list("int")),
                       body = list(arg = list("json"))),
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
  expect_mapequal(
    block[[1]]$tags[[1]]$val,
    list(method = "GET", path = "/", inputs = NULL,
         returning = list("json", "schema")))
})


test_that("Nice error on parse failure", {
  text <- paste(
    "#' @porcelain",
    "#'   GET / ->",
    "#'     json(schema)",
    "f <- function() {}",
    sep = "\n")
  err <- expect_error(
    roxygen2::parse_text(text))
  expect_match(err$message,
               "error occured at <text>:1-3", fixed = TRUE)
})


test_that("Nice error on input parse failure", {
  text <- paste(
    "#' @porcelain",
    "#'   GET / =>",
    "#'     json(schema)",
    "#' query x :: int",
    "#' other",
    "f <- function() {}",
    sep = "\n")
  err <- expect_error(
    roxygen2::parse_text(text))
  expect_match(err$message,
               "error occured at <text>:5", fixed = TRUE)
})

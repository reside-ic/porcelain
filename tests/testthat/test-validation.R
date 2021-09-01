context("validation")


test_that("find schema root", {
  expect_equal(schema_root("."), normalizePath("."))
  expect_error(schema_root(tempfile()))
})


test_that("default validation", {
  withr::with_envvar(c("PORCELAIN_VALIDATE" = NA_character_), {
    expect_true(porcelain_validate_default(TRUE))
    expect_false(porcelain_validate_default(FALSE))
    expect_false(porcelain_validate_default(NULL))
  })

  withr::with_envvar(c("PORCELAIN_VALIDATE" = "true"), {
    expect_true(porcelain_validate_default(TRUE))
    expect_false(porcelain_validate_default(FALSE))
    expect_true(porcelain_validate_default(NULL))
  })

  withr::with_envvar(c("PORCELAIN_VALIDATE" = "false"), {
    expect_true(porcelain_validate_default(TRUE))
    expect_false(porcelain_validate_default(FALSE))
    expect_false(porcelain_validate_default(NULL))
  })
})


test_that("validate successful return", {
  path <- system_file("schema/response-success.schema.json",
                      package = "porcelain")
  v <- jsonvalidate::json_validator(path, "ajv")
  expect_true(v(to_json(response_success(NULL))))
  expect_true(v(to_json(response_success(1))))
})


test_that("validate errors", {
  path <- system_file("schema/response-failure.schema.json",
                      package = "porcelain")
  v <- jsonvalidate::json_validator(path, "ajv")

  f <- function(x) {
    porcelain_process_error(porcelain_error_object(x, 400L))
  }

  e1 <- f(list("ERROR" = list(detail = "reason")))
  expect_equal(e1$value$errors, list(list(error = jsonlite::unbox("ERROR"),
                                          detail = jsonlite::unbox("reason"))))
  expect_true(v(e1$body))

  e2 <- f(list("ERROR" = NULL))
  expect_equal(e2$value$errors, list(list(error = jsonlite::unbox("ERROR"),
                                          detail = NULL)))
  expect_true(v(e2$body))

  e3 <- f(list("ERROR" = NULL, "OTHER" = list(detail = "reason")))
  expect_equal(e3$value$errors,
               list(list(error = jsonlite::unbox("ERROR"),
                         detail = NULL),
                    list(error = jsonlite::unbox("OTHER"),
                         detail = jsonlite::unbox("reason"))))
  expect_true(v(e3$body))

  e4 <- f(list("ERROR" = NULL, "OTHER" = list(detail = "reason")))
  expect_equal(e4$value$errors,
               list(list(error = jsonlite::unbox("ERROR"),
                         detail = NULL),
                    list(error = jsonlite::unbox("OTHER"),
                         detail = jsonlite::unbox("reason"))))
  expect_true(v(e4$body))

  e5 <- f(list("ERROR" = list(detail = "reason",
                              key = jsonlite::unbox("key"),
                              trace = c(jsonlite::unbox("the"),
                                        jsonlite::unbox("trace")))))
  expect_equal(e5$value$errors,
               list(list(error = jsonlite::unbox("ERROR"),
                         detail = jsonlite::unbox("reason"),
                         key = jsonlite::unbox("key"),
                         trace = c(jsonlite::unbox("the"),
                                   jsonlite::unbox("trace")))))
  expect_true(v(e5$body))
})


test_that("validate schema - success", {
  hello <- function() {
    jsonlite::unbox("hello")
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  res <- endpoint$run()
  expect_equal(res$status_code, 200L)
  expect_true(res$validated)
})


test_that("validate schema", {
  hello <- function() {
    jsonlite::unbox(1)
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  res <- endpoint$run()

  expect_is(res, "porcelain_response")
  expect_equal(res$status_code, 500L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$body, to_json_string(res$value))
  expect_is(res$error, "porcelain_validation_error")

  ## FALSE, because we did not validate the error schema here
  expect_false(res$validated)

  expect_equal(to_json_string(response_success(hello())), res$error$json)
})


test_that("can skip validation", {
  hello <- function() {
    jsonlite::unbox(1)
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json("String", "schema"),
    validate = FALSE)
  res <- endpoint$run()
  expect_equal(res$status_code, 200L)
  expect_false(res$validated)
})


test_that("validation respects default", {
  f <- function() {
    porcelain_endpoint$new(
      "GET", "/", function() jsonlite::unbox(1),
      returning = porcelain_returning_json(
        "String", "schema"))$run()$status_code
  }

  withr::with_envvar(c("PORCELAIN_VALIDATE" = NA_character_),
                     expect_equal(f(), 200))
  withr::with_envvar(c("PORCELAIN_VALIDATE" = "false"),
                     expect_equal(f(), 200))
  withr::with_envvar(c("PORCELAIN_VALIDATE" = "true"),
                     expect_equal(f(), 500))
})


test_that("return informaion about validation in header", {
  endpoint <- porcelain_endpoint$new(
    "GET", "/", function() jsonlite::unbox("hello"),
    returning = porcelain_returning_json("String", "schema"))
  api <- porcelain$new(validate = TRUE)
  api$handle(endpoint)
  res <- api$request("GET", "/")
  expect_equal(res$status, 200)
  expect_equal(res$headers[["X-Porcelain-Validated"]], "true")
})


test_that("override validation defaults at the api level", {
  endpoint <- porcelain_endpoint$new(
    "GET", "/", function() jsonlite::unbox(1),
    returning = porcelain_returning_json("String", "schema"),
    validate = TRUE)
  api <- porcelain$new(validate = FALSE)
  api$handle(endpoint)
  expect_true(endpoint$validate)

  expect_equal(endpoint$run()$status_code, 500)

  res <- api$request("GET", "/")
  expect_equal(res$status, 200)
  expect_equal(res$headers[["X-Porcelain-Validated"]], "false")
})


test_that("allow missing schema", {
  hello <- function() {
    jsonlite::unbox(1)
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/", hello,
    returning = porcelain_returning_json(),
    validate = TRUE)
  res <- endpoint$run()
  expect_equal(res$status_code, 200L)
  expect_equal(res$content_type, "application/json")
  expect_equal(res$data, hello())
  expect_equal(res$body, to_json_string(response_success(res$data)))
})


test_that("validate binary output", {
  binary <- function() {
    "not binary"
  }
  endpoint <- porcelain_endpoint$new(
    "GET", "/binary", binary,
    returning = porcelain_returning_binary(),
    validate = TRUE)
  res <- endpoint$run()
  expect_equal(res$status_code, 500L)
  endpoint$validate <- FALSE
  res <- endpoint$run()
  expect_equal(res$status_code, 200L)
})


test_that("Find schema root", {
  expect_true(same_path(
    schema_root(environment(porcelain_validate)),
    system_file("schema", package = "porcelain")))
  expect_true(same_path(
    schema_root(new.env(parent = environment(porcelain_validate))),
    system_file("schema", package = "porcelain")))
  expect_error(schema_root(environment(jsonlite::parse_json)),
               "File does not exist")
})


test_that("find schema by adding extensions", {
  tmp <- tempfile()
  dir.create(tmp)
  tmp <- normalizePath(tmp)
  expect_equal(find_schema("foo", tmp),
               file.path(tmp, "foo"))

  file.create(file.path(tmp, "foo.schema.json"))
  expect_equal(find_schema("foo", tmp),
               file.path(tmp, "foo.schema.json"))

  file.create(file.path(tmp, "foo.json"))
  expect_equal(find_schema("foo", tmp),
               file.path(tmp, "foo.json"))

  file.create(file.path(tmp, "foo"))
  expect_equal(find_schema("foo", tmp),
               file.path(tmp, "foo"))
})


test_that("sensible output returned for impossible root", {
  expect_null(schema_root(globalenv()))
  expect_error(find_schema("foo", NULL),
               "Did not find schema root")
})

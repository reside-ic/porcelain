context("utils")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("lock_bindings", {
  e <- new.env(parent = emptyenv())
  e$a <- 1
  e$b <- 2
  lock_bindings(c("a", "b"), e)
  expect_error(e$a <- 2)
})


test_that("parse_path_parameters", {
  expect_null(parse_path_parameters("/"))
  expect_null(parse_path_parameters("/no/routing/at/all"))
  expect_equal(parse_path_parameters("/my/<id>"),
               cbind(name = "id", type = "string"))
  expect_equal(parse_path_parameters("/my/<dynamic>/path"),
               cbind(name = "dynamic", type = "string"))
  expect_equal(parse_path_parameters("/my/<id:int>"),
               cbind(name = "id", type = "integer"))
  expect_equal(parse_path_parameters("/my/<id:int>/<action>"),
               cbind(name = c("id", "action"),
                     type = c("integer", "string")))
  expect_equal(parse_path_parameters("/my/<id:apple>/<action>"),
               cbind(name = c("id", "action"),
                     type = c("string", "string")))
})


test_that("parse_mime", {
  expect_equal(
    parse_mime("text/plain"),
    list(mime = "text/plain", type = "text", subtype = "plain",
         is_text = TRUE))
  expect_equal(
    parse_mime("application/json"),
    list(mime = "application/json", type = "application", subtype = "json",
         is_text = TRUE))
  expect_equal(
    parse_mime("application/octet-stream"),
    list(mime = "application/octet-stream", type = "application",
         subtype = "octet-stream", is_text = FALSE))

  expect_equal(
    parse_mime("text/plain;charset=UTF-8"),
    parse_mime("text/plain"))
})

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


test_that("parse_plumber_path", {
  expect_null(parse_plumber_path("/"))
  expect_null(parse_plumber_path("/no/routing/at/all"))
  expect_equal(parse_plumber_path("/my/<id>"),
               cbind(name = "id", type = "string"))
  expect_equal(parse_plumber_path("/my/<dynamic>/path"),
               cbind(name = "dynamic", type = "string"))
  expect_equal(parse_plumber_path("/my/<id:int>"),
               cbind(name = "id", type = "integer"))
  expect_equal(parse_plumber_path("/my/<id:int>/<action>"),
               cbind(name = c("id", "action"),
                     type = c("integer", "string")))
  expect_equal(parse_plumber_path("/my/<id:apple>/<action>"),
               cbind(name = c("id", "action"),
                     type = c("string", "string")))
})

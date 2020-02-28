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

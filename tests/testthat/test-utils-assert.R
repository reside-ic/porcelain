test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_error(assert_character(1), "must be character")
  expect_error(assert_character(TRUE), "must be character")
})


test_that("assert_logical", {
  expect_silent(assert_logical(TRUE))
  expect_error(assert_logical(1), "must be logical")
  expect_error(assert_logical("true"), "must be logical")
})


test_that("assert_numeric", {
  expect_silent(assert_numeric(1))
  expect_error(assert_numeric(TRUE), "must be numeric")
  expect_error(assert_numeric("one"), "must be numeric")
})


test_that("assert_nonmissing", {
  object <- NA
  expect_error(assert_nonmissing(object), "'object' must not be NA")

  expect_error(assert_nonmissing(NA_integer_), "must not be NA")
  expect_error(assert_nonmissing(NA_real_), "must not be NA")

  expect_silent(assert_nonmissing(TRUE))
})


test_that("match_value", {
  expect_error(match_value("foo", letters), "must be one of")
  expect_silent(match_value("a", letters))
})


test_that("assert_named", {
  expect_error(assert_named(1), "must be named")
  expect_error(assert_named(setNames(1:2, c("a", "a")), TRUE),
               "must have unique names")
  expect_silent(assert_named(setNames(1:2, c("a", "a")), FALSE))
  expect_error(assert_named(list(a = "one", "two")),
               "must be named")
})


test_that("assert_is", {
  expect_error(assert_is("x", "foo"), "must be a foo")
  expect_silent(assert_is(structure("x", class = "foo"), "foo"))
})


test_that("assert_file_exists", {
  path <- tempfile()
  expect_error(assert_file_exists(path), "File does not exist")
  writeLines(character(0), path)
  expect_silent(assert_file_exists(path))
})


test_that("assert_is_directory", {
  path <- tempfile()
  expect_error(assert_is_directory(path), "File does not exist")
  file.create(path)
  expect_error(assert_is_directory(path), "File exists but is not a directory")
  expect_silent(assert_is_directory("."))
})


test_that("assert_raw", {
  expect_error(assert_raw("x", "foo"), "must be a raw vector")
  expect_silent(assert_raw(as.raw(0:255)))
})

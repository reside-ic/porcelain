context("utils (assert)")


test_that("assert_named", {
  expect_error(assert_named(1), "must be named")
  expect_error(assert_named(setNames(1:2, c("a", "a")), TRUE),
               "must have unique names")
  expect_silent(assert_named(setNames(1:2, c("a", "a")), FALSE))
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

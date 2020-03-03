context("request")

test_that("content type guessing works", {
  expect_equal(request_content_type(as.raw(0:5), NULL),
               "application/octet-stream")
  expect_equal(request_content_type(as.raw(0:5), "image/png"),
               "image/png")
  expect_equal(request_content_type("string", NULL),
               "application/json") # note - this is not actually valid json
  expect_equal(request_content_type("string", "text/plain"),
               "text/plain")
})

test_that("can arrange to return text", {
  r <- porcelain_returning_text()
  expect_s3_class(r, "porcelain_returning")
  expect_equal(r$content_type, "text/plain")
  expect_identical(r$process, identity)
  expect_identical(r$validate, assert_scalar_character)
})


test_that("can arrange to return binary", {
  r <- porcelain_returning_binary()
  expect_s3_class(r, "porcelain_returning")
  expect_equal(r$content_type, "application/octet-stream")
  expect_identical(r$process, identity)
  expect_identical(r$validate, assert_raw)
})


test_that("can arrange to return json", {
  r <- porcelain_returning_json()
  expect_s3_class(r, "porcelain_returning")
  expect_equal(r$content_type, "application/json")
  expected <- '{"status":"success","errors":null,"data":[true]}'
  expect_equal(r$process(TRUE), expected)
  expect_silent(r$validate(expected))
})

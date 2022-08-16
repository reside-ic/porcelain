test_that("query strings are properly parsed", {
  expect_equal(parse_query("?a=1"), list(a = "1"))
  expect_equal(parse_query("b=2"), list(b = "2"))
  expect_equal(parse_query("a=1&b=2&c=url%20encoded"),
               list(a = "1", b = "2", c = "url encoded"))
})


test_that("special characters in query strings are handled properly", {
  expect_equal(parse_query("?a=1+.#"), list(a = "1 .#"))
  expect_equal(parse_query("?a=a%20b"), list(a = "a b"))
  expect_equal(parse_query("?a = %2C%2B%2F%3F%25%26"), list(a = ",+/?%&"))
})


test_that("null an empty strings return empty list", {
  expect_equal(parse_query(NULL), list())
  expect_equal(parse_query(""), list())
  expect_equal(parse_query("?"), list())
})


test_that("incomplete query strings are errors", {
  expect_error(parse_query("a="), "Incomplete query for 'a'")
  expect_error(parse_query("a=1&b=&c&d=1"), "Incomplete query for 'b', 'c'")
})


test_that("query strings with duplicates are errors", {
  expect_error(parse_query("a=1&a=2&a=3&a=4"),
               "Unexpected duplicate keys 'a'")
  expect_error(parse_query("a=1&b=2&b=3&c=4&c=5"),
               "Unexpected duplicate keys 'b', 'c'")
})

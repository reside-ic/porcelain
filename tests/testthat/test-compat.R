with_plumber_1_0_0 <- function(value, code) {
  prev <- cache$plumber_1_0_0
  on.exit(cache$plumber_1_0_0 <- prev)
  cache$plumber_1_0_0 <- value
  force(code)
}

test_that("base class under new and old plumber", {
  with_plumber_1_0_0(
    TRUE,
    expect_equal(plumber_base_class_name(), "Plumber"))
  with_plumber_1_0_0(
    FALSE,
    expect_equal(plumber_base_class_name(), "plumber"))
})


test_that("path handling under new", {
  req <- list(argsPath = list(x = "a"),
              args = list(y = "b", req = NULL, res = NULL))
  with_plumber_1_0_0(
    TRUE,
    expect_equal(plumber_path_args(req), list(x = "a")))
  with_plumber_1_0_0(
    FALSE,
    expect_equal(plumber_path_args(req), list(y = "b")))
})

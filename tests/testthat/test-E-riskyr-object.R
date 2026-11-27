# Tests (E): Object integrity
#
# Verify that riskyr() builds a well-formed object and that its S3 methods
# (summary, print) run without error.


test_that("riskyr() builds a valid object carrying its inputs", {

  s <- riskyr(prev = .1, sens = .8, spec = .9, N = 1000)

  expect_s3_class(s, "riskyr")

  expect_equal(s$prev, .1)
  expect_equal(s$sens, .8)
  expect_equal(s$spec, .9)

  expect_equal(s$N, 1000)

})


test_that("summary() and print() run without error on a riskyr object", {

  s <- riskyr(prev = .1, sens = .8, spec = .9, N = 1000)

  expect_no_error(invisible(capture.output(summary(s))))

  expect_output(print(s))  # print() runs and produces console output

})

test_that("all built-in scenarios are valid riskyr objects", {

  expect_type(scenarios, "list")

  expect_length(scenarios, 25)

  expect_true(all(vapply(scenarios, function(x) inherits(x, "riskyr"), logical(1))))

})

# eof.

# Tests (D): Extreme cases
#
# Boundary probabilities (0 and 1) are VALID probabilities, so they pass input
# validation and are computed. Some combinations make a denominator zero and
# yield NaN (with a warning) -- this pins that intended behavior so it can't
# regress silently.

# -- Finite boundary values (hand-verifiable, exact) ------------------------

test_that("PPV at boundaries gives exact finite values", {

  expect_equal(comp_PPV(0,  .8, .9), 0)   # prev = 0  -> no true positives (hi = 0)
  expect_equal(comp_PPV(.1,  0, .9), 0)   # sens = 0  -> hi = 0
  expect_equal(comp_PPV(.1, .8,  1), 1)   # spec = 1  -> no false alarms (fa = 0)

})


test_that("NPV at boundaries gives exact finite values", {

  expect_equal(comp_NPV(1,  .8, .9), 0)   # prev = 1  -> no true negatives (cr = 0)
  expect_equal(comp_NPV(.1,  1, .9), 1)   # sens = 1  -> no misses (mi = 0)
  expect_equal(comp_NPV(.1, .8,  0), 0)   # spec = 0  -> cr = 0

})


test_that("accuracy stays finite across boundaries", {

  expect_equal(comp_acc(0, .8, .9), 0.9)  # hi + cr = 0 + .9
  expect_equal(comp_acc(1, .8, .9), 0.8)  # hi + cr = .8 + 0
  expect_equal(comp_acc(0, .8,  1), 1)    # perfect:  cr = 1
  expect_equal(comp_acc(1,  0, .9), 0)    # worst:    hi = 0, cr = 0

  expect_false(is.nan(comp_acc(1, 1, 1)))

})

# -- NaN-producing cases (denominator == 0) --------------------------------

test_that("PPV is NaN (with warning) when hi + fa == 0", {

  # The three extreme combinations that zero out all positive decisions:
  expect_warning(a <- comp_PPV(1, 0, .9))   # prev = 1 & sens = 0
  expect_true(is.nan(a))

  expect_warning(b <- comp_PPV(0, .8, 1))   # prev = 0 & spec = 1
  expect_true(is.nan(b))

  expect_warning(c <- comp_PPV(.1, 0, 1))   # sens = 0 & spec = 1 (any prev)
  expect_true(is.nan(c))

})


test_that("NPV is NaN (with warning) when mi + cr == 0", {

  expect_warning(a <- comp_NPV(1, 1, .9))   # prev = 1 & sens = 1
  expect_true(is.nan(a))

  expect_warning(b <- comp_NPV(.1, 1, 0))   # sens = 1 & spec = 0 (any prev)
  expect_true(is.nan(b))

})


# -- Boundary values are valid inputs (not a validation failure) ------------

test_that("boundary probabilities (0 and 1) pass input validation", {

  # 0 and 1 are valid probabilities, so a finite boundary result is returned
  # without the "must be probabilities" validation warning.
  expect_true(is_valid_prob_triple(0, 0, 0))
  expect_true(is_valid_prob_triple(1, 1, 1))

  expect_no_warning(comp_PPV(0, .8, .9))    # finite (= 0), no warning

})

# eof.

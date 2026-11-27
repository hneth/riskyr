# Tests (B): Input validation added to computing functions
#
# These lock in the validation behavior: invalid or incomplete input sets now
# yield NA (with a warning) rather than silently computing invalid values.

# -- Probability metric functions -------------------------------------------

test_that("metric functions reject invalid probability inputs with NA + warning", {

  expect_warning(ppv <- comp_PPV(2, .8, .9))   # prev out of range
  expect_true(is.na(ppv))

  expect_warning(npv <- comp_NPV(.1, 2, .9))   # sens out of range
  expect_true(is.na(npv))

  expect_warning(fdr <- comp_FDR(.1, .8, -1))  # spec out of range
  expect_true(is.na(fdr))

  expect_warning(acc <- comp_acc(NA, .8, .9))  # prev missing
  expect_true(is.na(acc))

})

test_that("metric functions compute normally for valid inputs", {

  # PPV = hi / (hi + fa) = (prev*sens) / (prev*sens + (1-prev)*(1-spec))
  expect_equal(comp_PPV(.10, .80, .90), 0.08 / (.08 + .09))
  expect_false(is.na(comp_NPV(.10, .80, .90)))
  expect_false(is.na(comp_acc(.10, .80, .90)))

})


# -- is_valid_freq_set() ----------------------------------------------------

test_that("is_valid_freq_set() verifies a set of 4 frequencies", {

  expect_true(is_valid_freq_set(1, 2, 3, 4))
  expect_true(is_valid_freq_set(0, 0, 0, 0))

  # Non-integer frequencies are allowed by default (exact frequencies):
  expect_true(is_valid_freq_set(10.5, 2, 3, 4))

  # ... but can be rejected when integers are required:
  expect_false(is_valid_freq_set(10.5, 2, 3, 4, allow_noninteger = FALSE))

  # Invalid inputs:
  expect_false(is_valid_freq_set(1, 2, 3, NA))   # contains NA
  expect_false(is_valid_freq_set(1, 2, 3, -4))   # contains a negative value

  # Vector-safe: a single Boolean for whole vectors:
  expect_true(is_valid_freq_set(c(1,   2), c(3, 4), c(5, 6), c(7, 8)))
  expect_false(is_valid_freq_set(c(1, -2), c(3, 4), c(5, 6), c(7, 8)))

})

# -- Frequency-based functions ----------------------------------------------

test_that("frequency functions validate their inputs", {

  # Incomplete set (an NA frequency) -> NA result + warning:
  expect_warning(pb <- comp_prob_freq(10, 5, NA, 20))
  expect_true(is.na(pb$prev))

  # comp_popu() requires integer frequencies -> NULL + warning for non-integers:
  expect_warning(pop <- comp_popu(10.5, 5, 3, 20))
  expect_null(pop)

})

test_that("frequency functions compute normally for valid inputs", {

  pb <- comp_prob_freq(hi = 10, mi = 5, fa = 3, cr = 20)
  expect_false(is.na(pb$prev))
  expect_equal(pb$prev, (10 + 5) / (10 + 5 + 3 + 20))  # (hi + mi) / N

  pop <- comp_popu(hi = 10, mi = 5, fa = 3, cr = 20)
  expect_equal(nrow(pop), 10 + 5 + 3 + 20)

})

test_that("comp_accu_freq() validates its frequency inputs", {

  # Valid: returns accuracy metrics; acc = (hi + cr) / N:
  a <- comp_accu_freq(hi = 10, mi = 5, fa = 3, cr = 20)
  expect_equal(a$acc, (10 + 20) / (10 + 5 + 3 + 20))

  # Invalid (NA) or negative frequency -> NA metrics + warning:
  expect_warning(a_na <- comp_accu_freq(10, 5, NA, 20))
  expect_true(is.na(a_na$acc))

  expect_warning(a_neg <- comp_accu_freq(10, 5, -3, 20))
  expect_true(is.na(a_neg$acc))

})

test_that("comp_freq_freq() validates its frequency inputs", {

  # Valid: derives compound frequencies; the 4 essentials sum to N:
  f <- comp_freq_freq(hi = 10, mi = 5, fa = 3, cr = 20)
  expect_equal(f$N, 10 + 5 + 3 + 20)

  # Invalid (NA) or negative frequency -> NA result + warning:
  expect_warning(f_na <- comp_freq_freq(10, 5, NA, 20))
  expect_true(is.na(f_na$N))

  expect_warning(f_neg <- comp_freq_freq(-1, 5, 3, 20))
  expect_true(is.na(f_neg$N))

})

# eof.

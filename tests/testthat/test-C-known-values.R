# Tests (C): Known-value anchors
#
# These pin EXACT numeric outputs for a few well-defined inputs, so that any
# numeric drift anywhere in the computation chain fails loudly. Expected values
# are given as hand-derivable expressions (not magic constants) wherever possible.

test_that("clean example (prev = .1, sens = .8, spec = .9) gives exact metrics", {

  p <- .1; se <- .8; sp <- .9

  # Underlying (exact) joint probabilities:
  #   hi = prev*sens            = .08
  #   mi = prev*(1-sens)        = .02
  #   fa = (1-prev)*(1-spec)    = .09
  #   cr = (1-prev)*spec        = .81

  expect_equal(comp_PPV(p, se, sp),  0.08 / (0.08 + 0.09))  # hi / (hi + fa)
  expect_equal(comp_NPV(p, se, sp),  0.81 / (0.81 + 0.02))  # cr / (cr + mi)

  expect_equal(comp_FDR(p, se, sp),  0.09 / (0.08 + 0.09))  # fa / (hi + fa)
  expect_equal(comp_FOR(p, se, sp),  0.02 / (0.81 + 0.02))  # mi / (cr + mi)

  expect_equal(comp_acc(p, se, sp),  0.08 + 0.81)           # hi + cr
  expect_equal(comp_err(p, se, sp),  0.02 + 0.09)           # mi + fa

  expect_equal(comp_ppod(p, se, sp), 0.08 + 0.09)           # hi + fa (proportion positive)

})


test_that("classic mammography problem reproduces the textbook PPV (~7.8%)", {

  # Gigerenzer's mammography example: prevalence 1%, sensitivity 80%,
  # specificity 90.4% (false-alarm rate 9.6%). The famously counter-intuitive
  # result is a positive predictive value of only ~7.8%.
  #   hi = .01*.80         = .008
  #   fa = .99*(1-.904)    = .09504  ->  hi + fa = .10304

  expect_equal(comp_PPV(.01, .80, .904), .008 / .10304)
  expect_equal(round(comp_PPV(.01, .80, .904), 3), .078)

})


test_that("built-in scenario 'n1' (bowel cancer screening) has stable parameters and metrics", {

  s1 <- scenarios$n1

  # Parameters (anchors the dataset itself):
  expect_equal(s1$prev, 0.015)
  expect_equal(s1$sens, 0.667)
  expect_equal(s1$spec, 0.910)

  # Computed metrics (regression anchors; hand-verifiable from prev/sens/spec):
  expect_equal(comp_PPV(s1$prev, s1$sens, s1$spec), 0.101414, tolerance = 1e-5)
  expect_equal(comp_NPV(s1$prev, s1$sens, s1$spec), 0.994458, tolerance = 1e-5)
  expect_equal(comp_acc(s1$prev, s1$sens, s1$spec), 0.906355, tolerance = 1e-5)

})


test_that("accuracy metrics (comp_accu / comp_accu_prob) have known values", {

  # From 4 frequencies (hi = 10, mi = 5, fa = 3, cr = 20; N = 38), balanced w = .50:
  a <- comp_accu(hi = 10, mi = 5, fa = 3, cr = 20)
  expect_equal(a$acc,  (10 + 20) / 38)               # acc = (hi + cr) / N
  expect_equal(a$f1s,  2 * 10 / (2 * 10 + 3 + 5))    # F1  = 2*hi / (2*hi + fa + mi)
  expect_equal(a$wacc, 0.768116, tolerance = 1e-5)   # balanced accuracy
  expect_equal(a$mcc,  0.552485, tolerance = 1e-5)   # Matthews correlation

  # Weight w = 1 makes wacc equal sensitivity (hi / (hi + mi)):
  expect_equal(comp_accu(hi = 10, mi = 5, fa = 3, cr = 20, w = 1)$wacc, 10 / 15)

  # From probabilities: acc matches comp_acc(); wacc = w*sens + (1-w)*spec:
  ap <- comp_accu_prob(prev = .1, sens = .8, spec = .9)
  expect_equal(ap$acc,  0.89)
  expect_equal(ap$acc,  comp_acc(.1, .8, .9))
  expect_equal(ap$wacc, 0.85)

})


test_that("init_num() builds a valid num list (completing complements and N)", {

  # Provided N is kept; the missing complement (fart) is computed from spec:
  n <- init_num(prev = .1, sens = .8, spec = .9, N = 100)
  expect_type(n, "list")
  expect_equal(n$prev, .1)
  expect_equal(n$sens, .8)
  expect_equal(n$spec, .9)
  expect_equal(n$fart, .1)   # fart = 1 - spec
  expect_equal(n$N,    100)

  # The complement is completed both ways (spec computed from fart):
  expect_equal(init_num(prev = .1, sens = .8, spec = NA, fart = .1)$spec, .9)

  # With no N, a suitable minimum N is computed (via comp_min_N):
  expect_equal(init_num(prev = .1, sens = .8, spec = .9)$N, 1000)

})


test_that("comp_min_N() returns the minimum (power-of-10) N for a given min_freq", {

  expect_equal(comp_min_N(.1, .8, .9), 100)   # smallest cell = .02  -> N*.02 >= 1 -> 100
  expect_equal(comp_min_N(.5, .5, .5),  10)   # all cells = .25      -> N*.25 >= 1 ->  10
  expect_equal(comp_min_N(.1, .8, .9, min_freq = 10), 1000)  # larger min_freq -> larger N
  expect_equal(comp_min_N(.1, .8, .9, min_freq =  0),    1)  # no constraint   -> N = 1

})


# eof.

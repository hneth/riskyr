# Tests (A): Mathematical identities / invariants
#
# These verify relationships that must ALWAYS hold between riskyr's computations,
# across a grid of inputs. They catch whole classes of regressions cheaply,
# without having to hand-compute expected values.
#
# A grid of strictly interior probabilities (0 < p < 1) is used so that all
# denominators are positive (no 0/0 = NaN); extreme cases (0 and 1) are a
# separate concern (category D) and are not covered here.

interior_grid <- expand.grid(

  prev = c(.01, .10, .50, .90),
  sens = c(.10, .50, .80, .99),
  spec = c(.10, .50, .90, .99)

)

test_that("predictive-value complements sum to 1 (PPV + FDR, NPV + FOR)", {

  for (i in seq_len(nrow(interior_grid))) {

    p  <- interior_grid$prev[i]
    se <- interior_grid$sens[i]
    sp <- interior_grid$spec[i]

    expect_equal(comp_PPV(p, se, sp) + comp_FDR(p, se, sp), 1)
    expect_equal(comp_NPV(p, se, sp) + comp_FOR(p, se, sp), 1)
  }

})


test_that("accuracy and error are complements (acc + err == 1)", {

  for (i in seq_len(nrow(interior_grid))) {

    p  <- interior_grid$prev[i]
    se <- interior_grid$sens[i]
    sp <- interior_grid$spec[i]

    expect_equal(comp_acc(p, se, sp) + comp_err(p, se, sp), 1)

  }

})


test_that("decision-feature complements hold (sens/mirt, spec/fart)", {

  for (x in c(0, .25, .50, .75, 1)) {

    # comp_mirt() and comp_sens() are complements (sum to 1):
    expect_equal(comp_mirt(x) + x, 1)
    expect_equal(comp_sens(x) + x, 1)

    # comp_fart() and comp_spec() are complements (sum to 1):
    expect_equal(comp_fart(x) + x, 1)
    expect_equal(comp_spec(x) + x, 1)

    # Applying a conversion and its inverse returns the original value:
    expect_equal(comp_sens(comp_mirt(x)), x)
    expect_equal(comp_spec(comp_fart(x)), x)

  }

})


test_that("prob -> freq -> prob round-trips (using exact, non-rounded freq)", {

  grid <- expand.grid(prev = c(.10, .25, .40),
                      sens = c(.60, .80),
                      spec = c(.70, .90))

  for (i in seq_len(nrow(grid))) {

    p  <- grid$prev[i]
    se <- grid$sens[i]
    sp <- grid$spec[i]

    fr <- comp_freq(prev = p, sens = se, spec = sp, round = FALSE)
    pb <- comp_prob_freq(hi = fr$hi, mi = fr$mi, fa = fr$fa, cr = fr$cr)

    expect_equal(pb$prev, p)
    expect_equal(pb$sens, se)
    expect_equal(pb$spec, sp)

  }

})


test_that("the 4 essential frequencies sum to N", {

  grid <- expand.grid(prev = c(.10, .33), sens = c(.70, .90), spec = c(.60, .95))

  for (i in seq_len(nrow(grid))) {

    fr <- comp_freq(prev = grid$prev[i], sens = grid$sens[i], spec = grid$spec[i],
                    round = FALSE)
    expect_equal(fr$hi + fr$mi + fr$fa + fr$cr, fr$N)

  }

})

# eof.

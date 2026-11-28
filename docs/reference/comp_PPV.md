# Compute a decision's positive predictive value (PPV) from probabilities.

`comp_PPV` computes the positive predictive value
[`PPV`](https://hneth.github.io/riskyr/reference/PPV.md) from 3
essential probabilities
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md), and
[`spec`](https://hneth.github.io/riskyr/reference/spec.md).

## Usage

``` r
comp_PPV(prev, sens, spec)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md) (i.e., the
  probability of condition being `TRUE`).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/reference/sens.md) (i.e., the
  conditional probability of a positive decision provided that the
  condition is `TRUE`).

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/reference/spec.md) (i.e., the
  conditional probability of a negative decision provided that the
  condition is `FALSE`).

## Value

The positive predictive value
[`PPV`](https://hneth.github.io/riskyr/reference/PPV.md) as a
probability. A warning is provided for NaN values.

## Details

`comp_PPV` uses probabilities (not frequencies) and does not round
results.

## See also

[`comp_sens`](https://hneth.github.io/riskyr/reference/comp_sens.md) and
[`comp_NPV`](https://hneth.github.io/riskyr/reference/comp_NPV.md)
compute related probabilities;
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/reference/is_extreme_prob_set.md)
verifies extreme cases;
[`comp_complement`](https://hneth.github.io/riskyr/reference/comp_complement.md)
computes a probability's complement;
[`is_complement`](https://hneth.github.io/riskyr/reference/is_complement.md)
verifies probability complements;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md)
verifies probabilities.

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/reference/comp_FOR.md),
[`comp_NPV()`](https://hneth.github.io/riskyr/reference/comp_NPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/reference/comp_spec.md)

## Examples

``` r
# (1) Ways to work:
comp_PPV(.50, .500, .500)  # => PPV = 0.5
#> [1] 0.5
comp_PPV(.50, .333, .666)  # => PPV = 0.499
#> [1] 0.4992504

# (2) Watch out for vectors:
prev <- seq(0, 1, .1)
comp_PPV(prev, .5, .5)  # => without NaN values
#>  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
comp_PPV(prev,  0,  1)  # => with NaN values
#> Warning: PPV is NaN.
#>  [1] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN

# (3) Watch out for extreme values:
comp_PPV(prev = 1, sens = 0, spec = .5)  # => NaN, only mi: hi = 0 and fa = 0: PPV = 0/0 = NaN
#> Warning: PPV is NaN.
#> [1] NaN
is_extreme_prob_set(prev = 1, sens = 0, spec = .5)  # => verifies extreme cases
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> [1] TRUE

comp_PPV(prev = 0, sens = .5, spec = 1)  # => NaN, only cr: hi = 0 and fa = 0: PPV = 0/0 = NaN
#> Warning: PPV is NaN.
#> [1] NaN
is_extreme_prob_set(prev = 0, sens = .5, spec = 1)  # => verifies extreme cases
#> Warning: Extreme case (prev = 0 & spec = 1):
#>   N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.
#> [1] TRUE

comp_PPV(prev = .5, sens = 0, spec = 1)  # => NaN, only cr: hi = 0 and fa = 0: PPV = 0/0 = NaN
#> Warning: PPV is NaN.
#> [1] NaN
is_extreme_prob_set(prev = .5, sens = 0, spec = 1)  # => verifies extreme cases
#> Warning: Extreme case (sens = 0 & spec = 1):
#>   0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.
#> [1] TRUE
```

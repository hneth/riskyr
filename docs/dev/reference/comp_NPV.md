# Compute a decision's negative predictive value (NPV) from probabilities.

`comp_NPV` computes the negative predictive value
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) from 3
essential probabilities
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md).

## Usage

``` r
comp_NPV(prev, sens, spec)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (i.e.,
  the probability of condition being `TRUE`).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `TRUE`).

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `FALSE`).

## Value

The negative predictive value
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) as a
probability. A warning is provided for NaN values.

## Details

`comp_NPV` uses probabilities (not frequencies) and does not round
results.

## See also

[`comp_spec`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)
and
[`comp_PPV`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md)
compute related probabilities;
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
verifies extreme cases;
[`comp_complement`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md)
computes a probability's complement;
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)
verifies probability complements;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies probabilities.

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/dev/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/dev/reference/comp_FOR.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/dev/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

## Examples

``` r
# (1) Ways to work:
comp_NPV(.50, .500, .500)  # => NPV = 0.5
#> [1] 0.5
comp_NPV(.50, .333, .666)  # => NPV = 0.4996
#> [1] 0.4996249

# (2) Watch out for vectors:
prev <- seq(0, 1, .1)
comp_NPV(prev, .5, .5)  # => without NaN values
#>  [1] 1.0 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1 0.0
comp_NPV(prev,  1,  0)  # => with NaN values
#> Warning: NPV is NaN.
#>  [1] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN

# (3) Watch out for extreme values:
comp_NPV(1, 1, 1)   # => NaN, as cr = 0 and mi = 0: 0/0
#> Warning: NPV is NaN.
#> [1] NaN
comp_NPV(1, 1, 0)   # => NaN, as cr = 0 and mi = 0: 0/0
#> Warning: NPV is NaN.
#> [1] NaN
comp_NPV(.5, sens = 1, spec = 0)  # => NaN, no dec_neg cases:  NPV = 0/0 = NaN
#> Warning: NPV is NaN.
#> [1] NaN
is_extreme_prob_set(.5, sens = 1, spec = 0)  # => verifies extreme cases
#> Warning: Extreme case (sens = 1 & spec = 0):
#>   0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.
#> [1] TRUE
```

# Compute a decision's false omission rate (FOR) from probabilities.

`comp_FOR` computes the false omission rate
[`FOR`](https://hneth.github.io/riskyr/reference/FOR.md) from 3
essential probabilities
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md), and
[`spec`](https://hneth.github.io/riskyr/reference/spec.md).

## Usage

``` r
comp_FOR(prev, sens, spec)
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

The false omission rate
[`FOR`](https://hneth.github.io/riskyr/reference/FOR.md) as a
probability. A warning is provided for NaN values.

## Details

`comp_FOR` uses probabilities (not frequencies) and does not round
results.

## See also

[`comp_spec`](https://hneth.github.io/riskyr/reference/comp_spec.md) and
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
[`comp_NPV()`](https://hneth.github.io/riskyr/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/reference/comp_PPV.md),
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
comp_FOR(.50, .500, .500)  # => FOR = 0.5    = (1 - NPV)
#> [1] 0.5
comp_FOR(.50, .333, .666)  # => FOR = 0.5004 = (1 - NPV)
#> [1] 0.5003751
```

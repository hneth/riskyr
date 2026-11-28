# Compute overall error rate (err) from probabilities.

`comp_err` computes overall error rate
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md) from 3
essential probabilities
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md).

## Usage

``` r
comp_err(prev, sens, spec)
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

  The decision's specificity value
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `FALSE`).

## Value

Overall error rate
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md) as a
probability (proportion). A warning is provided for NaN values.

## Details

`comp_err` uses
[`comp_acc`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md)
to compute [`err`](https://hneth.github.io/riskyr/dev/reference/err.md)
as the complement of
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md):

`err = 1 - acc`

See
[`comp_acc`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md)
and [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) for
further details and
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) for other
accuracy metrics and several possible interpretations of accuracy.

## See also

[`comp_acc`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md)
computes overall accuracy
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) from
probabilities;
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) lists all
accuracy metrics;
[`comp_accu_prob`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md)
computes exact accuracy metrics from probabilities;
[`comp_accu_freq`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md)
computes accuracy metrics from frequencies;
[`comp_sens`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md)
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
[`comp_NPV()`](https://hneth.github.io/riskyr/dev/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md),
[`comp_fart()`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/dev/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

Other metrics:
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md),
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md)

## Examples

``` r
# ways to work:
comp_err(.10, .200, .300)  # => err = 0.71
#> [1] 0.71
comp_err(.50, .333, .666)  # => err = 0.5005
#> [1] 0.5005

# watch out for vectors:
prev.range <- seq(0, 1, by = .1)
comp_err(prev.range, .5, .5)  # => 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
#>  [1] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5

# watch out for extreme values:
comp_err(1, 1, 1)  #  => 0
#> [1] 0
comp_err(1, 1, 0)  #  => 0
#> [1] 0

comp_err(1, 0, 1)  #  => 1
#> [1] 1
comp_err(1, 0, 0)  #  => 1
#> [1] 1

comp_err(0, 1, 1)  #  => 0
#> [1] 0
comp_err(0, 1, 0)  #  => 1
#> [1] 1

comp_err(0, 0, 1)  #  => 0
#> [1] 0
comp_err(0, 0, 0)  #  => 1
#> [1] 1

```

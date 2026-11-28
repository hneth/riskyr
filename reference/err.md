# Error rate (err) as the probability of an incorrect decision.

`err` defines the error rate as the complement of accuracy
[`acc`](https://hneth.github.io/riskyr/reference/acc.md) or lack of
correspondence of decisions to conditions.

## Usage

``` r
err
```

## Details

Definition:

`err = (1 - `[`acc`](https://hneth.github.io/riskyr/reference/acc.md)`)`

When [`freq`](https://hneth.github.io/riskyr/reference/freq.md) are not
rounded (`round = FALSE`) then

`err = `[`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md)`/`[`N`](https://hneth.github.io/riskyr/reference/N.md)` = (`[`mi`](https://hneth.github.io/riskyr/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/reference/fa.md)`)/`[`N`](https://hneth.github.io/riskyr/reference/N.md)

`err` is currently not included in
[`prob`](https://hneth.github.io/riskyr/reference/prob.md), but shown in
plots.

See `err`'s complement of accuracy
[`acc`](https://hneth.github.io/riskyr/reference/acc.md) for computation
and [`accu`](https://hneth.github.io/riskyr/reference/accu.md) for
current accuracy metrics and several possible interpretations of
accuracy.

## See also

[`acc`](https://hneth.github.io/riskyr/reference/acc.md) provides
overall accuracy;
[`comp_acc`](https://hneth.github.io/riskyr/reference/comp_acc.md)
computes accuracy from probabilities;
[`accu`](https://hneth.github.io/riskyr/reference/accu.md) lists current
accuracy metrics;
[`comp_accu_prob`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md)
computes exact accuracy metrics from probabilities;
[`comp_accu_freq`](https://hneth.github.io/riskyr/reference/comp_accu_freq.md)
computes accuracy metrics from frequencies;
[`comp_sens`](https://hneth.github.io/riskyr/reference/comp_sens.md) and
[`comp_PPV`](https://hneth.github.io/riskyr/reference/comp_PPV.md)
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

Other probabilities:
[`FDR`](https://hneth.github.io/riskyr/reference/FDR.md),
[`FOR`](https://hneth.github.io/riskyr/reference/FOR.md),
[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md),
[`PPV`](https://hneth.github.io/riskyr/reference/PPV.md),
[`acc`](https://hneth.github.io/riskyr/reference/acc.md),
[`fart`](https://hneth.github.io/riskyr/reference/fart.md),
[`mirt`](https://hneth.github.io/riskyr/reference/mirt.md),
[`ppod`](https://hneth.github.io/riskyr/reference/ppod.md),
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md)

Other metrics: [`acc`](https://hneth.github.io/riskyr/reference/acc.md),
[`accu`](https://hneth.github.io/riskyr/reference/accu.md),
[`comp_acc()`](https://hneth.github.io/riskyr/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md),
[`comp_err()`](https://hneth.github.io/riskyr/reference/comp_err.md)

## Examples

``` r
err <- .50     # sets a rate of incorrect decisions of 50%
err <- 50/100  # (dec_err) for 50 out of 100 individuals
is_prob(err)   # TRUE
#> [1] TRUE
```

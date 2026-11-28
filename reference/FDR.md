# The false detection rate of a decision process or diagnostic procedure.

`FDR` defines a decision's false detection (or false discovery) rate
(`FDR`): The conditional probability of the condition being `FALSE`
provided that the decision is positive.

## Usage

``` r
FDR
```

## Details

Understanding or obtaining the false detection fate or false discovery
rate (`FDR`):

- Definition: `FDR` is the conditional probability for the condition
  being `FALSE` given a positive decision:

  `FDR = p(condition = FALSE | decision = positive)`

- Perspective: `FDR` further classifies the subset of
  [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)
  individuals by condition (`FDR = fa/dec_pos = fa/(hi + fa)`).

- Alternative names: false discovery rate

- Relationships:

  a\. `FDR` is the complement of the positive predictive value
  [`PPV`](https://hneth.github.io/riskyr/reference/PPV.md):

  `FDR = 1 - PPV`

  b\. `FDR` is the opposite conditional probability – but not the
  complement – of the false alarm rate
  [`fart`](https://hneth.github.io/riskyr/reference/fart.md):

  `fart = p(decision = positive | condition = FALSE)`

- In terms of frequencies, `FDR` is the ratio of
  [`fa`](https://hneth.github.io/riskyr/reference/fa.md) divided by
  [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`fa`](https://hneth.github.io/riskyr/reference/fa.md)):

  `FDR = fa/dec_pos = fa/(hi + fa)`

- Dependencies: `FDR` is a feature of a decision process or diagnostic
  procedure and a measure of incorrect decisions (positive decisions
  that are actually `FALSE`).

  However, due to being a conditional probability, the value of `FDR` is
  not intrinsic to the decision process, but also depends on the
  condition's prevalence value
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values)
for additional information.

## See also

[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md)
verifies probabilities.

Other probabilities:
[`FOR`](https://hneth.github.io/riskyr/reference/FOR.md),
[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md),
[`PPV`](https://hneth.github.io/riskyr/reference/PPV.md),
[`acc`](https://hneth.github.io/riskyr/reference/acc.md),
[`err`](https://hneth.github.io/riskyr/reference/err.md),
[`fart`](https://hneth.github.io/riskyr/reference/fart.md),
[`mirt`](https://hneth.github.io/riskyr/reference/mirt.md),
[`ppod`](https://hneth.github.io/riskyr/reference/ppod.md),
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md)

## Examples

``` r
FDR <- .45     # sets a false detection rate (FDR) of 45%
FDR <- 45/100  # (condition = FALSE) for 45 out of 100 people with (decision = positive)
is_prob(FDR)   # TRUE
#> [1] TRUE
```

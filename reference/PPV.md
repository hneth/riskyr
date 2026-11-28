# The positive predictive value of a decision process or diagnostic procedure.

`PPV` defines some decision's positive predictive value (PPV): The
conditional probability of the condition being `TRUE` provided that the
decision is positive.

## Usage

``` r
PPV
```

## Details

Understanding or obtaining the positive predictive value `PPV`:

- Definition: `PPV` is the conditional probability for the condition
  being `TRUE` given a positive decision:

  `PPV = p(condition = TRUE | decision = positive)`

  or the probability of a positive decision being correct.

- Perspective: `PPV` further classifies the subset of
  [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)
  individuals by condition (`PPV = hi/dec_pos = hi/(hi + fa)`).

- Alternative names: `precision`

- Relationships:

  a\. `PPV` is the complement of the false discovery or false detection
  rate [`FDR`](https://hneth.github.io/riskyr/reference/FDR.md):

  `PPV = 1 - FDR`

  b\. `PPV` is the opposite conditional probability – but not the
  complement – of the sensitivity
  [`sens`](https://hneth.github.io/riskyr/reference/sens.md):

  `sens = p(decision = positive | condition = TRUE)`

- In terms of frequencies, `PPV` is the ratio of
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md) divided by
  [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`fa`](https://hneth.github.io/riskyr/reference/fa.md)):

  `PPV = hi/dec_pos = hi/(hi + fa)`

- Dependencies: `PPV` is a feature of a decision process or diagnostic
  procedure and – similar to the sensitivity
  [`sens`](https://hneth.github.io/riskyr/reference/sens.md) – a measure
  of correct decisions (positive decisions that are actually TRUE).

  However, due to being a conditional probability, the value of `PPV` is
  not intrinsic to the decision process, but also depends on the
  condition's prevalence value
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values)
for additional information.

## See also

[`comp_PPV`](https://hneth.github.io/riskyr/reference/comp_PPV.md)
computes `PPV`;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md)
verifies probabilities.

Other probabilities:
[`FDR`](https://hneth.github.io/riskyr/reference/FDR.md),
[`FOR`](https://hneth.github.io/riskyr/reference/FOR.md),
[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md),
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
PPV <- .55     # sets a positive predictive value of 55%
PPV <- 55/100  # (condition = TRUE) for 55 out of 100 people with (decision = positive)
is_prob(PPV)   # TRUE
#> [1] TRUE
```

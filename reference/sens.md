# The sensitivity (or hit rate) of a decision process or diagnostic procedure.

`sens` defines a decision's sensitivity (or hit rate) value: The
conditional probability of the decision being positive if the condition
is `TRUE`.

## Usage

``` r
sens
```

## Details

Understanding or obtaining the sensitivity `sens` (or hit rate `HR`):

- Definition: `sens` is the conditional probability for a (correct)
  positive decision given that the condition is `TRUE`:

  `sens = p(decision = positive | condition = TRUE)`

  or the probability of correctly detecting true cases
  (`condition = TRUE`).

- Perspective: `sens` further classifies the subset of
  [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)
  individuals by decision (`sens = hi/cond_true`).

- Alternative names: true positive rate (`TPR`), hit rate (`HR`),
  probability of detection, `power = 1 - beta`, `recall`

- Relationships:

  a\. `sens` is the complement of the miss rate
  [`mirt`](https://hneth.github.io/riskyr/reference/mirt.md) (aka. false
  negative rate `FNR` or the rate of Type-II errors):

  `sens = (1 - miss rate) = (1 - FNR)`

  b\. `sens` is the opposite conditional probability – but not the
  complement – of the positive predictive value
  [`PPV`](https://hneth.github.io/riskyr/reference/PPV.md):

  `PPV = p(condition = TRUE | decision = positive)`

- In terms of frequencies, `sens` is the ratio of
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md) divided by
  [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)):

  `sens = hi/cond_true = hi/(hi + mi)`

- Dependencies: `sens` is a feature of a decision process or diagnostic
  procedure and a measure of correct decisions (true positives).

  Due to being a conditional probability, the value of `sens` is not
  intrinsic to the decision process, but also depends on the condition's
  prevalence value
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Sensitivity_and_specificity)
for additional information.

## See also

[`comp_sens`](https://hneth.github.io/riskyr/reference/comp_sens.md)
computes `sens` as the complement of
[`mirt`](https://hneth.github.io/riskyr/reference/mirt.md);
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
[`PPV`](https://hneth.github.io/riskyr/reference/PPV.md),
[`acc`](https://hneth.github.io/riskyr/reference/acc.md),
[`err`](https://hneth.github.io/riskyr/reference/err.md),
[`fart`](https://hneth.github.io/riskyr/reference/fart.md),
[`mirt`](https://hneth.github.io/riskyr/reference/mirt.md),
[`ppod`](https://hneth.github.io/riskyr/reference/ppod.md),
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md)

Other essential parameters:
[`cr`](https://hneth.github.io/riskyr/reference/cr.md),
[`fa`](https://hneth.github.io/riskyr/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md),
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md)

## Examples

``` r
sens <- .85     # sets a sensitivity value of 85%
sens <- 85/100  # (decision = positive) for 85 out of 100 people with (condition = TRUE)
is_prob(sens)   # TRUE
#> [1] TRUE
```

# The miss rate of a decision process or diagnostic procedure.

`mirt` defines a decision's miss rate value: The conditional probability
of the decision being negative if the condition is `TRUE`.

## Usage

``` r
mirt
```

## Details

Understanding or obtaining the miss rate `mirt`:

- Definition: `sens` is the conditional probability for an incorrect
  negative decision given that the condition is `TRUE`:

  `mirt = p(decision = negative | condition = TRUE)`

  or the probability of failing to detect true cases
  (`condition = TRUE`).

- Perspective: `mirt` further classifies the subset of
  [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)
  individuals by decision (`mirt = mi/cond_true`).

- Alternative names: false negative rate (`FNR`), rate of type-II errors
  (`beta`)

- Relationships:

  a\. `mirt` is the complement of the sensitivity
  [`sens`](https://hneth.github.io/riskyr/reference/sens.md) (aka. hit
  rate `HR`):

  `mirt = (1 - sens) = (1 - HR)`

  b\. `mirt` is the \_opposite\_ conditional probability – but not the
  complement – of the false omission rate
  [`FOR`](https://hneth.github.io/riskyr/reference/FOR.md):

  `FOR = p(condition = TRUE | decision = negative)`

- In terms of frequencies, `mirt` is the ratio of
  [`mi`](https://hneth.github.io/riskyr/reference/mi.md) divided by
  [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)):

  `mirt = mi/cond_true = mi/(hi + mi)`

- Dependencies: `mirt` is a feature of a decision process or diagnostic
  procedure and a measure of incorrect decisions (false negatives).

  However, due to being a conditional probability, the value of `mirt`
  is not intrinsic to the decision process, but also depends on the
  condition's prevalence value
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Sensitivity_and_specificity)
for additional information.

## See also

[`comp_mirt`](https://hneth.github.io/riskyr/reference/comp_mirt.md)
computes `mirt` as the complement of
[`sens`](https://hneth.github.io/riskyr/reference/sens.md);
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
[`ppod`](https://hneth.github.io/riskyr/reference/ppod.md),
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md)

## Examples

``` r
mirt <- .15     # => sets a miss rate of 15%
mirt <- 15/100  # => (decision = negative) for 15 out of 100 people with (condition = TRUE)
```

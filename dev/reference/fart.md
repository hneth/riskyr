# The false alarm rate (or false positive rate) of a decision process or diagnostic procedure.

`fart` defines a decision's false alarm rate (or the rate of false
positives): The conditional probability of the decision being positive
if the condition is FALSE.

## Usage

``` r
fart
```

## Details

Understanding or obtaining the false alarm rate `fart`:

- Definition: `fart` is the conditional probability for an incorrect
  positive decision given that the condition is `FALSE`:

  `fart = p(decision = positive | condition = FALSE)`

  or the probability of a false alarm.

- Perspective: `fart` further classifies the subset of
  [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
  individuals by decision (`fart = fa/cond_false`).

- Alternative names: false positive rate (`FPR`), rate of type-I errors
  (`alpha`), statistical significance level, `fallout`

- Relationships:

  a\. `fart` is the complement of the specificity
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md):

  `fart = 1 - spec`

  b\. `fart` is the opposite conditional probability – but not the
  complement – of the false discovery rate or false detection rate
  [`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md):

  `FDR = p(condition = FALSE | decision = positive)`

- In terms of frequencies, `fart` is the ratio of
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) divided by
  [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
  (i.e.,
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)):

  `fart = fa/cond_false = fa/(fa + cr)`

- Dependencies: `fart` is a feature of a decision process or diagnostic
  procedure and a measure of incorrect decisions (false positives).

  However, due to being a conditional probability, the value of `fart`
  is not intrinsic to the decision process, but also depends on the
  condition's prevalence value
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Sensitivity_and_specificity)
for additional information.

## See also

[`comp_fart`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md)
computes `fart` as the complement of
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric parameters;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information;
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies probabilities.

Other probabilities:
[`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md),
[`FOR`](https://hneth.github.io/riskyr/dev/reference/FOR.md),
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md),
[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md),
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md),
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md),
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md),
[`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md),
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)

## Examples

``` r
fart <- .25     # sets a false alarm rate of 25%
fart <- 25/100  # (decision = positive) for 25 out of 100 people with (condition = FALSE)
is_prob(fart)   # TRUE
#> [1] TRUE
```

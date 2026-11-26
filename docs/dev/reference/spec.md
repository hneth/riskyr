# The specificity of a decision process or diagnostic procedure.

`spec` defines a decision's specificity value (or correct rejection
rate): The conditional probability of the decision being negative if the
condition is FALSE.

## Usage

``` r
spec
```

## Details

Understanding or obtaining the specificity value `spec`:

- Definition: `spec` is the conditional probability for a (correct)
  negative decision given that the condition is `FALSE`:

  `spec = p(decision = negative | condition = FALSE)`

  or the probability of correctly detecting false cases
  (`condition = FALSE`).

- Perspective: `spec` further classifies the subset of
  [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
  individuals by decision (`spec = cr/cond_false`).

- Alternative names: true negative rate (`TNR`), correct rejection rate,
  `1 - alpha`

- Relationships:

  a\. `spec` is the complement of the false alarm rate
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md):

  `spec = 1 - fart`

  b\. `spec` is the opposite conditional probability – but not the
  complement – of the negative predictive value
  [`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md):

  `NPV = p(condition = FALSE | decision = negative)`

- In terms of frequencies, `spec` is the ratio of
  [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) divided by
  [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
  (i.e.,
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)):

  `spec = cr/cond_false = cr/(fa + cr)`

- Dependencies: `spec` is a feature of a decision process or diagnostic
  procedure and a measure of correct decisions (true negatives).

  However, due to being a conditional probability, the value of `spec`
  is not intrinsic to the decision process, but also depends on the
  condition's prevalence value
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Sensitivity_and_specificity)
for additional information.

## See also

[`comp_spec`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)
computes `spec` as the complement of
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md);
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
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md),
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md),
[`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md),
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md)

Other essential parameters:
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md)

## Examples

``` r
spec <- .75     # sets a specificity value of 75%
spec <- 75/100  # (decision = negative) for 75 out of 100 people with (condition = FALSE)
is_prob(spec)   # TRUE
#> [1] TRUE
```

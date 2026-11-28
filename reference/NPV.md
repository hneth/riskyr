# The negative predictive value of a decision process or diagnostic procedure.

`NPV` defines some decision's negative predictive value (NPV): The
conditional probability of the condition being `FALSE` provided that the
decision is negative.

## Usage

``` r
NPV
```

## Details

Understanding or obtaining the negative predictive value `NPV`:

- Definition: `NPV` is the conditional probability for the condition
  being `FALSE` given a negative decision:

  `NPV = p(condition = FALSE | decision = negative)`

  or the probability of a negative decision being correct.

- Perspective: `NPV` further classifies the subset of
  [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)
  individuals by condition (`NPV = cr/dec_neg = cr/(mi + cr)`).

- Alternative names: true omission rate

- Relationships:

  a\. `NPV` is the complement of the false omission rate
  [`FOR`](https://hneth.github.io/riskyr/reference/FOR.md):

  `NPV = 1 - FOR`

  b\. `NPV` is the opposite conditional probability – but not the
  complement – of the specificity
  [`spec`](https://hneth.github.io/riskyr/reference/spec.md):

  `spec = p(decision = negative | condition = FALSE)`

- In terms of frequencies, `NPV` is the ratio of
  [`cr`](https://hneth.github.io/riskyr/reference/cr.md) divided by
  [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)
  (i.e.,
  [`cr`](https://hneth.github.io/riskyr/reference/cr.md)` + `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)):

  `NPV = cr/dec_neg = cr/(cr + mi)`

- Dependencies: `NPV` is a feature of a decision process or diagnostic
  procedure and – similar to the specificity
  [`spec`](https://hneth.github.io/riskyr/reference/spec.md) – a measure
  of correct decisions (negative decisions that are actually FALSE).

  However, due to being a conditional probability, the value of `NPV` is
  not intrinsic to the decision process, but also depends on the
  condition's prevalence value
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values)
for additional information.

## See also

[`comp_NPV`](https://hneth.github.io/riskyr/reference/comp_NPV.md)
computes `NPV`;
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
NPV <- .95     # sets a negative predictive value of 95%
NPV <- 95/100  # (condition = FALSE) for 95 out of 100 people with (decision = negative)
is_prob(NPV)   # TRUE
#> [1] TRUE
```

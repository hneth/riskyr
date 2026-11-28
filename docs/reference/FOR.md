# The false omission rate (FOR) of a decision process or diagnostic procedure.

`FOR` defines a decision's false omission rate (`FOR`): The conditional
probability of the condition being `TRUE` provided that the decision is
negative.

## Usage

``` r
FOR
```

## Details

Understanding or obtaining the false omission rate `FOR`:

- Definition: `FOR` is the so-called false omission rate: The
  conditional probability for the condition being `TRUE` given a
  negative decision:

  `FOR = p(condition = TRUE | decision = negative)`

- Perspective: `FOR` further classifies the subset of
  [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)
  individuals by condition (`FOR = mi/dec_neg = mi/(mi + cr)`).

- Alternative names: none?

- Relationships:

  a\. `FOR` is the complement of the negative predictive value
  [`NPV`](https://hneth.github.io/riskyr/reference/NPV.md):

  `FOR = 1 - NPV`

  b\. `FOR` is the opposite conditional probability – but not the
  complement – of the miss rate
  [`mirt`](https://hneth.github.io/riskyr/reference/mirt.md) (aka. false
  negative rate `FDR`):

  `mirt = p(decision = negative | condition = TRUE)`

- In terms of frequencies, `FOR` is the ratio of
  [`mi`](https://hneth.github.io/riskyr/reference/mi.md) divided by
  [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)
  (i.e.,
  [`mi`](https://hneth.github.io/riskyr/reference/mi.md)` + `[`cr`](https://hneth.github.io/riskyr/reference/cr.md)):

  `NPV = mi/dec_neg = mi/(mi + cr)`

- Dependencies: `FOR` is a feature of a decision process or diagnostic
  procedure and a measure of incorrect decisions (negative decisions
  that are actually `FALSE`).

  However, due to being a conditional probability, the value of `FOR` is
  not intrinsic to the decision process, but also depends on the
  condition's prevalence value
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values)
for additional information.

## See also

[`comp_FOR`](https://hneth.github.io/riskyr/reference/comp_FOR.md)
computes `FOR` as the complement of
[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md);
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
FOR <- .05     # sets a false omission rate of 5%
FOR <- 5/100   # (condition = TRUE) for 5 out of 100 people with (decision = negative)
is_prob(FOR)   # TRUE
#> [1] TRUE
```

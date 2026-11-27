# The prevalence (baseline probability or prior) of a condition.

`prev` defines a condition's prevalence value (aka. incidence rate or
prior): The baseline probability of the condition being `TRUE`.

## Usage

``` r
prev
```

## Details

Understanding or obtaining the prevalence value `prev`:

- Definition: `prev` is the (non-conditional) probability:

  `prev = p(condition = TRUE)`

  or the base rate (baseline probability or prior) of the condition's
  occurrence or being true.

- In terms of frequencies, `prev` is the ratio of
  [`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md))
  divided by [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md) +
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)):

  `prev = cond_true/N = (hi + mi)/(hi + mi + fa + cr)`

- Perspective: `prev` classifies a population of
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) individuals
  by condition (`prev = cond_true/N`).

  `prev` is the "by condition" counterpart to
  [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md) (when
  adopting a "by decision" perspective) and to
  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) (when
  adopting a "by accuracy" perspective).

- Alternative names: base rate of condition, proportion affected, prior,
  rate of condition `= TRUE` cases.

  In Bayesian contexts, `prev` is typically known as `prior`.

  In medical contexts, `prev` is often distinguished from the *incidence
  rate* (i.e., the rate of new cases within a certain time period).

- Dependencies: `prev` is a feature of the population and of the
  condition, but independent of the decision process or diagnostic
  procedure.

  While the value of `prev` does *not* depend on features of the
  decision process or diagnostic procedure, `prev` must be taken into
  account when computing the conditional probabilities
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
  [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md),
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md),
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md),
  [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md), and
  [`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) (as they
  depend on `prev`).

## References

Consult [Wikipedia](https://en.wikipedia.org/wiki/Prevalence) for
additional information.

## See also

[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric variables;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric variables;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes derived probabilities;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes natural frequencies from probabilities;
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
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)

Other essential parameters:
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)

## Examples

``` r
prev <- .10     # sets a prevalence value of 10%
prev <- 10/100  # (condition = TRUE) for 10 out of 100 individuals
is_prob(prev)   # TRUE
#> [1] TRUE
```

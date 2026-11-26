# The proportion (or baseline) of a positive decision (aka. bias).

`ppod` defines the proportion (baseline probability or rate) of a
decision being `positive` (but not necessarily accurate/correct).

## Usage

``` r
ppod
```

## Details

`ppod` is also known as `bias`, though the latter term is also used to
describe a systematic tendency to deviate in any — rather than just
positive — direction.

Understanding or obtaining the proportion of positive decisions `ppod`:

- Definition: `ppod` is the (non-conditional) probability:

  `ppod = p(decision = positive)`

  or the base rate (or baseline probability) of a decision being
  positive (but not necessarily accurate/correct).

- Perspective: `ppod` classifies a population of
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) individuals
  by decision (`ppod = dec_pos/N`).

  `ppod` is the "by decision" counterpart to
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (which
  adopts a "by condition" perspective).

- Alternative names: base rate of positive decisions (`PR`), proportion
  predicted or diagnosed, rate of decision `= positive` cases

- In terms of frequencies, `ppod` is the ratio of
  [`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md))
  divided by [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md) +
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)):

  `ppod = dec_pos/N = (hi + fa)/(hi + mi + fa + cr)`

- Dependencies: `ppod` is a feature of the decision process or
  diagnostic procedure.

  However, the conditional probabilities
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
  [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md),
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md),
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md),
  [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md), and
  [`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) also
  depend on the condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md).

## References

Consult
[Wikipedia](https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values)
for additional information.

## See also

[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric parameters;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
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
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)

## Examples

``` r
ppod <- .50     # sets a rate of positive decisions of 50%
ppod <- 50/100  # (decision = TRUE) for 50 out of 100 individuals
is_prob(ppod)   # TRUE
#> [1] TRUE
```

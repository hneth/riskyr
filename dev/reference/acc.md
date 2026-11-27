# Accuracy (acc) is the probability of a correct decision.

`acc` defines overall accuracy as the probability of correspondence
between a positive decision and true condition (i.e., the proportion of
correct classification decisions or of
[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
cases).

## Usage

``` r
acc
```

## Details

Importantly, correct decisions
[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md) are
not necessarily positive decisions
[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md).

Understanding or obtaining the accuracy metric `acc`:

- Definition: `acc` is the (non-conditional) probability:

  `acc = p(dec_cor) = dec_cor/N`

  or the base rate (or baseline probability) of a decision being
  correct, but not necessarily positive.

  `acc` values range from 0 (no correct decision/prediction) to 1
  (perfect decision/prediction).

- Computation: `acc` can be computed in several ways:

  \(a\) from
  [`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md):
  `acc = (prev x sens) + [(1 - prev) x spec]`

  \(b\) from
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md):
  `acc = dec_cor/N = (hi + cr)/(hi + mi + fa + cr)`

  \(c\) as complement of the error rate
  [`err`](https://hneth.github.io/riskyr/dev/reference/err.md):
  `acc = 1 - err`

  When frequencies in
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) are not
  rounded, (b) coincides with (a) and (c).

- Perspective: `acc` classifies a population of
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) individuals
  by accuracy/correspondence (`acc = dec_cor/N`).

  `acc` is the "by accuracy" or "by correspondence" counterpart to
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (which
  adopts a "by condition" perspective) and to
  [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md) (which
  adopts a "by decision" perspective).

- Alternative names: base rate of correct decisions, non-erroneous cases

- In terms of frequencies, `acc` is the ratio of
  [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md))
  divided by [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md) +
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)):

  `acc = dec_cor/N = (hi + cr)/(hi + mi + fa + cr)`

- Dependencies: `acc` is a feature of both the environment (true
  condition) and of the decision process or diagnostic procedure. It
  reflects the correspondence of decisions to conditions.

See [`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) for
other accuracy metrics and several possible interpretations of accuracy.

## References

Consult
[Wikipedia:Accuracy_and_precision](https://en.wikipedia.org/wiki/Accuracy_and_precision)
for additional information.

## See also

[`comp_acc`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md)
computes accuracy from probabilities;
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) lists all
accuracy metrics;
[`comp_accu_prob`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md)
computes exact accuracy metrics from probabilities;
[`comp_accu_freq`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md)
computes accuracy metrics from frequencies;
[`comp_sens`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md)
and
[`comp_PPV`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md)
compute related probabilities;
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
verifies extreme cases;
[`comp_complement`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md)
computes a probability's complement;
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)
verifies probability complements;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies probabilities.

Other probabilities:
[`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md),
[`FOR`](https://hneth.github.io/riskyr/dev/reference/FOR.md),
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md),
[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md),
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md),
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md),
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md),
[`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md),
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)

Other metrics:
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md)

## Examples

``` r
acc <- .50     # sets a rate of correct decisions of 50%
acc <- 50/100  # (dec_cor) for 50 out of 100 individuals
is_prob(acc)   # TRUE
#> [1] TRUE
```

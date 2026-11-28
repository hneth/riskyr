# Compute overall accuracy (acc) from probabilities.

`comp_acc` computes overall accuracy
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) from 3
essential probabilities
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md).

## Usage

``` r
comp_acc(prev, sens, spec)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (i.e.,
  the probability of condition being `TRUE`).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `TRUE`).

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `FALSE`).

## Value

Overall accuracy
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) as a
probability (proportion). A warning is provided for NaN values.

See [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) for
definition and
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) for other
accuracy metrics.
[`comp_accu_freq`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md)
and
[`comp_accu_prob`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md)
compute accuracy metrics from frequencies and probabilities.

## Details

`comp_acc` uses probabilities (not frequencies) as inputs and returns an
exact probability (proportion) without rounding.

Understanding the probability
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md):

- Definition:
  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) is the
  (non-conditional) probability:

  `acc = p(dec_cor) = dec_cor/N`

  or the base rate (or baseline probability) of a decision being
  correct, but not necessarily positive.

  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) values
  range from 0 (no correct decision/prediction) to 1 (perfect
  decision/prediction).

- Computation:
  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) can be
  computed in 2 ways:

  \(a\) from
  [`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md):
  `acc = (prev x sens) + [(1 - prev) x spec]`

  \(b\) from
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md):
  `acc = dec_cor/N = (hi + cr)/(hi + mi + fa + cr)`

  When frequencies in
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) are not
  rounded, (b) coincides with (a).

- Perspective:
  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md)
  classifies a population of
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) individuals
  by accuracy/correspondence (`acc = dec_cor/N`).

  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) is the
  "by accuracy" or "by correspondence" counterpart to
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (which
  adopts a "by condition" perspective) and to
  [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md) (which
  adopts a "by decision" perspective).

- Alternative names of
  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md): base
  rate of correct decisions, non-erroneous cases

- In terms of frequencies,
  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) is the
  ratio of
  [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md))
  divided by [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
  (i.e.,
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md) +
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)):

  `acc = dec_cor/N = (hi + cr)/(hi + mi + fa + cr)`

- Dependencies:
  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) is a
  feature of both the environment (true condition) and of the decision
  process or diagnostic procedure. It reflects the correspondence of
  decisions to conditions.

See [`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) for
other accuracy metrics and several possible interpretations of accuracy.

## See also

[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) defines
accuracy as a probability;
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

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/dev/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/dev/reference/comp_FOR.md),
[`comp_NPV()`](https://hneth.github.io/riskyr/dev/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/dev/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

Other metrics:
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md),
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md)

## Examples

``` r
# ways to work:
comp_acc(.10, .200, .300)  # => acc = 0.29
#> [1] 0.29
comp_acc(.50, .333, .666)  # => acc = 0.4995
#> [1] 0.4995

# watch out for vectors:
prev.range <- seq(0, 1, by = .1)
comp_acc(prev.range, .5, .5)  # => 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5
#>  [1] 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5

# watch out for extreme values:
comp_acc(1, 1, 1)  #  => 1
#> [1] 1
comp_acc(1, 1, 0)  #  => 1
#> [1] 1

comp_acc(1, 0, 1)  #  => 0
#> [1] 0
comp_acc(1, 0, 0)  #  => 0
#> [1] 0

comp_acc(0, 1, 1)  #  => 1
#> [1] 1
comp_acc(0, 1, 0)  #  => 0
#> [1] 0

comp_acc(0, 0, 1)  #  => 1
#> [1] 1
comp_acc(0, 0, 0)  #  => 0
#> [1] 0
```

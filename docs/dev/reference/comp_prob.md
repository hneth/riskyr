# Compute probabilities from (3 essential) probabilities.

`comp_prob` computes current probability information from 3 essential
probabilities
([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md),
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)). It
returns a list of 13 key probabilities
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) as its
output.

## Usage

``` r
comp_prob(
  prev = num$prev,
  sens = num$sens,
  mirt = NA,
  spec = num$spec,
  fart = NA,
  tol = 0.01
)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (i.e.,
  the probability of condition being `TRUE`, aka. *prior*).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `TRUE`). `sens` is optional when its complement `mirt` is
  provided.

- mirt:

  The decision's miss rate
  [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `TRUE`). `mirt` is optional when its complement `sens` is
  provided.

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `FALSE`). `spec` is optional when its complement `fart`
  is provided.

- fart:

  The decision's false alarm rate
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `FALSE`). `fart` is optional when its complement `spec`
  is provided.

- tol:

  A numeric tolerance value for
  [`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md).
  Default: `tol = .01`.

## Value

A list [`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md)
containing 13 key probability values.

## Details

`comp_prob` assumes that a sufficient and consistent set of essential
probabilities (i.e.,
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) and
either [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or
its complement
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md), and
either [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or
its complement
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)) is
provided.

`comp_prob` computes and returns a full set of basic and various derived
probabilities (e.g., the probability of a positive decision
[`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md), the
probability of a correct decision
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md), the
predictive values
[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) and
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md), as well as
their complements
[`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md) and
[`FOR`](https://hneth.github.io/riskyr/dev/reference/FOR.md)) in its
output of a list
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md).

Extreme probabilities (sets containing two or more probabilities of 0
or 1) may yield unexpected values (e.g., predictive values
[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) or
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) turning
`NaN` when
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
evaluates to `TRUE`).

`comp_prob` is the probability counterpart to the frequency function
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md).

Key relationships between probabilities and frequencies:

- Three perspectives on a population:

  A population of
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) individuals
  can be split into 2 subsets of frequencies in 3 different ways:

  1.  by condition:

      [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)` + `[`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)

      The frequency
      [`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)
      depends on the prevalence
      [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) and
      the frequency
      [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
      depends on the prevalence's complement
      `1 - `[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md).

  2.  by decision:

      [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)` + `[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)

      The frequency
      [`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)
      depends on the proportion of positive decisions
      [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md) and
      the frequency
      [`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)
      depends on the proportion of negative decisions
      `1 - `[`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md).

  3.  by accuracy (i.e., correspondence of decision to condition):

      [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)` + `[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)

  Each perspective combines 2 pairs of the 4 essential probabilities
  (hi, mi, fa, cr).

  When providing probabilities, the population size
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) is a free
  parameter (independent of the essential probabilities
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md), and
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)).

  If [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) is unknown
  (`NA`), a suitable minimum value can be computed by
  [`comp_min_N`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md).

- Defining probabilities in terms of frequencies:

  Probabilities *are* – determine, describe, or are defined as – the
  relationships between frequencies. Thus, they can be computed as
  ratios between frequencies:

  1.  prevalence
      [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md):

      [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md)` = `[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)`/`[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)`) / (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`)`

  2.  sensitivity
      [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md):

      [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md)` = `[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)`/`[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)` = `[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` / (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)`) = (1 - `[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md)`)`

  3.  miss rate
      [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md):

      [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md)` = `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)`/`[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)` = `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` / (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)`) = (1 - `[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md)`)`

  4.  specificity
      [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md):

      [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)` = `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`/`[`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)` = `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)` / (`[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`) = (1 - `[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)`)`

  5.  false alarm rate
      [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md):

      [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)` = `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)`/`[`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)` = `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` / (`[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`) = (1 - `[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)`)`

  6.  proportion of positive decisions
      [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md):

      [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md)` = `[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)`/`[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)`) / (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`)`

  7.  positive predictive value
      [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md):

      [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md)` = `[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)`/`[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)` = `[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` / (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)`) = (1 - `[`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md)`)`

  8.  negative predictive value
      [`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md):

      [`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md)` = `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`/`[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)` = `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)` / (`[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`) = (1 - `[`FOR`](https://hneth.github.io/riskyr/dev/reference/FOR.md)`)`

  9.  false detection rate
      [`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md):

      [`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md)` = `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)`/`[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)` = `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` / (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)`) = (1 - `[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md)`)`

  10. false omission rate
      [`FOR`](https://hneth.github.io/riskyr/dev/reference/FOR.md):

      [`FOR`](https://hneth.github.io/riskyr/dev/reference/FOR.md)` = `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)`/`[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)` = `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` / (`[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`) = (1 - `[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md)`)`

  11. accuracy
      [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md):

      [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md)` = `[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)`/`[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`) / (`[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`)`

  12. rate of hits, given accuracy `p_acc_hi`:

      `p_acc_hi = `[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)`/`[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)` = (1 - `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)`/`[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)`)`

  13. rate of false alarms, given inaccuracy `p_err_fa`:

      `p_err_fa = `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)`/`[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)` = (1 - `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)`/`[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)`)`

  Note: When frequencies are rounded (by `round = TRUE` in
  [`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)),
  probabilities computed from
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) may
  differ from exact probabilities.

Functions translating between representational formats:
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md),
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
(see documentation of
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
for details).

## See also

[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) contains
current accuracy information;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric parameters;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color information;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) contains
current text information;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes frequencies from probabilities;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)
verifies sets of probability inputs;
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
verifies sets of extreme probabilities;
[`comp_min_N`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md)
computes a suitable minimum population size
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md);
[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
computes current frequency information from (4 essential) frequencies;
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
computes current frequency information from (3 essential) probabilities;
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md)
computes current probability information from (4 essential) frequencies;
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
computes current probability information from (3 essential)
probabilities.

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/dev/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/dev/reference/comp_FOR.md),
[`comp_NPV()`](https://hneth.github.io/riskyr/dev/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/dev/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

## Examples

``` r
# Basics:
comp_prob(prev = .11, sens = .88, spec = .77)                        # => ok: PPV = 0.3210614
#> $prev
#> [1] 0.11
#> 
#> $sens
#> [1] 0.88
#> 
#> $mirt
#> [1] 0.12
#> 
#> $spec
#> [1] 0.77
#> 
#> $fart
#> [1] 0.23
#> 
#> $ppod
#> [1] 0.3015
#> 
#> $PPV
#> [1] 0.3210614
#> 
#> $FDR
#> [1] 0.6789386
#> 
#> $NPV
#> [1] 0.9811024
#> 
#> $FOR
#> [1] 0.01889764
#> 
#> $acc
#> [1] 0.7821
#> 
#> $p_acc_hi
#> [1] 0.1237693
#> 
#> $p_err_fa
#> [1] 0.9394218
#> 
comp_prob(prev = .11, sens = NA, mirt = .12, spec = NA, fart = .23)  # => ok: PPV = 0.3210614
#> $prev
#> [1] 0.11
#> 
#> $sens
#> [1] 0.88
#> 
#> $mirt
#> [1] 0.12
#> 
#> $spec
#> [1] 0.77
#> 
#> $fart
#> [1] 0.23
#> 
#> $ppod
#> [1] 0.3015
#> 
#> $PPV
#> [1] 0.3210614
#> 
#> $FDR
#> [1] 0.6789386
#> 
#> $NPV
#> [1] 0.9811024
#> 
#> $FOR
#> [1] 0.01889764
#> 
#> $acc
#> [1] 0.7821
#> 
#> $p_acc_hi
#> [1] 0.1237693
#> 
#> $p_err_fa
#> [1] 0.9394218
#> 
comp_prob()          # => ok, using current defaults
#> $prev
#> [1] 0.25
#> 
#> $sens
#> [1] 0.85
#> 
#> $mirt
#> [1] 0.15
#> 
#> $spec
#> [1] 0.75
#> 
#> $fart
#> [1] 0.25
#> 
#> $ppod
#> [1] 0.4
#> 
#> $PPV
#> [1] 0.53125
#> 
#> $FDR
#> [1] 0.46875
#> 
#> $NPV
#> [1] 0.9375
#> 
#> $FOR
#> [1] 0.0625
#> 
#> $acc
#> [1] 0.775
#> 
#> $p_acc_hi
#> [1] 0.2741935
#> 
#> $p_err_fa
#> [1] 0.8333333
#> 
length(comp_prob())  # => 13 probabilities
#> [1] 13

# Ways to work:
comp_prob(.99, sens = .99, spec = .99)              # => ok: PPV = 0.999898
#> $prev
#> [1] 0.99
#> 
#> $sens
#> [1] 0.99
#> 
#> $mirt
#> [1] 0.01
#> 
#> $spec
#> [1] 0.99
#> 
#> $fart
#> [1] 0.01
#> 
#> $ppod
#> [1] 0.9802
#> 
#> $PPV
#> [1] 0.999898
#> 
#> $FDR
#> [1] 0.00010202
#> 
#> $NPV
#> [1] 0.5
#> 
#> $FOR
#> [1] 0.5
#> 
#> $acc
#> [1] 0.99
#> 
#> $p_acc_hi
#> [1] 0.99
#> 
#> $p_err_fa
#> [1] 0.01
#> 
comp_prob(.99, sens = .90, spec = NA, fart = .10)   # => ok: PPV = 0.9988789
#> $prev
#> [1] 0.99
#> 
#> $sens
#> [1] 0.9
#> 
#> $mirt
#> [1] 0.1
#> 
#> $spec
#> [1] 0.9
#> 
#> $fart
#> [1] 0.1
#> 
#> $ppod
#> [1] 0.892
#> 
#> $PPV
#> [1] 0.9988789
#> 
#> $FDR
#> [1] 0.001121076
#> 
#> $NPV
#> [1] 0.08333333
#> 
#> $FOR
#> [1] 0.9166667
#> 
#> $acc
#> [1] 0.9
#> 
#> $p_acc_hi
#> [1] 0.99
#> 
#> $p_err_fa
#> [1] 0.01
#> 

# Watch out for extreme cases:
comp_prob(1, sens = 0, spec = 1)      # => ok, but with warnings (as PPV & FDR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 0
#> 
#> $mirt
#> [1] 1
#> 
#> $spec
#> [1] 1
#> 
#> $fart
#> [1] 0
#> 
#> $ppod
#> [1] 0
#> 
#> $PPV
#> [1] NaN
#> 
#> $FDR
#> [1] NaN
#> 
#> $NPV
#> [1] 0
#> 
#> $FOR
#> [1] 1
#> 
#> $acc
#> [1] 0
#> 
#> $p_acc_hi
#> [1] NaN
#> 
#> $p_err_fa
#> [1] 0
#> 
comp_prob(1, sens = 0, spec = 0)      # => ok, but with warnings (as PPV & FDR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 0
#> 
#> $mirt
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $ppod
#> [1] 0
#> 
#> $PPV
#> [1] NaN
#> 
#> $FDR
#> [1] NaN
#> 
#> $NPV
#> [1] 0
#> 
#> $FOR
#> [1] 1
#> 
#> $acc
#> [1] 0
#> 
#> $p_acc_hi
#> [1] NaN
#> 
#> $p_err_fa
#> [1] 0
#> 
comp_prob(1, sens = 0, spec = NA, fart = 0)  # => ok, but with warnings (as PPV & FDR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 0
#> 
#> $mirt
#> [1] 1
#> 
#> $spec
#> [1] 1
#> 
#> $fart
#> [1] 0
#> 
#> $ppod
#> [1] 0
#> 
#> $PPV
#> [1] NaN
#> 
#> $FDR
#> [1] NaN
#> 
#> $NPV
#> [1] 0
#> 
#> $FOR
#> [1] 1
#> 
#> $acc
#> [1] 0
#> 
#> $p_acc_hi
#> [1] NaN
#> 
#> $p_err_fa
#> [1] 0
#> 
comp_prob(1, sens = 0, spec = NA, fart = 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 0
#> 
#> $mirt
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $ppod
#> [1] 0
#> 
#> $PPV
#> [1] NaN
#> 
#> $FDR
#> [1] NaN
#> 
#> $NPV
#> [1] 0
#> 
#> $FOR
#> [1] 1
#> 
#> $acc
#> [1] 0
#> 
#> $p_acc_hi
#> [1] NaN
#> 
#> $p_err_fa
#> [1] 0
#> 

# Watch out for extreme cases:
comp_prob(1, sens = 0, spec = 1)      # => ok, but with warnings (as PPV & FDR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 0
#> 
#> $mirt
#> [1] 1
#> 
#> $spec
#> [1] 1
#> 
#> $fart
#> [1] 0
#> 
#> $ppod
#> [1] 0
#> 
#> $PPV
#> [1] NaN
#> 
#> $FDR
#> [1] NaN
#> 
#> $NPV
#> [1] 0
#> 
#> $FOR
#> [1] 1
#> 
#> $acc
#> [1] 0
#> 
#> $p_acc_hi
#> [1] NaN
#> 
#> $p_err_fa
#> [1] 0
#> 
comp_prob(1, sens = 0, spec = 0)      # => ok, but with warnings (as PPV & FDR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 0
#> 
#> $mirt
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $ppod
#> [1] 0
#> 
#> $PPV
#> [1] NaN
#> 
#> $FDR
#> [1] NaN
#> 
#> $NPV
#> [1] 0
#> 
#> $FOR
#> [1] 1
#> 
#> $acc
#> [1] 0
#> 
#> $p_acc_hi
#> [1] NaN
#> 
#> $p_err_fa
#> [1] 0
#> 
comp_prob(1, sens = 0, spec = NA, fart = 0)  # => ok, but with warnings (as PPV & FDR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 0
#> 
#> $mirt
#> [1] 1
#> 
#> $spec
#> [1] 1
#> 
#> $fart
#> [1] 0
#> 
#> $ppod
#> [1] 0
#> 
#> $PPV
#> [1] NaN
#> 
#> $FDR
#> [1] NaN
#> 
#> $NPV
#> [1] 0
#> 
#> $FOR
#> [1] 1
#> 
#> $acc
#> [1] 0
#> 
#> $p_acc_hi
#> [1] NaN
#> 
#> $p_err_fa
#> [1] 0
#> 
comp_prob(1, sens = 0, spec = NA, fart = 1)  # => ok, but with warnings (as PPV & FDR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 0
#> 
#> $mirt
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $ppod
#> [1] 0
#> 
#> $PPV
#> [1] NaN
#> 
#> $FDR
#> [1] NaN
#> 
#> $NPV
#> [1] 0
#> 
#> $FOR
#> [1] 1
#> 
#> $acc
#> [1] 0
#> 
#> $p_acc_hi
#> [1] NaN
#> 
#> $p_err_fa
#> [1] 0
#> 

comp_prob(1, sens = 1, spec = 0)      # => ok, but with warnings (as NPV & FOR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $mirt
#> [1] 0
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $ppod
#> [1] 1
#> 
#> $PPV
#> [1] 1
#> 
#> $FDR
#> [1] 0
#> 
#> $NPV
#> [1] NaN
#> 
#> $FOR
#> [1] NaN
#> 
#> $acc
#> [1] 1
#> 
#> $p_acc_hi
#> [1] 1
#> 
#> $p_err_fa
#> [1] NaN
#> 
comp_prob(1, sens = 1, spec = 1)      # => ok, but with warnings (as NPV & FOR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $mirt
#> [1] 0
#> 
#> $spec
#> [1] 1
#> 
#> $fart
#> [1] 0
#> 
#> $ppod
#> [1] 1
#> 
#> $PPV
#> [1] 1
#> 
#> $FDR
#> [1] 0
#> 
#> $NPV
#> [1] NaN
#> 
#> $FOR
#> [1] NaN
#> 
#> $acc
#> [1] 1
#> 
#> $p_acc_hi
#> [1] 1
#> 
#> $p_err_fa
#> [1] NaN
#> 
comp_prob(1, sens = 1, spec = NA, fart = 0)  # => ok, but with warnings (as NPV & FOR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $mirt
#> [1] 0
#> 
#> $spec
#> [1] 1
#> 
#> $fart
#> [1] 0
#> 
#> $ppod
#> [1] 1
#> 
#> $PPV
#> [1] 1
#> 
#> $FDR
#> [1] 0
#> 
#> $NPV
#> [1] NaN
#> 
#> $FOR
#> [1] NaN
#> 
#> $acc
#> [1] 1
#> 
#> $p_acc_hi
#> [1] 1
#> 
#> $p_err_fa
#> [1] NaN
#> 
comp_prob(1, sens = 1, spec = NA, fart = 1)  # => ok, but with warnings (as NPV & FOR are NaN)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $mirt
#> [1] 0
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $ppod
#> [1] 1
#> 
#> $PPV
#> [1] 1
#> 
#> $FDR
#> [1] 0
#> 
#> $NPV
#> [1] NaN
#> 
#> $FOR
#> [1] NaN
#> 
#> $acc
#> [1] 1
#> 
#> $p_acc_hi
#> [1] 1
#> 
#> $p_err_fa
#> [1] NaN
#> 

# Ways to fail:
comp_prob(NA, 1, 1, NA)  # => only warning: invalid set (prev not numeric)
#> Warning: Invalid probabilities. Please enter a valid set of essential probabilities.
comp_prob(8,  1, 1, NA)  # => only warning: prev no probability
#> Warning: Invalid probabilities. Please enter a valid set of essential probabilities.
comp_prob(1,  8, 1, NA)  # => only warning: sens no probability
#> Warning: Invalid probabilities. Please enter a valid set of essential probabilities.
comp_prob(1,  1, 1,  1)  # => only warning: is_complement not in tolerated range
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> Warning: Invalid probabilities. Please enter a valid set of essential probabilities.
```

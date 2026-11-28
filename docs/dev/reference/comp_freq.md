# Compute frequencies from (3 essential) probabilities.

`comp_freq` computes frequencies (typically as rounded integers) given 3
basic probabilities –
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) – for a
population of [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
individuals. It returns a list of 11 key frequencies
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) as its
output.

## Usage

``` r
comp_freq(
  prev = num$prev,
  sens = num$sens,
  spec = num$spec,
  N = num$N,
  round = TRUE,
  sample = FALSE
)
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

- N:

  The number of individuals in the population. If
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) is unknown
  (`NA`), a suitable minimum value is computed by
  [`comp_min_N`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md).

- round:

  Boolean value that determines whether frequency values are rounded to
  the nearest integer. Default: `round = TRUE`.

  Note: Removed `n_digits` parameter: Number of digits to which
  frequency values are to be rounded when `round = FALSE`. Default:
  `n_digits = 5`.

- sample:

  Boolean value that determines whether frequency values are sampled
  from `N`, given the probability values of `prev`, `sens`, and `spec`.
  Default: `sample = FALSE`.

  Note: Sampling uses [`sample()`](https://rdrr.io/r/base/sample.html)
  and returns integer values.

## Value

A list [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md)
containing 11 key frequency values.

## Details

In addition to
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md), both
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) are
necessary arguments. If only their complements
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) or
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) are
known, use the wrapper function
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
which also accepts
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) and
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) as inputs
(but requires that the entire set of provided probabilities is
sufficient and consistent). Alternatively, use
[`comp_complement`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
[`comp_comp_pair`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
or
[`comp_complete_prob_set`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md)
to obtain the 3 essential probabilities.

`comp_freq` is the frequency counterpart to the probability function
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md).

By default, `comp_freq` and its wrapper function
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
round frequencies to nearest integers to avoid decimal values in
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) (i.e.,
`round = TRUE` by default). When frequencies are rounded, probabilities
computed from
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) may
differ from exact probabilities. Using the option `round = FALSE` turns
off rounding.

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

  Beware of rounding and sampling issues! If frequencies are rounded (by
  `round = TRUE` in `comp_freq`) or sampled from probabilities (by
  `sample = TRUE`), then any probabilities computed from
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) may
  differ from original and exact probabilities.

Functions translating between representational formats:
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md),
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
(see documentation of
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
for details).

## See also

[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
corresponding wrapper function;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric variables;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric variables;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`comp_complement`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md)
computes a probability's complement;
[`comp_comp_pair`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md)
computes pairs of complements;
[`comp_complete_prob_set`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md)
completes valid sets of probabilities;
[`comp_min_N`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md)
computes a suitable population size
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) (if missing).

Other functions computing frequencies:
[`comp_freq_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md),
[`comp_freq_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_min_N()`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md),
[`comp_prob_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)

## Examples

``` r
comp_freq()          # ok, using current defaults
#> $N
#> [1] 1000
#> 
#> $cond_true
#> [1] 250
#> 
#> $cond_false
#> [1] 750
#> 
#> $dec_pos
#> [1] 400
#> 
#> $dec_neg
#> [1] 600
#> 
#> $dec_cor
#> [1] 774
#> 
#> $dec_err
#> [1] 226
#> 
#> $hi
#> [1] 212
#> 
#> $mi
#> [1] 38
#> 
#> $fa
#> [1] 188
#> 
#> $cr
#> [1] 562
#> 
length(comp_freq())  # 11 key frequencies
#> [1] 11

# Rounding:
comp_freq(prev = .5, sens = .5, spec = .5, N = 1)   # yields fa = 1 (see ?round for reason)
#> $N
#> [1] 1
#> 
#> $cond_true
#> [1] 0
#> 
#> $cond_false
#> [1] 1
#> 
#> $dec_pos
#> [1] 1
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 1
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 1
#> 
#> $cr
#> [1] 0
#> 
comp_freq(prev = .1, sens = .9, spec = .8, N = 10)  # 1 hit (TP, rounded)
#> $N
#> [1] 10
#> 
#> $cond_true
#> [1] 1
#> 
#> $cond_false
#> [1] 9
#> 
#> $dec_pos
#> [1] 3
#> 
#> $dec_neg
#> [1] 7
#> 
#> $dec_cor
#> [1] 8
#> 
#> $dec_err
#> [1] 2
#> 
#> $hi
#> [1] 1
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 2
#> 
#> $cr
#> [1] 7
#> 
comp_freq(prev = .1, sens = .9, spec = .8, N = 10, round = FALSE)    # hi = .9
#> $N
#> [1] 10
#> 
#> $cond_true
#> [1] 1
#> 
#> $cond_false
#> [1] 9
#> 
#> $dec_pos
#> [1] 2.7
#> 
#> $dec_neg
#> [1] 7.3
#> 
#> $dec_cor
#> [1] 8.1
#> 
#> $dec_err
#> [1] 1.9
#> 
#> $hi
#> [1] 0.9
#> 
#> $mi
#> [1] 0.1
#> 
#> $fa
#> [1] 1.8
#> 
#> $cr
#> [1] 7.2
#> 
comp_freq(prev = 1/3, sens = 6/7, spec = 2/3, N = 1, round = FALSE)  # hi = 0.2857143
#> $N
#> [1] 1
#> 
#> $cond_true
#> [1] 0.3333333
#> 
#> $cond_false
#> [1] 0.6666667
#> 
#> $dec_pos
#> [1] 0.5079365
#> 
#> $dec_neg
#> [1] 0.4920635
#> 
#> $dec_cor
#> [1] 0.7301587
#> 
#> $dec_err
#> [1] 0.2698413
#> 
#> $hi
#> [1] 0.2857143
#> 
#> $mi
#> [1] 0.04761905
#> 
#> $fa
#> [1] 0.2222222
#> 
#> $cr
#> [1] 0.4444444
#> 

# Sampling (from probabilistic description):
comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 100, sample = TRUE)  # freq values vary
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 44
#> 
#> $cond_false
#> [1] 56
#> 
#> $dec_pos
#> [1] 54
#> 
#> $dec_neg
#> [1] 46
#> 
#> $dec_cor
#> [1] 60
#> 
#> $dec_err
#> [1] 40
#> 
#> $hi
#> [1] 29
#> 
#> $mi
#> [1] 15
#> 
#> $fa
#> [1] 25
#> 
#> $cr
#> [1] 31
#> 

# Extreme cases:
comp_freq(prev = 1, sens = 1, spec = 1, 100)  # ok, N hits (TP)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 100
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 100
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 100
#> 
#> $dec_err
#> [1] 0
#> 
#> $hi
#> [1] 100
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 0
#> 
comp_freq(prev = 1, sens = 1, spec = 0, 100)  # ok, N hits
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 100
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 100
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 100
#> 
#> $dec_err
#> [1] 0
#> 
#> $hi
#> [1] 100
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 0
#> 
comp_freq(prev = 1, sens = 0, spec = 1, 100)  # ok, N misses (FN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 100
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 0
#> 
#> $dec_neg
#> [1] 100
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 100
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 100
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 0
#> 
comp_freq(prev = 1, sens = 0, spec = 0, 100)  # ok, N misses
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 100
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 0
#> 
#> $dec_neg
#> [1] 100
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 100
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 100
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 0
#> 
comp_freq(prev = 0, sens = 1, spec = 1, 100)  # ok, N correct rejections (TN)
#> Warning: Extreme case (prev = 0 & spec = 1):
#>   N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 0
#> 
#> $cond_false
#> [1] 100
#> 
#> $dec_pos
#> [1] 0
#> 
#> $dec_neg
#> [1] 100
#> 
#> $dec_cor
#> [1] 100
#> 
#> $dec_err
#> [1] 0
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 100
#> 
comp_freq(prev = 0, sens = 1, spec = 0, 100)  # ok, N false alarms (FP)
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 0
#> 
#> $cond_false
#> [1] 100
#> 
#> $dec_pos
#> [1] 100
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 100
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 100
#> 
#> $cr
#> [1] 0
#> 

# Watch out for:
comp_freq(prev = 1, sens = 1, spec = 1, N = NA)  # ok, but warning that N = 1 was computed
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> Warning: Unknown population size N. A suitable minimum value of N = 1 was computed.
#> $N
#> [1] 1
#> 
#> $cond_true
#> [1] 1
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 1
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 1
#> 
#> $dec_err
#> [1] 0
#> 
#> $hi
#> [1] 1
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 0
#> 
comp_freq(prev = 1, sens = 1, spec = 1, N =  0)  # ok, but all 0 + warning (extreme case: N hits)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $N
#> [1] 0
#> 
#> $cond_true
#> [1] 0
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 0
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 0
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 0
#> 
comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = TRUE)   # ok, rounded (see mi and fa)
#> $N
#> [1] 10
#> 
#> $cond_true
#> [1] 5
#> 
#> $cond_false
#> [1] 5
#> 
#> $dec_pos
#> [1] 5
#> 
#> $dec_neg
#> [1] 5
#> 
#> $dec_cor
#> [1] 4
#> 
#> $dec_err
#> [1] 6
#> 
#> $hi
#> [1] 2
#> 
#> $mi
#> [1] 3
#> 
#> $fa
#> [1] 3
#> 
#> $cr
#> [1] 2
#> 
comp_freq(prev = .5, sens = .5, spec = .5, N = 10, round = FALSE)  # ok, not rounded
#> $N
#> [1] 10
#> 
#> $cond_true
#> [1] 5
#> 
#> $cond_false
#> [1] 5
#> 
#> $dec_pos
#> [1] 5
#> 
#> $dec_neg
#> [1] 5
#> 
#> $dec_cor
#> [1] 5
#> 
#> $dec_err
#> [1] 5
#> 
#> $hi
#> [1] 2.5
#> 
#> $mi
#> [1] 2.5
#> 
#> $fa
#> [1] 2.5
#> 
#> $cr
#> [1] 2.5
#> 

# Ways to fail:
comp_freq(prev = NA,  sens = 1, spec = 1,  100)   # NAs + warning (prev NA)
#> $N
#> [1] NA
#> 
#> $cond_true
#> [1] NA
#> 
#> $cond_false
#> [1] NA
#> 
#> $dec_pos
#> [1] NA
#> 
#> $dec_neg
#> [1] NA
#> 
#> $dec_cor
#> [1] NA
#> 
#> $dec_err
#> [1] NA
#> 
#> $hi
#> [1] NA
#> 
#> $mi
#> [1] NA
#> 
#> $fa
#> [1] NA
#> 
#> $cr
#> [1] NA
#> 
comp_freq(prev = 1,  sens = NA, spec = 1,  100)   # NAs + warning (sens NA)
#> $N
#> [1] NA
#> 
#> $cond_true
#> [1] NA
#> 
#> $cond_false
#> [1] NA
#> 
#> $dec_pos
#> [1] NA
#> 
#> $dec_neg
#> [1] NA
#> 
#> $dec_cor
#> [1] NA
#> 
#> $dec_err
#> [1] NA
#> 
#> $hi
#> [1] NA
#> 
#> $mi
#> [1] NA
#> 
#> $fa
#> [1] NA
#> 
#> $cr
#> [1] NA
#> 
comp_freq(prev = 1,  sens = 1,  spec = NA, 100)   # NAs + warning (spec NA)
#> $N
#> [1] NA
#> 
#> $cond_true
#> [1] NA
#> 
#> $cond_false
#> [1] NA
#> 
#> $dec_pos
#> [1] NA
#> 
#> $dec_neg
#> [1] NA
#> 
#> $dec_cor
#> [1] NA
#> 
#> $dec_err
#> [1] NA
#> 
#> $hi
#> [1] NA
#> 
#> $mi
#> [1] NA
#> 
#> $fa
#> [1] NA
#> 
#> $cr
#> [1] NA
#> 
comp_freq(prev = 8,  sens = 1,  spec = 1,  100)   # NAs + warning (prev beyond range)
#> $N
#> [1] NA
#> 
#> $cond_true
#> [1] NA
#> 
#> $cond_false
#> [1] NA
#> 
#> $dec_pos
#> [1] NA
#> 
#> $dec_neg
#> [1] NA
#> 
#> $dec_cor
#> [1] NA
#> 
#> $dec_err
#> [1] NA
#> 
#> $hi
#> [1] NA
#> 
#> $mi
#> [1] NA
#> 
#> $fa
#> [1] NA
#> 
#> $cr
#> [1] NA
#> 
comp_freq(prev = 1,  sens = 8,  spec = 1,  100)   # NAs + warning (sens beyond range)
#> $N
#> [1] NA
#> 
#> $cond_true
#> [1] NA
#> 
#> $cond_false
#> [1] NA
#> 
#> $dec_pos
#> [1] NA
#> 
#> $dec_neg
#> [1] NA
#> 
#> $dec_cor
#> [1] NA
#> 
#> $dec_err
#> [1] NA
#> 
#> $hi
#> [1] NA
#> 
#> $mi
#> [1] NA
#> 
#> $fa
#> [1] NA
#> 
#> $cr
#> [1] NA
#> 
```

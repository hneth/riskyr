# Compute frequencies from (3 essential) probabilities.

`comp_freq_prob` computes frequency information from a sufficient and
valid set of 3 essential probabilities
([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md), and
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or its
complement
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or its
complement
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)). It
returns a list of 11 key frequencies
([`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md)) as its
output.

## Usage

``` r
comp_freq_prob(
  prev = prob$prev,
  sens = prob$sens,
  mirt = NA,
  spec = prob$spec,
  fart = NA,
  tol = 0.01,
  N = freq$N,
  round = TRUE,
  sample = FALSE
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

- N:

  The number of individuals in the population. If
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) is unknown
  (`NA`), a suitable minimum value is computed by
  [`comp_min_N`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md).

- round:

  A Boolean value that determines whether frequencies are rounded to the
  nearest integer. Default: `round = TRUE`.

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

`comp_freq_prob` is a wrapper function for the more basic function
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md),
which only accepts 3 essential probabilities (i.e.,
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)) as
inputs.

Defaults and constraints:

- Initial values:

  By default, the values of
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md), and
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) are
  initialized to the probability information currently contained in
  [`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md).

  Similarly, the population size
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) uses the
  frequency information currently contained in
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) as its
  default. If [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
  is unknown (`NA`), a suitable minimum value is computed by
  [`comp_min_N`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md).

- Constraints:

  When using `comp_freq_prob` with the arguments
  [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) and
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md), their
  complements
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) and
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) must
  either be valid complements (as in
  [`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md))
  or set to `NA`.

  In addition to
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md), both
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) and
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) are
  necessary arguments. If only their complements
  [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) or
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) are
  known, first use
  [`comp_complement`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
  [`comp_comp_pair`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
  or
  [`comp_complete_prob_set`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md)
  to compute the 3 essential probabilities.

- Rounding:

  By default, `comp_freq_prob` and its basic function
  [`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
  round frequencies to nearest integers to avoid decimal values in
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) (i.e.,
  `round = TRUE` by default).

  When frequencies are rounded, probabilities computed from
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) may
  differ from exact probabilities.

  Using the option `round = FALSE` turns off rounding.

Key relationships between frequencies and probabilities (see
documentation of
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
or
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
for details):

- Three perspectives on a population:

  by condition / by decision / by accuracy.

- Defining probabilities in terms of frequencies:

  Probabilities can be computed as ratios between frequencies, but
  beware of rounding and sampling issues!

Functions translating between representational formats:
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md),
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
`comp_freq_prob`,
[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
(see documentation of
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
for details).

## See also

[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
computes current frequency information from (4 essential) frequencies;
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md)
computes current probability information from (4 essential) frequencies;
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
computes current probability information from (3 essential)
probabilities;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric variables;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric variables;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information;
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
[`comp_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md),
[`comp_freq_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md),
[`comp_min_N()`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md),
[`comp_prob_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)

Other format conversion functions:
[`comp_freq_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_prob_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)

## Examples

``` r
# Basics:
comp_freq_prob(prev = .1, sens = .9, spec = .8, N = 100)  # ok: hi = 9, ... cr = 72.
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 10
#> 
#> $cond_false
#> [1] 90
#> 
#> $dec_pos
#> [1] 27
#> 
#> $dec_neg
#> [1] 73
#> 
#> $dec_cor
#> [1] 81
#> 
#> $dec_err
#> [1] 19
#> 
#> $hi
#> [1] 9
#> 
#> $mi
#> [1] 1
#> 
#> $fa
#> [1] 18
#> 
#> $cr
#> [1] 72
#> 
# Same case with complements (using NAs to prevent defaults):
comp_freq_prob(prev = .1, sens = NA, mirt = .1, spec = NA, fart = .2, N = 100)  # same result
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 10
#> 
#> $cond_false
#> [1] 90
#> 
#> $dec_pos
#> [1] 27
#> 
#> $dec_neg
#> [1] 73
#> 
#> $dec_cor
#> [1] 81
#> 
#> $dec_err
#> [1] 19
#> 
#> $hi
#> [1] 9
#> 
#> $mi
#> [1] 1
#> 
#> $fa
#> [1] 18
#> 
#> $cr
#> [1] 72
#> 

comp_freq_prob()                   # ok, using probability info currently contained in prob
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
length(comp_freq_prob())           # list of 11 key frequencies
#> [1] 11
all.equal(freq, comp_freq_prob())  # TRUE, unless prob has been changed after computing freq
#> [1] TRUE
freq <- comp_freq_prob()           # computes frequencies and stores them in freq

# Ways to work:
comp_freq_prob(prev = 1, sens = 1, spec = 1, N = 101)  # ok + warning: N hits (TP)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $N
#> [1] 101
#> 
#> $cond_true
#> [1] 101
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 101
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 101
#> 
#> $dec_err
#> [1] 0
#> 
#> $hi
#> [1] 101
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

# Same case with complements (note NAs to prevent default arguments):
comp_freq_prob(prev = 1, sens = NA, mirt = 0, spec = NA, fart = 0, N = 101)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $N
#> [1] 101
#> 
#> $cond_true
#> [1] 101
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 101
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 101
#> 
#> $dec_err
#> [1] 0
#> 
#> $hi
#> [1] 101
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

comp_freq_prob(prev = 1, sens = 1, spec = 0, N = 102)  # ok + warning: N hits (TP)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $N
#> [1] 102
#> 
#> $cond_true
#> [1] 102
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 102
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 102
#> 
#> $dec_err
#> [1] 0
#> 
#> $hi
#> [1] 102
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
comp_freq_prob(prev = 1, sens = 0, spec = 1, N = 103)  # ok + warning: N misses (FN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> $N
#> [1] 103
#> 
#> $cond_true
#> [1] 103
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 0
#> 
#> $dec_neg
#> [1] 103
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 103
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 103
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 0
#> 
comp_freq_prob(prev = 1, sens = 0, spec = 0, N = 104)  # ok + warning: N misses (FN)
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> $N
#> [1] 104
#> 
#> $cond_true
#> [1] 104
#> 
#> $cond_false
#> [1] 0
#> 
#> $dec_pos
#> [1] 0
#> 
#> $dec_neg
#> [1] 104
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 104
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 104
#> 
#> $fa
#> [1] 0
#> 
#> $cr
#> [1] 0
#> 
comp_freq_prob(prev = 0, sens = 1, spec = 1, N = 105)  # ok + warning: N correct rejections (TN)
#> Warning: Extreme case (prev = 0 & spec = 1):
#>   N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.
#> $N
#> [1] 105
#> 
#> $cond_true
#> [1] 0
#> 
#> $cond_false
#> [1] 105
#> 
#> $dec_pos
#> [1] 0
#> 
#> $dec_neg
#> [1] 105
#> 
#> $dec_cor
#> [1] 105
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
#> [1] 105
#> 

comp_freq_prob(prev = 0, sens = 1, spec = 0, N = 106)  # ok + warning: N false alarms (FP)
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> $N
#> [1] 106
#> 
#> $cond_true
#> [1] 0
#> 
#> $cond_false
#> [1] 106
#> 
#> $dec_pos
#> [1] 106
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 106
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 106
#> 
#> $cr
#> [1] 0
#> 
# Same case with complements (using NAs to prevent defaults):
comp_freq_prob(prev = 0, sens = NA, mirt = 0,
               spec = NA, fart = 1, N = 106)  # ok + warning: N false alarms (FP)
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> $N
#> [1] 106
#> 
#> $cond_true
#> [1] 0
#> 
#> $cond_false
#> [1] 106
#> 
#> $dec_pos
#> [1] 106
#> 
#> $dec_neg
#> [1] 0
#> 
#> $dec_cor
#> [1] 0
#> 
#> $dec_err
#> [1] 106
#> 
#> $hi
#> [1] 0
#> 
#> $mi
#> [1] 0
#> 
#> $fa
#> [1] 106
#> 
#> $cr
#> [1] 0
#> 

# Rounding:
comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 1)   # yields fa = 1 (see ?round for reason)
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
comp_freq_prob(prev = .1, sens = .9, spec = .8, N = 10)  # 1 hit (TP, rounded)
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
comp_freq_prob(prev = .1, sens = .9, spec = .8, N = 10, round = FALSE)  # hi = .9
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

# Sampling (from probabilistic description):
comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 100, sample = TRUE)  # freq values vary
#> $N
#> [1] 100
#> 
#> $cond_true
#> [1] 56
#> 
#> $cond_false
#> [1] 44
#> 
#> $dec_pos
#> [1] 48
#> 
#> $dec_neg
#> [1] 52
#> 
#> $dec_cor
#> [1] 58
#> 
#> $dec_err
#> [1] 42
#> 
#> $hi
#> [1] 31
#> 
#> $mi
#> [1] 25
#> 
#> $fa
#> [1] 17
#> 
#> $cr
#> [1] 27
#> 

# Watch out for:
comp_freq_prob(prev = 1, sens = 1, spec = 1, N = NA)  # ok + warning: N = 1 computed
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
comp_freq_prob(prev = 1, sens = 1, spec = 1, N =  0)  # ok, but all 0 + warning (NPV = NaN)
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
comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 10, round = TRUE)  # ok, but all rounded
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
comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 10, round = FALSE) # ok, but not rounded
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
comp_freq_prob(prev = NA, sens = 1, spec = 1, 100)  # NAs + no warning (prev NA)
#> Warning: Please enter a valid set of essential probabilities.
comp_freq_prob(prev = 1, sens = NA, spec = 1, 100)  # NAs + no warning (sens NA)
#> Warning: Please enter a valid set of essential probabilities.
comp_freq_prob(prev = 1, sens = 1, spec = NA, 100)  # NAs + no warning (spec NA)
#> Warning: Please enter a valid set of essential probabilities.
comp_freq_prob(prev = 8, sens = 1, spec = 1,  100)  # NAs + warning (prev beyond range)
#> Warning: Please enter a valid set of essential probabilities.
comp_freq_prob(prev = 1, sens = 8, spec = 1,  100)  # NAs + warning (sens & spec beyond range)
#> Warning: Please enter a valid set of essential probabilities.
```

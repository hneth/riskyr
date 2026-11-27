# Compute probabilities from (3 essential) probabilities.

`comp_prob_prob` computes current probability information from a
sufficient and valid set of 3 essential probabilities
([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md), and
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or its
complement
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or its
complement
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)). It
returns a list of 13 key probabilities
([`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md)) as its
output.

## Usage

``` r
comp_prob_prob(
  prev = prob$prev,
  sens = prob$sens,
  mirt = NA,
  spec = prob$spec,
  fart = NA,
  tol = 0.01
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

`comp_prob_prob` is a wrapper function for the more basic function
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md).

Extreme probabilities (sets containing 2 or more probabilities of 0
or 1) may yield unexpected values (e.g., predictive values
[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) or
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) turning
`NaN` when
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
evaluates to `TRUE`).

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
  beware of rounding issues.

Functions translating between representational formats:

1.  `comp_prob_prob` (defined here) is a wrapper function for
    [`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
    and an analog to 3 other format conversion functions:

2.  [`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md)
    computes current *probability* information contained in
    [`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) from
    4 essential frequencies
    ([`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
    [`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
    [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
    [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)).

3.  [`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
    computes current *frequency* information contained in
    [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) from
    3 essential probabilities
    ([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
    [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
    [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)).

4.  [`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
    computes current *frequency* information contained in
    [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) from
    4 essential frequencies
    ([`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
    [`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
    [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
    [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)).

## See also

[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
computes current frequency information from (3 essential) probabilities;
[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
computes current frequency information from (4 essential) frequencies;
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md)
computes current probability information from (4 essential) frequencies;
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
[`comp_freq_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_min_N()`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md)

Other format conversion functions:
[`comp_freq_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md),
[`comp_freq_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md)

## Examples

``` r
# Basics:
comp_prob_prob(prev = .11, sens = .88, spec = .77)                        # ok: PPV = 0.3210614
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
comp_prob_prob(prev = .11, sens = NA, mirt = .12, spec = NA, fart = .23)  # ok: PPV = 0.3210614
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
comp_prob_prob()          # ok, using current defaults
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
length(comp_prob_prob())  # 13 key probability values
#> [1] 13

# Ways to work:
comp_prob_prob(.99, sens = .99, spec = .99)              # ok: PPV = 0.999898
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
comp_prob_prob(.99, sens = .90, spec = NA, fart = .10)   # ok: PPV = 0.9988789
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
comp_prob_prob(1, sens = 0, spec = 1)      # ok, but with warnings (as PPV & FDR are NaN)
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
comp_prob_prob(1, sens = 0, spec = 0)      # ok, but with warnings (as PPV & FDR are NaN)
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
comp_prob_prob(1, sens = 0, spec = NA, fart = 0)  # ok, but with warnings (as PPV & FDR are NaN)
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
comp_prob_prob(1, sens = 0, spec = NA, fart = 1)  # ok, but with warnings (as PPV & FDR are NaN)
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

comp_prob_prob(1, sens = 1, spec = 0)      # ok, but with warnings (as NPV & FOR are NaN)
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
comp_prob_prob(1, sens = 1, spec = 1)      # ok, but with warnings (as NPV & FOR are NaN)
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
comp_prob_prob(1, sens = 1, spec = NA, fart = 0)  # ok, but with warnings (as NPV & FOR are NaN)
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
comp_prob_prob(1, sens = 1, spec = NA, fart = 1)  # ok, but with warnings (as NPV & FOR are NaN)
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
comp_prob_prob(NA, 1, 1, NA)  # only warning: invalid set (prev not numeric)
#> Warning: Invalid probabilities. Please enter a valid set of essential probabilities.
#> [1] "Invalid probabilities. Please enter a valid set of essential probabilities."
comp_prob_prob(8,  1, 1, NA)  # only warning: prev no probability
#> Warning: Invalid probabilities. Please enter a valid set of essential probabilities.
#> [1] "Invalid probabilities. Please enter a valid set of essential probabilities."
comp_prob_prob(1,  8, 1, NA)  # only warning: sens no probability
#> Warning: Invalid probabilities. Please enter a valid set of essential probabilities.
#> [1] "Invalid probabilities. Please enter a valid set of essential probabilities."
comp_prob_prob(1,  1, 1,  1)  # only warning: is_complement not in tolerated range
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> Warning: Invalid probabilities. Please enter a valid set of essential probabilities.
#> [1] "Invalid probabilities. Please enter a valid set of essential probabilities."
```

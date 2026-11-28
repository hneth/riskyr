# Initialize basic numeric variables.

`init_num` initializes basic numeric variables to define
[`num`](https://hneth.github.io/riskyr/reference/num.md) as a list of
named elements containing four basic probabilities
([`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md), and
[`fart`](https://hneth.github.io/riskyr/reference/fart.md)) and one
frequency parameter (the population size
[`N`](https://hneth.github.io/riskyr/reference/N.md)).

## Usage

``` r
init_num(
  prev = num.def$prev,
  sens = num.def$sens,
  spec = num.def$spec,
  fart = num.def$fart,
  N = num.def$N
)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md) (i.e., the
  probability of condition being `TRUE`, aka. *prior*).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/reference/sens.md) (i.e., the
  conditional probability of a positive decision provided that the
  condition is `TRUE`).

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/reference/spec.md) (i.e., the
  conditional probability of a negative decision provided that the
  condition is `FALSE`). `spec` is optional when its complement `fart`
  is provided.

- fart:

  The decision's false alarm rate
  [`fart`](https://hneth.github.io/riskyr/reference/fart.md) (i.e., the
  conditional probability of a positive decision provided that the
  condition is `FALSE`). `fart` is optional when its complement `spec`
  is provided.

- N:

  The population size
  [`N`](https://hneth.github.io/riskyr/reference/N.md).

## Value

A list containing a valid quadruple of probabilities
([`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md), and
[`fart`](https://hneth.github.io/riskyr/reference/fart.md)) and one
frequency (population size
[`N`](https://hneth.github.io/riskyr/reference/N.md)).

## Details

If [`spec`](https://hneth.github.io/riskyr/reference/spec.md) is
provided, its complement
[`fart`](https://hneth.github.io/riskyr/reference/fart.md) is optional.
If [`fart`](https://hneth.github.io/riskyr/reference/fart.md) is
provided, its complement
[`spec`](https://hneth.github.io/riskyr/reference/spec.md) is optional.
If no [`N`](https://hneth.github.io/riskyr/reference/N.md) is provided,
a suitable minimum value is computed by
[`comp_min_N`](https://hneth.github.io/riskyr/reference/comp_min_N.md).

## See also

[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`pal`](https://hneth.github.io/riskyr/reference/pal.md) contains
current color settings;
[`txt`](https://hneth.github.io/riskyr/reference/txt.md) contains
current text settings;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes frequencies from probabilities;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md)
verifies sets of probability inputs;
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/reference/is_extreme_prob_set.md)
verifies sets of extreme probabilities;
[`comp_min_N`](https://hneth.github.io/riskyr/reference/comp_min_N.md)
computes a suitable minimum population size
[`N`](https://hneth.github.io/riskyr/reference/N.md).

Other functions initializing scenario information:
[`init_pal()`](https://hneth.github.io/riskyr/reference/init_pal.md),
[`init_txt()`](https://hneth.github.io/riskyr/reference/init_txt.md),
[`riskyr()`](https://hneth.github.io/riskyr/reference/riskyr.md)

## Examples

``` r
# ways to succeed:
init_num(1, 1, 1, 0, 100)  # => succeeds
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $spec
#> [1] 1
#> 
#> $fart
#> [1] 0
#> 
#> $N
#> [1] 100
#> 
init_num(1, 1, 0, 1, 100)  # => succeeds
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $N
#> [1] 100
#> 

# watch out for:
init_num(1, 1, 0, 1)           # => succeeds (with N computed)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $N
#> [1] 1000
#> 
init_num(1, 1, NA, 1, 100)     # => succeeds (with spec computed)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $N
#> [1] 100
#> 
init_num(1, 1, 0, NA, 100)     # => succeeds (with fart computed)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $N
#> [1] 100
#> 
init_num(1, 1, NA, 1)          # => succeeds (with spec and N computed)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $N
#> [1] 1000
#> 
init_num(1, 1, 0, NA)          # => succeeds (with fart and N computed)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $spec
#> [1] 0
#> 
#> $fart
#> [1] 1
#> 
#> $N
#> [1] 1000
#> 
init_num(1, 1, .51, .50, 100)  # => succeeds (as spec and fart are within tolarated range)
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> $prev
#> [1] 1
#> 
#> $sens
#> [1] 1
#> 
#> $spec
#> [1] 0.51
#> 
#> $fart
#> [1] 0.5
#> 
#> $N
#> [1] 100
#> 

# ways to fail:
init_num(prev = NA)                                  # => NAs + warning (NA)
#> $prev
#> [1] NA
#> 
#> $sens
#> [1] NA
#> 
#> $spec
#> [1] NA
#> 
#> $fart
#> [1] NA
#> 
#> $N
#> [1] NA
#> 
init_num(prev = 88)                                  # => NAs + warning (beyond range)
#> $prev
#> [1] NA
#> 
#> $sens
#> [1] NA
#> 
#> $spec
#> [1] NA
#> 
#> $fart
#> [1] NA
#> 
#> $N
#> [1] NA
#> 
init_num(prev =  1, sens = NA)                       # => NAs + warning (NA)
#> $prev
#> [1] NA
#> 
#> $sens
#> [1] NA
#> 
#> $spec
#> [1] NA
#> 
#> $fart
#> [1] NA
#> 
#> $N
#> [1] NA
#> 
init_num(prev =  1, sens = 1, spec = NA, fart = NA)  # => NAs + warning (NAs)
#> $prev
#> [1] NA
#> 
#> $sens
#> [1] NA
#> 
#> $spec
#> [1] NA
#> 
#> $fart
#> [1] NA
#> 
#> $N
#> [1] NA
#> 
init_num(1, 1, .52, .50, 100)   # => NAs + warning (complements beyond range)
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> $prev
#> [1] NA
#> 
#> $sens
#> [1] NA
#> 
#> $spec
#> [1] NA
#> 
#> $fart
#> [1] NA
#> 
#> $N
#> [1] NA
#> 
```

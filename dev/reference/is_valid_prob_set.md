# Verify that a set of probability inputs is valid.

`is_valid_prob_set` is a function that verifies that a set of (3 to 5)
numeric inputs can be interpreted as a valid set of (3 essential and 2
optional) probabilities.

## Usage

``` r
is_valid_prob_set(prev, sens = NA, mirt = NA, spec = NA, fart = NA, tol = 0.01)
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

  A numeric tolerance value used by
  [`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md).

## Value

A Boolean value: `TRUE` if the probabilities provided are valid;
otherwise `FALSE`.

## Details

`is_valid_prob_set` is a wrapper function that combines
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md),
[`is_suff_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_suff_prob_set.md),
and
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)
in one function.

While no alternative input option for frequencies is provided,
specification of the essential probability
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) is always
necessary. However, for 2 other essential probabilities there is a
choice:

1.  Either
    [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or
    [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) is
    necessary (as both are complements).

2.  Either
    [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or
    [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) is
    necessary (as both are complements).

The argument `tol` is optional (with a default value of .01) and used as
the tolerance value of
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md).

`is_valid_prob_set` verifies the validity of inputs, but does not
compute or return numeric variables. Use
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
to verify sets of probabilities that describe extreme cases and
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
for initializing basic parameters.

## See also

[`is_valid_prob_pair`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md)
verifies that probability pairs are complements;
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies probabilities;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric variables;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric variables;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information;
[`as_pc`](https://hneth.github.io/riskyr/dev/reference/as_pc.md)
displays a probability as a percentage;
[`as_pb`](https://hneth.github.io/riskyr/dev/reference/as_pb.md)
displays a percentage as probability.

Other verification functions:
[`is_complement()`](https://hneth.github.io/riskyr/dev/reference/is_complement.md),
[`is_extreme_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md),
[`is_freq()`](https://hneth.github.io/riskyr/dev/reference/is_freq.md),
[`is_integer()`](https://hneth.github.io/riskyr/dev/reference/is_integer.md),
[`is_matrix()`](https://hneth.github.io/riskyr/dev/reference/is_matrix.md),
[`is_perc()`](https://hneth.github.io/riskyr/dev/reference/is_perc.md),
[`is_prob()`](https://hneth.github.io/riskyr/dev/reference/is_prob.md),
[`is_suff_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_suff_prob_set.md),
[`is_valid_freq_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_freq_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_triple.md)

## Examples

``` r
# ways to succeed:
is_valid_prob_set(1, 1, 0, 1, 0)                 # => TRUE
#> [1] TRUE
is_valid_prob_set(.3, .9, .1, .8, .2)            # => TRUE
#> [1] TRUE
is_valid_prob_set(.3, .9, .1, .8, NA)            # => TRUE + warning (NA)
#> [1] TRUE
is_valid_prob_set(.3, .9, NA, .8, NA)            # => TRUE + warning (NAs)
#> [1] TRUE
is_valid_prob_set(.3, .9, NA, NA, .8)            # => TRUE + warning (NAs)
#> [1] TRUE
is_valid_prob_set(.3, .8, .1, .7, .2, tol = .1)  # => TRUE (due to increased tol)
#> [1] TRUE

# watch out for:
is_valid_prob_set(1, 0, 1, 0, 1)    # => TRUE, but NO warning about extreme case!
#> [1] TRUE
is_valid_prob_set(1, 1, 0, 1, 0)    # => TRUE, but NO warning about extreme case!
#> [1] TRUE
is_valid_prob_set(1, 1, 0, 1, NA)   # => TRUE, but NO warning about extreme case!
#> [1] TRUE
is_valid_prob_set(1, 1, 0, NA, 1)   # => TRUE, but NO warning about extreme case!
#> [1] TRUE
is_valid_prob_set(1, 1, 0, NA, 0)   # => TRUE, but NO warning about extreme case!
#> [1] TRUE

# ways to fail:
is_valid_prob_set(8, 1, 0, 1, 0)      # => FALSE + warning (is_prob fails)
#> [1] FALSE
is_valid_prob_set(1, 1, 8, 1, 0)      # => FALSE + warning (is_prob fails)
#> [1] FALSE
is_valid_prob_set(2, 1, 3, 1, 4)      # => FALSE + warning (is_prob fails)
#> [1] FALSE
is_valid_prob_set(1, .8, .2, .7, .2)  # => FALSE + warning (beyond complement range)
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> [1] FALSE
is_valid_prob_set(1, .8, .3, .7, .3)  # => FALSE + warning (beyond complement range)
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> [1] FALSE
is_valid_prob_set(1, 1, 1, 1, 1)      # => FALSE + warning (beyond complement range)
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> [1] FALSE
is_valid_prob_set(1, 1, 0, 1, 1)      # => FALSE + warning (beyond complement range)
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> [1] FALSE
```

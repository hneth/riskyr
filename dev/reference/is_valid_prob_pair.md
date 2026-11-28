# Verify that a pair of probability inputs can be a pair of complementary probabilities.

`is_valid_prob_pair` is a function that verifies that a pair of 2
numeric inputs `p1` and `p2` can be interpreted as a valid pair of
probabilities.

## Usage

``` r
is_valid_prob_pair(p1, p2, tol = 0.01)
```

## Arguments

- p1:

  A numeric argument (typically probability in range from 0 to 1).

- p2:

  A numeric argument (typically probability in range from 0 to 1).

- tol:

  A numeric tolerance value.

## Value

A Boolean value: `TRUE` if exactly one argument is a probability, if
both arguments are probabilities and complements, otherwise `FALSE`.

## Details

`is_valid_prob_pair` is a wrapper function that combines
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md) and
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)
in one function.

Either `p1` or `p2` must be a probability (verified via
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)).
If both arguments are provided they must be probabilities and
complements (verified via
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)).

The argument `tol` is optional (with a default value of .01) Numeric
near-complements that differ by less than this value are still
considered to be complements.

## See also

[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)
uses this function to verify sets of probability inputs;
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)
verifies numeric complements;
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies probabilities;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric variables;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric variables;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
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
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_triple.md)

## Examples

``` r
# ways to succeed:
is_valid_prob_pair(1, 0)      # => TRUE
#> [1] TRUE
is_valid_prob_pair(0, 1)      # => TRUE
#> [1] TRUE
is_valid_prob_pair(1, NA)     # => TRUE + warning (NA)
#> [1] TRUE
is_valid_prob_pair(NA, 1)     # => TRUE + warning (NA)
#> [1] TRUE
is_valid_prob_pair(.50, .51)  # => TRUE (as within tol)
#> [1] TRUE

# ways to fail:
is_valid_prob_pair(.50, .52)  # => FALSE (as beyond tol)
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> [1] FALSE
is_valid_prob_pair(1, 2)      # => FALSE + warning (beyond range)
#> [1] FALSE
is_valid_prob_pair(NA, NA)    # => FALSE + warning (NA)
#> [1] FALSE
```

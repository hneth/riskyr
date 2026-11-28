# Verify that two numbers are complements.

`is_complement` is a function that takes 2 numeric arguments (typically
probabilities) as inputs and verifies that they are *complements* (i.e.,
add up to 1, within some tolerance range `tol`).

## Usage

``` r
is_complement(p1, p2, tol = 0.01)
```

## Arguments

- p1:

  A numeric argument (typically probability in range from 0 to 1).

- p2:

  A numeric argument (typically probability in range from 0 to 1).

- tol:

  A numeric tolerance value. Default: `tol = .01`.

## Value

`NA` or a Boolean value: `NA` if one or both arguments are `NA`; `TRUE`
if both arguments are provided and complements (in `tol` range);
otherwise `FALSE`.

## Details

Both `p1` and `p2` are necessary arguments. If one or both arguments are
`NA`, `is_complement` returns `NA` (i.e., neither `TRUE` nor `FALSE`).

The argument `tol` is optional (with a default value of .01) Numeric
near-complements that differ by less than this value are still
considered to be complements.

This function does not verify the type, range, or sufficiency of the
inputs provided. See
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md) and
[`is_suff_prob_set`](https://hneth.github.io/riskyr/reference/is_suff_prob_set.md)
for this purpose.

## See also

[`comp_complement`](https://hneth.github.io/riskyr/reference/comp_complement.md)
computes a probability's complement;
[`comp_comp_pair`](https://hneth.github.io/riskyr/reference/comp_comp_pair.md)
computes pairs of complements;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric variables;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric variables;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md)
verifies the validity of probability inputs;
[`as_pc`](https://hneth.github.io/riskyr/reference/as_pc.md) displays a
probability as a percentage;
[`as_pb`](https://hneth.github.io/riskyr/reference/as_pb.md) displays a
percentage as probability.

Other verification functions:
[`is_extreme_prob_set()`](https://hneth.github.io/riskyr/reference/is_extreme_prob_set.md),
[`is_freq()`](https://hneth.github.io/riskyr/reference/is_freq.md),
[`is_integer()`](https://hneth.github.io/riskyr/reference/is_integer.md),
[`is_matrix()`](https://hneth.github.io/riskyr/reference/is_matrix.md),
[`is_perc()`](https://hneth.github.io/riskyr/reference/is_perc.md),
[`is_prob()`](https://hneth.github.io/riskyr/reference/is_prob.md),
[`is_suff_prob_set()`](https://hneth.github.io/riskyr/reference/is_suff_prob_set.md),
[`is_valid_freq_set()`](https://hneth.github.io/riskyr/reference/is_valid_freq_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/reference/is_valid_prob_triple.md)

## Examples

``` r
# Basics:
is_complement(0, 1)           # => TRUE
#> [1] TRUE
is_complement(1/3, 2/3)       # => TRUE
#> [1] TRUE
is_complement(.33, .66)       # => TRUE  (as within default tol = .01)
#> [1] TRUE
is_complement(.33, .65)       # => FALSE (as beyond default tol = .01)
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> [1] FALSE

# watch out for:
is_complement(NA, NA)            # => NA (but not FALSE)
#> [1] NA
is_complement(1, NA)             # => NA (but not FALSE)
#> [1] NA
is_complement(2, -1)             # => TRUE + warnings (p1 and p2 beyond range)
#> [1] TRUE
is_complement(8, -7)             # => TRUE + warnings (p1 and p2 beyond range)
#> [1] TRUE
is_complement(.3, .6)            # => FALSE + warning (beyond tolerance)
#> Warning: Probabilities (p1 and p2) are not complements (in tolerated range).
#> [1] FALSE
is_complement(.3, .6, tol = .1)  # => TRUE (due to increased tolerance)
#> [1] TRUE

# ways to fail:
# is_complement(0, 0)            # => FALSE + warning (beyond tolerance)
# is_complement(1, 1)            # => FALSE + warning (beyond tolerance)
# is_complement(8, 8)            # => FALSE + warning (beyond tolerance)
```

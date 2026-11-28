# Test for inters (i.e., whole numbers).

`is_integer` tests if `x` contains *only* integer numbers.

## Usage

``` r
is_integer(x, tol = .Machine$double.eps^0.5)
```

## Arguments

- x:

  Number(s) to test (required, accepts numeric vectors).

- tol:

  Numeric tolerance value. Default: `tol = .Machine$double.eps^0.5` (see
  [`?.Machine`](https://rdrr.io/r/base/zMachine.html) for details).

## Details

Thus, `is_integer` does what the **base** R function `is.integer` is
*not* designed to do:

- `is_integer()` returns TRUE or FALSE depending on whether its numeric
  argument `x` is an integer value (i.e., a "whole" number).

- [`is.integer()`](https://rdrr.io/r/base/integer.html) returns TRUE or
  FALSE depending on whether its argument is of type "integer", and
  FALSE if its argument is a factor.

See the documentation of
[`is.integer`](https://rdrr.io/r/base/integer.html) for definition and
details.

## See also

[`is.integer`](https://rdrr.io/r/base/integer.html) function of the R
**base** package.

Other verification functions:
[`is_complement()`](https://hneth.github.io/riskyr/reference/is_complement.md),
[`is_extreme_prob_set()`](https://hneth.github.io/riskyr/reference/is_extreme_prob_set.md),
[`is_freq()`](https://hneth.github.io/riskyr/reference/is_freq.md),
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
is_integer(2)    # TRUE
#> [1] TRUE
is_integer(2/1)  # TRUE
#> [1] TRUE
is_integer(2/3)  # FALSE
#> [1] FALSE
x <- seq(1, 2, by = 0.5)
is_integer(x)
#> [1]  TRUE FALSE  TRUE

# Note contrast to base R:
is.integer(2/1)  # FALSE!
#> [1] FALSE

# Compare:
is.integer(1 + 2)
#> [1] FALSE
is_integer(1 + 2)
#> [1] TRUE
```

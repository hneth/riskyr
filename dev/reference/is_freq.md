# Verify that input is a frequency (positive integer value).

`is_freq` is a function that checks whether its single argument `freq`
is a frequency (i.e., a positive numeric integer value).

## Usage

``` r
is_freq(freq)
```

## Arguments

- freq:

  A single (typically numeric) argument.

## Value

A Boolean value: `TRUE` if `freq` is a frequency (positive integer),
otherwise `FALSE`.

## See also

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
[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)
verifies the validity of probability inputs;
[`as_pc`](https://hneth.github.io/riskyr/dev/reference/as_pc.md)
displays a probability as a percentage;
[`as_pb`](https://hneth.github.io/riskyr/dev/reference/as_pb.md)
displays a percentage as probability.

Other verification functions:
[`is_complement()`](https://hneth.github.io/riskyr/dev/reference/is_complement.md),
[`is_extreme_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md),
[`is_integer()`](https://hneth.github.io/riskyr/dev/reference/is_integer.md),
[`is_matrix()`](https://hneth.github.io/riskyr/dev/reference/is_matrix.md),
[`is_perc()`](https://hneth.github.io/riskyr/dev/reference/is_perc.md),
[`is_prob()`](https://hneth.github.io/riskyr/dev/reference/is_prob.md),
[`is_suff_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_suff_prob_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_triple.md)

## Examples

``` r
# ways to succeed:
is_freq(2)    # => TRUE, but does NOT return the frequency 2.
#> [1] TRUE
is_freq(0:3)  # => TRUE (for vector)
#> [1] TRUE

## ways to fail:
# is_freq(-1)            # => FALSE + warning (negative values)
# is_freq(1:-1)          # => FALSE (for vector) + warning (negative values)
# is_freq(c(1, 1.5, 2))  # => FALSE (for vector) + warning (non-integer values)

## note:
# is.integer(2)          # => FALSE!
```

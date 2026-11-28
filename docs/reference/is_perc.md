# Verify that input is a percentage (numeric value from 0 to 100).

`is_perc` is a function that checks whether its single argument `perc`
is a percentage (proportion, i.e., a numeric value in the range from 0
to 100).

## Usage

``` r
is_perc(perc)
```

## Arguments

- perc:

  A single (typically numeric) argument.

## Value

A Boolean value: `TRUE` if `perc` is a percentage (proportion),
otherwise `FALSE`.

## See also

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
[`is_complement()`](https://hneth.github.io/riskyr/reference/is_complement.md),
[`is_extreme_prob_set()`](https://hneth.github.io/riskyr/reference/is_extreme_prob_set.md),
[`is_freq()`](https://hneth.github.io/riskyr/reference/is_freq.md),
[`is_integer()`](https://hneth.github.io/riskyr/reference/is_integer.md),
[`is_matrix()`](https://hneth.github.io/riskyr/reference/is_matrix.md),
[`is_prob()`](https://hneth.github.io/riskyr/reference/is_prob.md),
[`is_suff_prob_set()`](https://hneth.github.io/riskyr/reference/is_suff_prob_set.md),
[`is_valid_freq_set()`](https://hneth.github.io/riskyr/reference/is_valid_freq_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/reference/is_valid_prob_triple.md)

## Examples

``` r
# ways to succeed:
is_perc(2)    # => TRUE, but does NOT return the percentage 2.
#> [1] TRUE
is_perc(1/2)  # => TRUE, but does NOT return the percentage 0.5.
#> [1] TRUE

## note:
# pc_sq <- seq(0, 100, by = 10)
# is_perc(pc_sq)       # => TRUE (for vector)

## ways to fail:
# is_perc(NA)          # => FALSE + warning (NA values)
# is_perc(NaN)         # => FALSE + warning (NaN values)
# is_perc("Bernoulli") # => FALSE + warning (non-numeric values)
# is_perc(101)         # => FALSE + warning (beyond range)
```

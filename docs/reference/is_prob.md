# Verify that input is a probability (numeric value from 0 to 1).

`is_prob` is a function that checks whether its argument `prob` (a
scalar or a vector) is a probability (i.e., a numeric value in the range
from 0 to 1).

## Usage

``` r
is_prob(prob, NA_warn = FALSE)
```

## Arguments

- prob:

  A numeric argument (scalar or vector) that is to be checked.

- NA_warn:

  Boolean value determining whether a warning is shown for `NA` values.
  Default: `NA_warn = FALSE`.

## Value

A Boolean value: `TRUE` if `prob` is a probability, otherwise `FALSE`.

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
[`is_perc()`](https://hneth.github.io/riskyr/reference/is_perc.md),
[`is_suff_prob_set()`](https://hneth.github.io/riskyr/reference/is_suff_prob_set.md),
[`is_valid_freq_set()`](https://hneth.github.io/riskyr/reference/is_valid_freq_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/reference/is_valid_prob_triple.md)

## Examples

``` r
is_prob(1/2)                  # TRUE
#> [1] TRUE
is_prob(2)                    # FALSE
#> [1] FALSE

# vectors:
p_seq <- seq(0, 1, by = .1)   # Vector of probabilities
is_prob(p_seq)                # TRUE (as scalar, not: TRUE TRUE etc.)
#> [1] TRUE
is_prob(c(.1, 2, .9))         # FALSE (as scalar, not: TRUE FALSE etc.)
#> [1] FALSE

## watch out for:
# is_prob(NA)                   # => FALSE + NO warning!
# is_prob(0/0)                  # => FALSE + NO warning (NA + NaN values)
# is_prob(0/0, NA_warn = TRUE)  # => FALSE + warning (NA values)

## ways to fail:
# is_prob(8, NA_warn = TRUE)         # => FALSE + warning (outside range element)
# is_prob(c(.5, 8), NA_warn = TRUE)  # => FALSE + warning (outside range vector element)
# is_prob("Laplace", NA_warn = TRUE) # => FALSE + warning (non-numeric values)
```

# Verify that a triple of essential probability inputs is valid.

`is_valid_prob_triple` is a **deprecated** function that verifies that a
set of 3 numeric inputs can be interpreted as a valid set of 3
probabilities.

## Usage

``` r
is_valid_prob_triple(prev, sens, spec)
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
  condition is `TRUE`).

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `FALSE`).

## Value

A Boolean value: `TRUE` if the probabilities provided are valid;
otherwise `FALSE`.

## Details

`is_valid_prob_triple` is a simplified version of
[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md).
It is a quick wrapper function that only verifies
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md) for
all of its 3 arguments.

`is_valid_prob_triple` does not compute or return numeric variables. Use
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
to verify extreme cases and
[`comp_complete_prob_set`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md)
to complete sets of valid probabilities.

## See also

[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
verifies extreme cases;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)
verifies sets of probability inputs;
[`is_valid_prob_pair`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md)
verifies that probability pairs are complements;
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
[`is_valid_freq_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_freq_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)

## Examples

``` r
# ways to work:
is_valid_prob_triple(0, 0, 0)    # => TRUE
#> [1] TRUE
is_valid_prob_triple(1, 1, 1)    # => TRUE
#> [1] TRUE

## ways to fail:
# is_valid_prob_triple(0, 0)       # => ERROR (as no triple)
# is_valid_prob_triple(0, 0, 7)    # => FALSE + warning (beyond range)
# is_valid_prob_triple(0, NA, 0)   # => FALSE + warning (NA)
# is_valid_prob_triple("p", 0, 0)  # => FALSE + warning (non-numeric)
```

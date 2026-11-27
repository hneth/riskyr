# Verify a sufficient set of probability inputs.

`is_suff_prob_set` is a function that takes 3 to 5 probabilities as
inputs and verifies that they are sufficient to compute all derived
probabilities and combined frequencies for a population of
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) individuals.

## Usage

``` r
is_suff_prob_set(prev, sens = NA, mirt = NA, spec = NA, fart = NA)
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

## Value

A Boolean value: `TRUE` if the probabilities provided are sufficient,
otherwise `FALSE`.

## Details

While no alternative input option for frequencies is provided,
specification of the essential probability
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) is always
necessary.

However, for 2 other essential probabilities there is a choice:

1.  either
    [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or
    [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) is
    necessary (as both are complements).

2.  either
    [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or
    [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) is
    necessary (as both are complements).

`is_suff_prob_set` does not verify the type, range, or consistency of
its arguments. See
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md) and
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)
for this purpose.

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
[`is_freq()`](https://hneth.github.io/riskyr/dev/reference/is_freq.md),
[`is_integer()`](https://hneth.github.io/riskyr/dev/reference/is_integer.md),
[`is_matrix()`](https://hneth.github.io/riskyr/dev/reference/is_matrix.md),
[`is_perc()`](https://hneth.github.io/riskyr/dev/reference/is_perc.md),
[`is_prob()`](https://hneth.github.io/riskyr/dev/reference/is_prob.md),
[`is_valid_freq_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_freq_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_triple.md)

## Examples

``` r
# ways to work:
is_suff_prob_set(prev = 1, sens = 1, spec = 1)  # => TRUE
#> [1] TRUE
is_suff_prob_set(prev = 1, mirt = 1, spec = 1)  # => TRUE
#> [1] TRUE
is_suff_prob_set(prev = 1, sens = 1, fart = 1)  # => TRUE
#> [1] TRUE
is_suff_prob_set(prev = 1, mirt = 1, fart = 1)  # => TRUE
#> [1] TRUE

# watch out for:
is_suff_prob_set(prev = 1, sens = 2, spec = 3)  # => TRUE, but is_prob is FALSE
#> [1] TRUE
is_suff_prob_set(prev = 1, mirt = 2, fart = 4)  # => TRUE, but is_prob is FALSE
#> [1] TRUE
is_suff_prob_set(prev = 1, sens = 2, spec = 3, fart = 4)  # => TRUE, but is_prob is FALSE
#> [1] TRUE

## ways to fail:
# is_suff_prob_set()                    # => FALSE + warning (prev missing)
# is_suff_prob_set(prev = 1)            # => FALSE + warning (sens or mirt missing)
# is_suff_prob_set(prev = 1, sens = 1)  # => FALSE + warning (spec or fart missing)
```

# Verify a set of 4 essential frequencies.

`is_valid_freq_set` is a function that verifies that a set of 4
essential frequencies
([`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md),
[`fa`](https://hneth.github.io/riskyr/reference/fa.md), and
[`cr`](https://hneth.github.io/riskyr/reference/cr.md)) forms a valid
set of frequencies (i.e., each value is a non-missing, non-negative
numeric value).

## Usage

``` r
is_valid_freq_set(hi, mi, fa, cr, allow_noninteger = TRUE)
```

## Arguments

- hi:

  The number of hits
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md) (or true
  positives).

- mi:

  The number of misses
  [`mi`](https://hneth.github.io/riskyr/reference/mi.md) (or false
  negatives).

- fa:

  The number of false alarms
  [`fa`](https://hneth.github.io/riskyr/reference/fa.md) (or false
  positives).

- cr:

  The number of correct rejections
  [`cr`](https://hneth.github.io/riskyr/reference/cr.md) (or true
  negatives).

- allow_noninteger:

  Boolean value for allowing non-integer (i.e., exact, non-rounded)
  frequencies. Default: `allow_noninteger = TRUE`.

## Value

A Boolean value: `TRUE` if the 4 frequencies form a valid set; otherwise
`FALSE`.

## Details

Importantly, the *values* of the frequencies are *not* checked for
consistency with current probabilities or population size parameters.

`is_valid_freq_set` is the frequency analog of
[`is_valid_prob_set`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md)
and uses
[`is_freq`](https://hneth.github.io/riskyr/reference/is_freq.md)-style
criteria, but (by default) also accepts non-integer frequencies, as
**riskyr** uses exact (i.e., non-rounded) frequencies internally.
Setting `allow_noninteger = FALSE` additionally requires that all 4
frequencies are integers (e.g., for creating a population of
individuals).

The function is vector-safe: When provided with vectors of frequencies,
it returns a single Boolean value that is `TRUE` only if *all* elements
are valid frequencies.

## See also

[`is_freq`](https://hneth.github.io/riskyr/reference/is_freq.md)
verifies (integer) frequencies;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md)
verifies sets of probabilities;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) contains
current frequency information.

Other verification functions:
[`is_complement()`](https://hneth.github.io/riskyr/reference/is_complement.md),
[`is_extreme_prob_set()`](https://hneth.github.io/riskyr/reference/is_extreme_prob_set.md),
[`is_freq()`](https://hneth.github.io/riskyr/reference/is_freq.md),
[`is_integer()`](https://hneth.github.io/riskyr/reference/is_integer.md),
[`is_matrix()`](https://hneth.github.io/riskyr/reference/is_matrix.md),
[`is_perc()`](https://hneth.github.io/riskyr/reference/is_perc.md),
[`is_prob()`](https://hneth.github.io/riskyr/reference/is_prob.md),
[`is_suff_prob_set()`](https://hneth.github.io/riskyr/reference/is_suff_prob_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/reference/is_valid_prob_triple.md)

## Examples

``` r
is_valid_freq_set(1, 2, 3, 4)    # => TRUE
#> [1] TRUE
is_valid_freq_set(10.5, 2, 3, 4) # => TRUE (non-integer allowed by default)
#> [1] TRUE
is_valid_freq_set(10.5, 2, 3, 4, allow_noninteger = FALSE)  # => FALSE
#> [1] FALSE
is_valid_freq_set(1, 2, 3, NA)   # => FALSE (due to NA)
#> [1] FALSE
is_valid_freq_set(1, 2, 3, -4)   # => FALSE (due to negative value)
#> [1] FALSE
```

# Compute a complete set of probabilities from valid probability inputs.

`comp_complete_prob_set` is a function takes a valid set of (3 to 5)
probabilities as inputs (as a vector) and returns the complete set of (3
essential and 2 optional) probabilities.

## Usage

``` r
comp_complete_prob_set(prev, sens = NA, mirt = NA, spec = NA, fart = NA)
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

A vector of 5 probabilities:
`c(`[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md)`, `[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md)`, `[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md)`, `[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)`, `[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)`)`.

## Details

Assuming that
[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)` = TRUE`
this function uses
[`comp_comp_pair`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md)
on the two optional pairs (i.e.,
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) and
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) and
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)) and
returns the complete set of 5 probabilities.

## See also

[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)
verifies a set of probability inputs;
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
verifies extreme cases;
[`comp_comp_pair`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md)
computes pairs of complements;
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)
verifies numeric complements;
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies probabilities;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric variables;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric variables.

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/dev/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/dev/reference/comp_FOR.md),
[`comp_NPV()`](https://hneth.github.io/riskyr/dev/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/dev/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

## Examples

``` r
# ways to work:
comp_complete_prob_set(1, .8, NA, .7, NA) # => 1.0 0.8 0.2 0.7 0.3
#> [1] 1.0 0.8 0.2 0.7 0.3
comp_complete_prob_set(1, NA, .8, NA, .4) # => 1.0 0.2 0.8 0.6 0.4
#> [1] 1.0 0.2 0.8 0.6 0.4

# watch out for:
comp_complete_prob_set(8)                  # => 8 NA NA NA NA + warnings
#> Warning: One argument (either p1 or p2) is necessary.
#> Warning: One argument (either p1 or p2) is necessary.
#> [1]  8 NA NA NA NA
comp_complete_prob_set(8, 7, 6, 5, 4)      # => 8 7 6 5 4 + no warning (valid set assumed)
#> [1] 8 7 6 5 4
comp_complete_prob_set(8, .8, NA, .7, NA)  # => 8.0 0.8 0.2 0.7 0.3 + no warning (sic)
#> [1] 8.0 0.8 0.2 0.7 0.3
comp_complete_prob_set(8, 2, NA, 3, NA)    # => 8 2 NA 3 NA + no warning (sic)
#> [1]  8  2 NA  3 NA
```

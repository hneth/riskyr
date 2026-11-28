# Compute a probability's (missing) complement and return both.

`comp_comp_pair` is a function that takes 0, 1, or 2 probabilities (`p1`
and `p2`) as inputs. If either of them is missing (`NA`), it computes
the complement of the other one and returns both probabilities.

## Usage

``` r
comp_comp_pair(p1 = NA, p2 = NA)
```

## Arguments

- p1:

  A numeric probability value (in range from 0 to 1). `p1` is optional
  when `p2` is provided.

- p2:

  A numeric probability value (in range from 0 to 1). `p2` is optional
  when `p1` is provided.

## Value

A vector `v` containing 2 numeric probability values (in range from 0 to
1): `v = c(p1, p2)`.

## Details

`comp_comp_pair` does *nothing* when both arguments are provided (i.e.,
`!is.na(p1) & !is.na(p2)`) and only issues a warning if both arguments
are missing (i.e., `is.na(p1) & is.na(p2)`).

Inputs are *not* verified: Use
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md) to
verify that an input is a probability and
[`is_complement`](https://hneth.github.io/riskyr/reference/is_complement.md)
to verify that two provided values actually are complements.

## See also

[`is_complement`](https://hneth.github.io/riskyr/reference/is_complement.md)
verifies numeric complements;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md)
verifies sets of probabilities;
[`comp_complete_prob_set`](https://hneth.github.io/riskyr/reference/comp_complete_prob_set.md)
completes valid sets of probabilities;
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/reference/is_extreme_prob_set.md)
verifies extreme cases;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md)
verifies probabilities.

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/reference/comp_FOR.md),
[`comp_NPV()`](https://hneth.github.io/riskyr/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/reference/comp_PPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md),
[`comp_complement()`](https://hneth.github.io/riskyr/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/reference/comp_spec.md)

## Examples

``` r
# ways to work:
comp_comp_pair(1, 0)   # => 1 0
#> [1] 1 0
comp_comp_pair(0, 1)   # => 0 1
#> [1] 0 1
comp_comp_pair(1, NA)  # => 1 0
#> [1] 1 0
comp_comp_pair(NA, 1)  # => 0 1
#> [1] 0 1

# watch out for:
comp_comp_pair(NA, NA) # => NA NA + warning
#> Warning: One argument (either p1 or p2) is necessary.
#> [1] NA NA
comp_comp_pair(8, 8)   # => 8 8 + NO warning (as is_prob is not verified)
#> [1] 8 8
comp_comp_pair(1, 1)   # => 1 1 + NO warning (as is_complement is not verified)
#> [1] 1 1
```

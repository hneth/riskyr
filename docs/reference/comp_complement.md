# Compute a probability's complement probability.

`comp_complement` computes the probability complement of a given
probability `prob`.

## Usage

``` r
comp_complement(prob)
```

## Arguments

- prob:

  A numeric probability value (in range from 0 to 1).

## Value

A numeric probability value (in range from 0 to 1).

## Details

The type and range of `prob` is verified with
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md).

## See also

[`is_complement`](https://hneth.github.io/riskyr/reference/is_complement.md)
verifies numeric complements;
[`comp_comp_pair`](https://hneth.github.io/riskyr/reference/comp_comp_pair.md)
returns a probability and its complement;
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
[`comp_comp_pair()`](https://hneth.github.io/riskyr/reference/comp_comp_pair.md),
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
comp_complement(0)    # => 1
#> [1] 1
comp_complement(1)    # => 0
#> [1] 0

comp_complement(2)    # => NA + warning (beyond range)
#> [1] NA
comp_complement("p")  # => NA + warning (non-numeric)
#> [1] NA
```

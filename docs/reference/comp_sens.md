# Compute a decision's sensitivity from its miss rate.

`comp_sens` is a conversion function that takes a miss rate
[`mirt`](https://hneth.github.io/riskyr/reference/mirt.md) – given as a
probability (i.e., a numeric value in the range from 0 to 1) – as its
input, and returns the corresponding sensitivity
[`sens`](https://hneth.github.io/riskyr/reference/sens.md) – also as a
probability – as its output.

## Usage

``` r
comp_sens(mirt)
```

## Arguments

- mirt:

  The decision's miss rate
  [`mirt`](https://hneth.github.io/riskyr/reference/mirt.md) as a
  probability.

## Value

The decision's sensitivity
[`sens`](https://hneth.github.io/riskyr/reference/sens.md) as a
probability.

## Details

The sensitivity
[`sens`](https://hneth.github.io/riskyr/reference/sens.md) and miss rate
[`mirt`](https://hneth.github.io/riskyr/reference/mirt.md) are
complements (`sens = (1 - mirt)`) and both features of the decision
process (e.g., a diagnostic test).

The function `comp_sens` is complementary to the conversion function
[`comp_mirt`](https://hneth.github.io/riskyr/reference/comp_mirt.md) and
uses the generic function
[`comp_complement`](https://hneth.github.io/riskyr/reference/comp_complement.md).

## See also

[`comp_complement`](https://hneth.github.io/riskyr/reference/comp_complement.md)
computes a probability's complement;
[`is_complement`](https://hneth.github.io/riskyr/reference/is_complement.md)
verifies probability complements;
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
[`comp_comp_pair()`](https://hneth.github.io/riskyr/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/reference/comp_prob_freq.md),
[`comp_spec()`](https://hneth.github.io/riskyr/reference/comp_spec.md)

## Examples

``` r
comp_sens(2)                       # => NA + warning (beyond range)
#> [1] NA
comp_sens(1/3)                     # => 0.6666667
#> [1] 0.6666667
comp_sens(comp_complement(0.123))  # => 0.123
#> [1] 0.123
```

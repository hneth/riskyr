# Compute a decision's miss rate from its sensitivity.

`comp_mirt` is a conversion function that takes a sensitivity
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) – given
as a probability (i.e., a numeric value in the range from 0 to 1) – as
its input, and returns the corresponding miss rate
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) – also as
a probability – as its output.

## Usage

``` r
comp_mirt(sens)
```

## Arguments

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) as a
  probability.

## Value

The decision's miss rate
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) as a
probability.

## Details

The miss rate
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) and
sensitivity
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) are
complements (`mirt = (1 - sens)`) and both features of the decision
process (e.g., a diagnostic test).

The function `comp_mirt` is complementary to the conversion function
[`comp_sens`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md)
and uses the generic function
[`comp_complement`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md).

## See also

[`comp_complement`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md)
computes a probability's complement;
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md)
verifies probability complements;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies probabilities.

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
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

## Examples

``` r
comp_mirt(2)                       # => NA + warning (beyond range)
#> [1] NA
comp_mirt(1/3)                     # => 0.6666667
#> [1] 0.6666667
comp_mirt(comp_complement(0.123))  # => 0.123
#> [1] 0.123
```

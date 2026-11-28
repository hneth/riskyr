# Compute a decision's false alarm rate from its specificity.

`comp_fart` is a conversion function that takes a specificity
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) – given
as a probability (i.e., a numeric value in the range from 0 to 1) – as
its input, and returns the corresponding false alarm rate
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) – also as
a probability – as its output.

## Usage

``` r
comp_fart(spec)
```

## Arguments

- spec:

  The decision's specificity value
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) as a
  probability.

## Value

The decision's false alarm rate
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) as a
probability.

## Details

The false alarm rate
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) and
specificity
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) are
complements (`fart = (1 - spec)`) and both features of the decision
process (e.g., a diagnostic test).

The function `comp_fart` is complementary to the conversion function
[`comp_spec`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)
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
[`comp_mirt()`](https://hneth.github.io/riskyr/dev/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

## Examples

``` r
comp_fart(2)                       # => NA + warning (beyond range)
#> [1] NA
comp_fart(1/3)                     # => 0.6666667
#> [1] 0.6666667
comp_fart(comp_complement(0.123))  # => 0.123
#> [1] 0.123
```

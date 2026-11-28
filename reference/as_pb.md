# Display a percentage as a (numeric and rounded) probability.

`as_pb` is a function that displays a percentage `perc` as a probability
(rounded to `n_digits` decimals).

## Usage

``` r
as_pb(perc, n_digits = 4)
```

## Arguments

- perc:

  A percentage (as a scalar or vector of numeric values from 0 to 100).

- n_digits:

  Number of decimal places to which percentage is rounded. Default:
  `n_digits = 4`.

## Value

A probability (as a numeric value).

## Details

`as_pb` and its complement function
[`as_pc`](https://hneth.github.io/riskyr/reference/as_pc.md) allow
toggling the display of numeric values between percentages and
probabilities.

## See also

[`is_perc`](https://hneth.github.io/riskyr/reference/is_perc.md)
verifies a percentage;
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md)
verifies a probability;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md)
verifies the validity of probability inputs;
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
[`comp_complement`](https://hneth.github.io/riskyr/reference/comp_complement.md)
computes a probability's complement;
[`comp_comp_pair`](https://hneth.github.io/riskyr/reference/comp_comp_pair.md)
computes pairs of complements.

Other utility functions:
[`as_pc()`](https://hneth.github.io/riskyr/reference/as_pc.md),
[`plot.box()`](https://hneth.github.io/riskyr/reference/plot.box.md),
[`print.box()`](https://hneth.github.io/riskyr/reference/print.box.md)

Other display functions:
[`as_pc()`](https://hneth.github.io/riskyr/reference/as_pc.md)

## Examples

``` r
as_pb(1/3)          # => 0.0033
#> [1] 0.0033
as_pb(as_pc(2/3))   # => 0.6667 (rounded to 4 decimals)
#> [1] 0.6667
```

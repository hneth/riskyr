# Display a probability as a (numeric and rounded) percentage.

`as_pc` is a function that displays a probability `prob` as a percentage
(rounded to `n_digits` decimals).

## Usage

``` r
as_pc(prob, n_digits = 2)
```

## Arguments

- prob:

  A probability (as a scalar or vector of numeric values from 0 to 1).

- n_digits:

  Number of decimal places to which percentage is rounded. Default:
  `n_digits = 2`.

## Value

A percentage (as a numeric value).

## Details

`as_pc` and its complement function
[`as_pb`](https://hneth.github.io/riskyr/dev/reference/as_pb.md) allow
toggling the display of numeric values between percentages and
probabilities.

## See also

[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies a probability;
[`is_perc`](https://hneth.github.io/riskyr/dev/reference/is_perc.md)
verifies a percentage;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)
verifies the validity of probability inputs;
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
[`comp_complement`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md)
computes a probability's complement;
[`comp_comp_pair`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md)
computes pairs of complements.

Other utility functions:
[`as_pb()`](https://hneth.github.io/riskyr/dev/reference/as_pb.md),
[`plot.box()`](https://hneth.github.io/riskyr/dev/reference/plot.box.md),
[`print.box()`](https://hneth.github.io/riskyr/dev/reference/print.box.md)

Other display functions:
[`as_pb()`](https://hneth.github.io/riskyr/dev/reference/as_pb.md)

## Examples

``` r
as_pc(.50)                # 50
#> [1] 50
as_pc(1/3)                # 33.33
#> [1] 33.33
as_pc(1/3, n_digits = 0)  # 33
#> [1] 33
as_pc(as_pb(12.3))        # 12.3
#> [1] 12.3
```

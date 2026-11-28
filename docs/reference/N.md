# Number of individuals in the population.

`N` is a frequency that describes the number of individuals in the
current population (i.e., the overall number of cases considered).

## Usage

``` r
N
```

## Details

Key relationships between frequencies and probabilities (see
documentation of
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md) or
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md) for
details):

- Three perspectives on a population:

  by condition / by decision / by accuracy.

- Defining probabilities in terms of frequencies:

  Probabilities can be computed as ratios between frequencies, but
  beware of rounding issues.

Current frequency information is computed by
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md) and
contained in a list
[`freq`](https://hneth.github.io/riskyr/reference/freq.md).

## References

Consult [Wikipedia: Statistical
population](https://en.wikipedia.org/wiki/Statistical_population) for
additional information.

## See also

[`is_freq`](https://hneth.github.io/riskyr/reference/is_freq.md)
verifies frequencies;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information.

Other frequencies:
[`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md),
[`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md),
[`cr`](https://hneth.github.io/riskyr/reference/cr.md),
[`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md),
[`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md),
[`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md),
[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md),
[`fa`](https://hneth.github.io/riskyr/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md)

## Examples

``` r
N <- 1000   # => sets a population size of 1000
is_freq(N)  # => TRUE
#> [1] TRUE
is_prob(N)  # => FALSE (as N is no probability)
#> [1] FALSE
```

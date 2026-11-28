# Number of individuals for which the decision is erroneous.

`dec_err` is a frequency that describes the number of individuals in the
current population [`N`](https://hneth.github.io/riskyr/reference/N.md)
for which the decision is incorrect or erroneous (i.e., cases in which
the decision does not correspond to the condition).

## Usage

``` r
dec_err
```

## Details

Key relationships:

1.  to probabilities: The frequency of `dec_err` individuals depends on
    the population size
    [`N`](https://hneth.github.io/riskyr/reference/N.md) and is equal to
    the sum of false negatives
    [`mi`](https://hneth.github.io/riskyr/reference/mi.md) and false
    positives [`fa`](https://hneth.github.io/riskyr/reference/fa.md).

2.  to other frequencies: In a population of size
    [`N`](https://hneth.github.io/riskyr/reference/N.md) the following
    relationships hold:

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)` + `[`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md)
      (by condition)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)` + `[`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)
      (by decision)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)` + dec_err`
      (by correspondence of decision to condition)

    - [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)` = `[`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`cr`](https://hneth.github.io/riskyr/reference/cr.md)

    - `dec_err = `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/reference/fa.md)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/reference/cr.md)
      (by condition x decision)

Current frequency information is computed by
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md) and
contained in a list
[`freq`](https://hneth.github.io/riskyr/reference/freq.md).

## References

Consult [Wikipedia: Confusion
matrix](https://en.wikipedia.org/wiki/Confusion_matrix) for additional
information.

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

Other frequencies: [`N`](https://hneth.github.io/riskyr/reference/N.md),
[`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md),
[`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md),
[`cr`](https://hneth.github.io/riskyr/reference/cr.md),
[`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md),
[`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md),
[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md),
[`fa`](https://hneth.github.io/riskyr/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md)

## Examples

``` r
dec_err <- 1000 * .50   # => sets dec_err to 50% of 1000 = 500 cases.
is_freq(dec_err)        # => TRUE
#> [1] TRUE
is_prob(dec_err)        # => FALSE, as dec_err is no probability (but acc, bacc/wacc ARE)
#> [1] FALSE
```

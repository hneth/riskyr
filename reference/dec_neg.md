# Number of individuals for which the decision is negative.

`dec_neg` is a frequency that describes the number of individuals in the
current population [`N`](https://hneth.github.io/riskyr/reference/N.md)
for which the decision is negative (i.e., cases not called or not
predicted).

## Usage

``` r
dec_neg
```

## Details

Key relationships:

1.  to probabilities: The frequency of `dec_neg` individuals depends on
    the population size
    [`N`](https://hneth.github.io/riskyr/reference/N.md) and the
    decision's proportion of negative decisions
    `(1 - `[`ppod`](https://hneth.github.io/riskyr/reference/ppod.md)`)`
    and is split further into two subsets of
    [`cr`](https://hneth.github.io/riskyr/reference/cr.md) by the
    negative predictive value
    [`NPV`](https://hneth.github.io/riskyr/reference/NPV.md) and
    [`mi`](https://hneth.github.io/riskyr/reference/mi.md) by the false
    omission rate
    [`FOR`](https://hneth.github.io/riskyr/reference/FOR.md)` = 1 - `[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md).

    Perspectives:

    1.  by condition:

        The frequency `dec_neg` is determined by the population size
        [`N`](https://hneth.github.io/riskyr/reference/N.md) times the
        proportion of negative decisions
        `(1 - `[`ppod`](https://hneth.github.io/riskyr/reference/ppod.md)`)`:

        `dec_neg = `[`N`](https://hneth.github.io/riskyr/reference/N.md)` x (1 - `[`ppod`](https://hneth.github.io/riskyr/reference/ppod.md)`)`

    2.  by decision:

        a\. The frequency
        [`cr`](https://hneth.github.io/riskyr/reference/cr.md) is
        determined by `dec_neg` times the negative predictive value
        [`NPV`](https://hneth.github.io/riskyr/reference/NPV.md):

        [`cr`](https://hneth.github.io/riskyr/reference/cr.md)` = dec_neg x `[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md)

        b\. The frequency
        [`mi`](https://hneth.github.io/riskyr/reference/mi.md) is
        determined by `dec_neg` times the false omission rate
        [`FOR`](https://hneth.github.io/riskyr/reference/FOR.md)` = (1 - `[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md)`)`:

        [`mi`](https://hneth.github.io/riskyr/reference/mi.md)` = dec_neg x `[`FOR`](https://hneth.github.io/riskyr/reference/FOR.md)` = dec_neg x (1 - `[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md)`)`

2.  to other frequencies: In a population of size
    [`N`](https://hneth.github.io/riskyr/reference/N.md) the following
    relationships hold:

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)` + `[`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md)
      (by condition)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)` + dec_neg`
      (by decision)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)` + `[`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md)
      (by correspondence of decision to condition)

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
[`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md),
[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md),
[`fa`](https://hneth.github.io/riskyr/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md)

## Examples

``` r
dec_neg <- 1000 * .67   # => sets dec_neg to 67% of 1000 = 670 cases.
is_freq(dec_neg)        # => TRUE
#> [1] TRUE
is_prob(dec_neg)        # => FALSE, as dec_neg is no probability (but ppod, NPV and FOR are)
#> [1] FALSE
```

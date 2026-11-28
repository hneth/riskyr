# Number of individuals for which the condition is true.

`cond_true` is a frequency that describes the number of individuals in
the current population
[`N`](https://hneth.github.io/riskyr/reference/N.md) for which the
condition is `TRUE` (i.e., actually true cases).

## Usage

``` r
cond_true
```

## Details

Key relationships:

1.  to probabilities: The frequency of `cond_true` individuals depends
    on the population size
    [`N`](https://hneth.github.io/riskyr/reference/N.md) and the
    condition's prevalence
    [`prev`](https://hneth.github.io/riskyr/reference/prev.md) and is
    split further into two subsets of
    [`hi`](https://hneth.github.io/riskyr/reference/hi.md) by the
    sensitivity
    [`sens`](https://hneth.github.io/riskyr/reference/sens.md) and
    [`mi`](https://hneth.github.io/riskyr/reference/mi.md) by the miss
    rate [`mirt`](https://hneth.github.io/riskyr/reference/mirt.md).

    Perspectives:

    1.  by condition:

        The frequency `cond_true` is determined by the population size
        [`N`](https://hneth.github.io/riskyr/reference/N.md) times the
        prevalence
        [`prev`](https://hneth.github.io/riskyr/reference/prev.md):

        ` cond_true = `[`N`](https://hneth.github.io/riskyr/reference/N.md)` x `[`prev`](https://hneth.github.io/riskyr/reference/prev.md)

    2.  by decision:

        a\. The frequency
        [`hi`](https://hneth.github.io/riskyr/reference/hi.md) is
        determined by `cond_true` times the sensitivity
        [`sens`](https://hneth.github.io/riskyr/reference/sens.md) (aka.
        hit rate
        [`HR`](https://hneth.github.io/riskyr/reference/sens.md)):

        ` `[`hi`](https://hneth.github.io/riskyr/reference/hi.md)` = cond_true x `[`sens`](https://hneth.github.io/riskyr/reference/sens.md)

        b\. The frequency
        [`mi`](https://hneth.github.io/riskyr/reference/mi.md) is
        determined by `cond_true` times the miss rate
        [`mirt`](https://hneth.github.io/riskyr/reference/mirt.md)` = (1 - `[`sens`](https://hneth.github.io/riskyr/reference/sens.md)`)`:

        ` `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)` = cond_true x `[`mirt`](https://hneth.github.io/riskyr/reference/mirt.md)` = cond_true x (1 - `[`sens`](https://hneth.github.io/riskyr/reference/sens.md)`)`

2.  to other frequencies: In a population of size
    [`N`](https://hneth.github.io/riskyr/reference/N.md) the following
    relationships hold:

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = cond_true + `[`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md)
      (by condition)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)` + `[`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)
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
cond_true <- 1000 * .10   # => sets cond_true to 10% of 1000 = 100 cases.
is_freq(cond_true)        # => TRUE
#> [1] TRUE
is_prob(cond_true)        # => FALSE, as cond_true is no probability (but prev and sens are)
#> [1] FALSE
```

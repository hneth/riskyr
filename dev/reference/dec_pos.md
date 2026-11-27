# Number of individuals for which the decision is positive.

`dec_pos` is a frequency that describes the number of individuals in the
current population
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) for which the
decision is positive (i.e., called or predicted cases).

## Usage

``` r
dec_pos
```

## Details

Key relationships:

1.  to probabilities: The frequency of `dec_pos` individuals depends on
    the population size
    [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) and the
    decision's proportion of positive decisions
    [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md) and
    is split further into two subsets of
    [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md) by the
    positive predictive value
    [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) and
    [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) by the
    false detection rate
    [`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md)` = 1 - `[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md).

    Perspectives:

    1.  by condition:

        The frequency `dec_pos` is determined by the population size
        [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) times
        the proportion of positive decisions
        [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md):

        `dec_pos = `[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` x `[`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md)

    2.  by decision:

        a\. The frequency
        [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md) is
        determined by `dec_pos` times the positive predictive value
        [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md)
        (aka.
        [`precision`](https://hneth.github.io/riskyr/dev/reference/PPV.md)):

        [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` = dec_pos x `[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md)

        b\. The frequency
        [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) is
        determined by `dec_pos` times the false detection rate
        [`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md)` = (1 - `[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md)`)`:

        [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` = dec_pos x `[`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md)` = dec_pos x (1 - `[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md)`)`

2.  to other frequencies: In a population of size
    [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) the
    following relationships hold:

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)` + `[`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
      (by condition)

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = dec_pos + `[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)
      (by decision)

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)` + `[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)
      (by correspondence of decision to condition)

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)
      (by condition x decision)

Current frequency information is computed by
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
and contained in a list
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md).

## References

Consult [Wikipedia: Confusion
matrix](https://en.wikipedia.org/wiki/Confusion_matrix) for additional
information.

## See also

[`is_freq`](https://hneth.github.io/riskyr/dev/reference/is_freq.md)
verifies frequencies;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric parameters;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information.

Other frequencies:
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md),
[`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md),
[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md),
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md),
[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md),
[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md),
[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)

## Examples

``` r
dec_pos <- 1000 * .33   # => sets dec_pos to 33% of 1000 = 330 cases.
is_freq(dec_pos)        # => TRUE
#> [1] TRUE
is_prob(dec_pos)        # => FALSE, as dec_pos is no probability (but ppod and PPV are)
#> [1] FALSE
```

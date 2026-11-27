# Number of individuals for which the decision is correct.

`dec_cor` is a frequency that describes the number of individuals in the
current population
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) for which the
decision is correct/accurate (i.e., cases in which the decision
corresponds to the condition).

## Usage

``` r
dec_cor
```

## Details

Key relationships:

1.  to probabilities: The frequency of `dec_cor` individuals depends on
    the population size
    [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) and the
    accuracy
    [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md).

2.  to other frequencies: In a population of size
    [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) the
    following relationships hold:

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)` + `[`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
      (by condition)

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)` + `[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)
      (by decision)

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = dec_cor + `[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)
      (by correspondence of decision to condition)

    - `dec_cor = `[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)

    - [`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)` = `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)
      (by condition x decision)

3.  correspondence: When not rounding the frequencies of
    [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) then

    `dec_cor = N x acc = hi + cr`

    (i.e., `dec_cor` corresponds to the sum of true positives
    [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md) and true
    negatives
    [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md).

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
[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md),
[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md),
[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)

## Examples

``` r
dec_cor <- 1000 * .50   # => sets dec_cor to 50% of 1000 = 500 cases.
is_freq(dec_cor)        # => TRUE
#> [1] TRUE
is_prob(dec_cor)        # => FALSE, as dec_cor is no probability (but acc, bacc/wacc ARE)
#> [1] FALSE
```

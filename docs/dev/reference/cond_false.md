# Number of individuals for which the condition is false.

`cond_false` is a frequency that describes the number of individuals in
the current population
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) for which the
condition is `FALSE` (i.e., actually false cases).

## Usage

``` r
cond_false
```

## Details

Key relationships:

1.  to probabilities: The frequency of `cond_false` individuals depends
    on the population size
    [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) and the
    complement of the condition's prevalence
    `1 - `[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md)
    and is split further into two subsets of
    [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) by the
    false alarm rate
    [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) and
    [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) by the
    specificity
    [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md).

    Perspectives:

    1.  by condition:

        The frequency `cond_false` is determined by the population size
        [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) times
        the complement of the prevalence
        `(1 - `[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md)`)`:

        `cond_false= `[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` x (1 - `[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md)`)`

    2.  by decision:

        a\. The frequency
        [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) is
        determined by `cond_false` times the false alarm rate
        [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)` = (1 - `[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)`)`
        (aka.
        [`FPR`](https://hneth.github.io/riskyr/dev/reference/fart.md)):

        [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)` = cond_false x `[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)` = cond_false x (1 - `[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)`) `

        b\. The frequency
        [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) is
        determined by `cond_false` times the specificity
        [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)` = (1 - `[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)`)`:

        [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)` = cond_false x `[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)` = cond_false x (1 - `[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)`) `

2.  to other frequencies: In a population of size
    [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) the
    following relationships hold:

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)` + cond_false`
      (by condition)

    - [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` = `[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)` + `[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)
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
[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md),
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md),
[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md),
[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md),
[`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md),
[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)

## Examples

``` r
cond_false <- 1000 * .90  # => sets cond_false to 90% of 1000 = 900 cases.
is_freq(cond_false)       # => TRUE
#> [1] TRUE
is_prob(cond_false)       # => FALSE, as cond_false is no probability [but (1 - prev) and spec are]
#> [1] FALSE
```

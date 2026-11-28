# Frequency of correct rejections or true negatives (TN).

`cr` is the frequency of correct rejections or true negatives (`TN`) in
a population of [`N`](https://hneth.github.io/riskyr/reference/N.md)
individuals.

## Usage

``` r
cr
```

## Details

Definition: `cr` is the frequency of individuals for which
`Condition = FALSE` and `Decision = FALSE` (negative).

`cr` is a measure of correct classifications, not an individual case.

Relationships:

1.  to probabilities: The frequency `cr` depends on the specificity
    [`spec`](https://hneth.github.io/riskyr/reference/spec.md) (aka.
    true negative rate, TNR) and is conditional on the prevalence
    [`prev`](https://hneth.github.io/riskyr/reference/prev.md).

2.  to other frequencies: In a population of size
    [`N`](https://hneth.github.io/riskyr/reference/N.md) the following
    relationships hold:

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)` + `[`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md)
      (by condition)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)` + `[`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)
      (by decision)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)` + `[`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md)
      (by correspondence of decision to condition)

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/reference/fa.md)` + cr`
      (by condition x decision)

## See also

[`spec`](https://hneth.github.io/riskyr/reference/spec.md) is the
specificity or correct rejection rate (aka. true negative rate
[`TNR`](https://hneth.github.io/riskyr/reference/spec.md));
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
computes current probability information;
[`is_freq`](https://hneth.github.io/riskyr/reference/is_freq.md)
verifies frequencies.

Other essential parameters:
[`fa`](https://hneth.github.io/riskyr/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md),
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md)

Other frequencies: [`N`](https://hneth.github.io/riskyr/reference/N.md),
[`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md),
[`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md),
[`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md),
[`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md),
[`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md),
[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md),
[`fa`](https://hneth.github.io/riskyr/reference/fa.md),
[`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md)

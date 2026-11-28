# Frequency of false alarms or false positives (FP).

`fa` is the frequency of false alarms or false positives (`FP`) in a
population of [`N`](https://hneth.github.io/riskyr/reference/N.md)
individuals.

## Usage

``` r
fa
```

## Details

Definition: `fa` is the frequency of individuals for which
`Condition = FALSE` and `Decision = TRUE` (positive).

`fa` is a measure of incorrect classifications (type-I-errors), not an
individual case.

Relationships:

1.  to probabilities: The frequency `fa` depends on the false alarm rate
    [`fart`](https://hneth.github.io/riskyr/reference/fart.md) (aka.
    false positive rate, FPR) and is conditional on the prevalence
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

    - [`N`](https://hneth.github.io/riskyr/reference/N.md)` = `[`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)` + fa + `[`cr`](https://hneth.github.io/riskyr/reference/cr.md)
      (by condition x decision)

## See also

[`fart`](https://hneth.github.io/riskyr/reference/fart.md) is the
probability of false alarms (aka. false positive rate
[`FPR`](https://hneth.github.io/riskyr/reference/fart.md) or
[`fallout`](https://hneth.github.io/riskyr/reference/fart.md));
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
[`cr`](https://hneth.github.io/riskyr/reference/cr.md),
[`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md),
[`prev`](https://hneth.github.io/riskyr/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/reference/spec.md)

Other frequencies: [`N`](https://hneth.github.io/riskyr/reference/N.md),
[`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md),
[`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md),
[`cr`](https://hneth.github.io/riskyr/reference/cr.md),
[`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md),
[`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md),
[`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md),
[`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md),
[`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md)

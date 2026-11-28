# Compute the condition's prevalence (baseline probability) from frequencies.

`comp_prev` computes a condition's prevalence value
[`prev`](https://hneth.github.io/riskyr/reference/prev.md) (or baseline
probability) from 4 essential frequencies
([`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md),
[`fa`](https://hneth.github.io/riskyr/reference/fa.md),
[`cr`](https://hneth.github.io/riskyr/reference/cr.md)).

## Usage

``` r
comp_prev(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)
```

## Arguments

- hi:

  The number of hits
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md) (or true
  positives).

- mi:

  The number of misses
  [`mi`](https://hneth.github.io/riskyr/reference/mi.md) (or false
  negatives).

- fa:

  The number of false alarms
  [`fa`](https://hneth.github.io/riskyr/reference/fa.md) (or false
  positives).

- cr:

  The number of correct rejections
  [`cr`](https://hneth.github.io/riskyr/reference/cr.md) (or true
  negatives).

## Details

A condition's prevalence value
[`prev`](https://hneth.github.io/riskyr/reference/prev.md) is the
probability of the condition being `TRUE`.

The probability
[`prev`](https://hneth.github.io/riskyr/reference/prev.md) can be
computed from frequencies as the the ratio of
[`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)
(i.e.,
[`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/reference/mi.md))
divided by [`N`](https://hneth.github.io/riskyr/reference/N.md) (i.e.,
[`hi`](https://hneth.github.io/riskyr/reference/hi.md)` + `[`mi`](https://hneth.github.io/riskyr/reference/mi.md)` + `[`fa`](https://hneth.github.io/riskyr/reference/fa.md)` + `[`cr`](https://hneth.github.io/riskyr/reference/cr.md)):

`prev = cond_true/N = (hi + mi)/(hi + mi + fa + cr)`

## See also

[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`is_prob`](https://hneth.github.io/riskyr/reference/is_prob.md)
verifies probability inputs;
[`is_freq`](https://hneth.github.io/riskyr/reference/is_freq.md)
verifies frequency inputs.

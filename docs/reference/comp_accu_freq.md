# Compute accuracy metrics of current classification results.

`comp_accu_freq` computes a list of current accuracy metrics from the 4
essential frequencies
([`hi`](https://hneth.github.io/riskyr/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/reference/mi.md),
[`fa`](https://hneth.github.io/riskyr/reference/fa.md),
[`cr`](https://hneth.github.io/riskyr/reference/cr.md)) that constitute
the current confusion matrix and are contained in
[`freq`](https://hneth.github.io/riskyr/reference/freq.md).

## Usage

``` r
comp_accu_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr, w = 0.5)
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

- w:

  The weighting parameter `w` (from 0 to 1) for computing weighted
  accuracy `wacc`. Default: `w = .50` (i.e., yielding balanced accuracy
  `bacc`).

## Value

A list [`accu`](https://hneth.github.io/riskyr/reference/accu.md)
containing current accuracy metrics.

## Details

Currently computed accuracy metrics include:

1.  `acc`: Overall accuracy as the proportion (or probability) of
    correctly classifying cases or of
    [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)
    cases:

    `acc = dec_cor/N = (hi + cr)/(hi + mi + fa + cr)`

    Values range from 0 (no correct prediction) to 1 (perfect
    prediction).

2.  `wacc`: Weighted accuracy, as a weighted average of the sensitivity
    [`sens`](https://hneth.github.io/riskyr/reference/sens.md) (aka. hit
    rate [`HR`](https://hneth.github.io/riskyr/reference/sens.md),
    [`TPR`](https://hneth.github.io/riskyr/reference/sens.md),
    [`power`](https://hneth.github.io/riskyr/reference/sens.md) or
    [`recall`](https://hneth.github.io/riskyr/reference/sens.md)) and
    the the specificity
    [`spec`](https://hneth.github.io/riskyr/reference/spec.md) (aka.
    [`TNR`](https://hneth.github.io/riskyr/reference/spec.md)) in which
    [`sens`](https://hneth.github.io/riskyr/reference/sens.md) is
    multiplied by a weighting parameter `w` (ranging from 0 to 1) and
    [`spec`](https://hneth.github.io/riskyr/reference/spec.md) is
    multiplied by `w`'s complement `(1 - w)`:

    `wacc = (w * sens) + ((1 - w) * spec)`

    If `w = .50`, `wacc` becomes *balanced* accuracy `bacc`.

3.  `mcc`: The Matthews correlation coefficient (with values ranging
    from -1 to +1):

    `mcc = ((hi * cr) - (fa * mi)) / sqrt((hi + fa) * (hi + mi) * (cr + fa) * (cr + mi))`

    A value of `mcc = 0` implies random performance; `mcc = 1` implies
    perfect performance.

    See [Wikipedia: Matthews correlation
    coefficient](https://en.wikipedia.org/wiki/Matthews_correlation_coefficient)
    for additional information.

4.  `f1s`: The harmonic mean of the positive predictive value
    [`PPV`](https://hneth.github.io/riskyr/reference/PPV.md) (aka.
    [`precision`](https://hneth.github.io/riskyr/reference/PPV.md)) and
    the sensitivity
    [`sens`](https://hneth.github.io/riskyr/reference/sens.md) (aka. hit
    rate [`HR`](https://hneth.github.io/riskyr/reference/sens.md),
    [`TPR`](https://hneth.github.io/riskyr/reference/sens.md),
    [`power`](https://hneth.github.io/riskyr/reference/sens.md) or
    [`recall`](https://hneth.github.io/riskyr/reference/sens.md)):

    `f1s = 2 * (PPV * sens) / (PPV + sens)`

    See [Wikipedia: F1 score](https://en.wikipedia.org/wiki/F1_score)
    for additional information.

Notes:

- Accuracy metrics describe the *correspondence* of decisions (or
  predictions) to actual conditions (or truth).

  There are several possible interpretations of accuracy:

  1.  as *probabilities* (i.e., `acc` being the proportion of correct
      classifications, or the ratio
      [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)/[`N`](https://hneth.github.io/riskyr/reference/N.md)),

  2.  as *frequencies* (e.g., as classifying a population of
      [`N`](https://hneth.github.io/riskyr/reference/N.md) individuals
      into cases of
      [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)
      vs.
      [`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md)),

  3.  as *correlations* (e.g., see `mcc` in
      [`accu`](https://hneth.github.io/riskyr/reference/accu.md)).

- Computing exact accuracy values based on probabilities (by
  [`comp_accu_prob`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md))
  may differ from accuracy values computed from (possibly rounded)
  frequencies (by `comp_accu_freq`).

  When frequencies are rounded to integers (see the default of
  `round = TRUE` in
  [`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
  and
  [`comp_freq_prob`](https://hneth.github.io/riskyr/reference/comp_freq_prob.md))
  the accuracy metrics computed by `comp_accu_freq` correspond to these
  rounded values. Use
  [`comp_accu_prob`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md)
  to obtain exact accuracy metrics from probabilities.

## References

Consult [Wikipedia: Confusion
matrix](https://en.wikipedia.org/wiki/Confusion_matrix) for additional
information.

## See also

[`accu`](https://hneth.github.io/riskyr/reference/accu.md) for all
accuracy metrics;
[`comp_accu_prob`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md)
computes exact accuracy metrics from probabilities;
[`num`](https://hneth.github.io/riskyr/reference/num.md) for basic
numeric parameters;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) for current
frequency information;
[`txt`](https://hneth.github.io/riskyr/reference/txt.md) for current
text settings; [`pal`](https://hneth.github.io/riskyr/reference/pal.md)
for current color settings;
[`popu`](https://hneth.github.io/riskyr/reference/popu.md) for a table
of the current population.

Other metrics: [`acc`](https://hneth.github.io/riskyr/reference/acc.md),
[`accu`](https://hneth.github.io/riskyr/reference/accu.md),
[`comp_acc()`](https://hneth.github.io/riskyr/reference/comp_acc.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md),
[`comp_err()`](https://hneth.github.io/riskyr/reference/comp_err.md),
[`err`](https://hneth.github.io/riskyr/reference/err.md)

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/reference/comp_FOR.md),
[`comp_NPV()`](https://hneth.github.io/riskyr/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/reference/comp_PPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/reference/comp_acc.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/reference/comp_spec.md)

## Examples

``` r
comp_accu_freq()  # => accuracy metrics for freq of current scenario
#> $acc
#> [1] 0.774
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] 0.7986667
#> 
#> $mcc
#> [1] 0.5279731
#> 
#> $f1s
#> [1] 0.6523077
#> 
comp_accu_freq(hi = 1, mi = 2, fa = 3, cr = 4)  # medium accuracy, but cr > hi
#> $acc
#> [1] 0.5
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] 0.452381
#> 
#> $mcc
#> [1] -0.08908708
#> 
#> $f1s
#> [1] 0.2857143
#> 

# Extreme cases:
comp_accu_freq(hi = 1, mi = 1, fa = 1, cr = 1)  # random performance
#> $acc
#> [1] 0.5
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] 0.5
#> 
#> $mcc
#> [1] 0
#> 
#> $f1s
#> [1] 0.5
#> 
comp_accu_freq(hi = 0, mi = 0, fa = 1, cr = 1)  # random performance: wacc and f1s are NaN
#> Warning: accu$mcc: A denominator of 0 was corrected to 1, resulting in mcc = 0.
#> $acc
#> [1] 0.5
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] NaN
#> 
#> $mcc
#> [1] 0
#> 
#> $f1s
#> [1] NaN
#> 
comp_accu_freq(hi = 1, mi = 0, fa = 0, cr = 1)  # perfect accuracy/optimal performance
#> $acc
#> [1] 1
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] 1
#> 
#> $mcc
#> [1] 1
#> 
#> $f1s
#> [1] 1
#> 
comp_accu_freq(hi = 0, mi = 1, fa = 1, cr = 0)  # zero accuracy/worst performance, but see f1s
#> $acc
#> [1] 0
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] 0
#> 
#> $mcc
#> [1] -1
#> 
#> $f1s
#> [1] NaN
#> 
comp_accu_freq(hi = 1, mi = 0, fa = 0, cr = 0)  # perfect accuracy, but see wacc and mcc
#> Warning: accu$mcc: A denominator of 0 was corrected to 1, resulting in mcc = 0.
#> $acc
#> [1] 1
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] NaN
#> 
#> $mcc
#> [1] 0
#> 
#> $f1s
#> [1] 1
#> 

# Effects of w:
comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/2)  # equal weights to sens and spec
#> $acc
#> [1] 0.7
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] 0.7
#> 
#> $mcc
#> [1] 0.4082483
#> 
#> $f1s
#> [1] 0.6666667
#> 
comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 2/3)  # more weight to sens
#> $acc
#> [1] 0.7
#> 
#> $w
#> [1] 0.6666667
#> 
#> $wacc
#> [1] 0.6666667
#> 
#> $mcc
#> [1] 0.4082483
#> 
#> $f1s
#> [1] 0.6666667
#> 
comp_accu_freq(hi = 3, mi = 2, fa = 1, cr = 4, w = 1/3)  # more weight to spec
#> $acc
#> [1] 0.7
#> 
#> $w
#> [1] 0.3333333
#> 
#> $wacc
#> [1] 0.7333333
#> 
#> $mcc
#> [1] 0.4082483
#> 
#> $f1s
#> [1] 0.6666667
#> 

## Contrasting comp_accu_freq and comp_accu_prob:
# (a) comp_accu_freq (based on rounded frequencies):
freq1 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4)   # => hi = 2, mi = 1, fa = 2, cr = 5
accu1 <- comp_accu_freq(freq1$hi, freq1$mi, freq1$fa, freq1$cr)  # => accu1 (based on rounded freq).
# accu1
#
# (b) comp_accu_prob (based on probabilities):
accu2 <- comp_accu_prob(prev = 1/3, sens = 2/3, spec = 3/4)      # => exact accu (based on prob).
# accu2
all.equal(accu1, accu2)  # => 4 differences!
#> [1] "Component “acc”: Mean relative difference: 0.03174603" 
#> [2] "Component “wacc”: Mean relative difference: 0.02586207"
#> [3] "Component “mcc”: Mean relative difference: 0.1306675"  
#> [4] "Component “f1s”: Mean relative difference: 0.07692308" 
#
# (c) comp_accu_freq (exact values, i.e., without rounding):
freq3 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4, round = FALSE)
accu3 <- comp_accu_freq(freq3$hi, freq3$mi, freq3$fa, freq3$cr)  # => accu3 (based on EXACT freq).
# accu3
all.equal(accu2, accu3)  # => TRUE (qed).
#> [1] TRUE

```

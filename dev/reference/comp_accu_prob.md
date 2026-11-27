# Compute exact accuracy metrics based on probabilities.

`comp_accu_prob` computes a list of exact accuracy metrics from a
sufficient and valid set of 3 essential probabilities
([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md), and
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or its
complement
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or its
complement
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)).

## Usage

``` r
comp_accu_prob(
  prev = prob$prev,
  sens = prob$sens,
  mirt = NA,
  spec = prob$spec,
  fart = NA,
  tol = 0.01,
  w = 0.5
)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (i.e.,
  the probability of condition being `TRUE`, aka. *prior*).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `TRUE`). `sens` is optional when its complement `mirt` is
  provided.

- mirt:

  The decision's miss rate
  [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `TRUE`). `mirt` is optional when its complement `sens` is
  provided.

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `FALSE`). `spec` is optional when its complement `fart`
  is provided.

- fart:

  The decision's false alarm rate
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `FALSE`). `fart` is optional when its complement `spec`
  is provided.

- tol:

  A numeric tolerance value for
  [`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md).
  Default: `tol = .01`.

- w:

  The weighting parameter `w` (from 0 to 1) for computing weighted
  accuracy `wacc`. Default: `w = .50` (i.e., yielding balanced accuracy
  `bacc`).

  Notes:

  - Accuracy metrics describe the *correspondence* of decisions (or
    predictions) to actual conditions (or truth).

    There are several possible interpretations of accuracy:

    1.  as *probabilities* (i.e., `acc` being the proportion of correct
        classifications, or the ratio
        [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)/[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)),

    2.  as *frequencies* (e.g., as classifying a population of
        [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
        individuals into cases of
        [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
        vs.
        [`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)),

    3.  as *correlations* (e.g., see `mcc` in
        [`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md)).

  - Computing exact accuracy values based on probabilities (by
    `comp_accu_prob`) may differ from accuracy values computed from
    (possibly rounded) frequencies (by
    [`comp_accu_freq`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md)).

    When frequencies are rounded to integers (see the default of
    `round = TRUE` in
    [`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
    and
    [`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md))
    the accuracy metrics computed by `comp_accu_freq` correspond to
    these rounded values. Use `comp_accu_prob` to obtain exact accuracy
    metrics from probabilities.

## Value

A list [`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md)
containing current accuracy metrics.

## Details

Currently computed accuracy metrics include:

1.  `acc`: Overall accuracy as the proportion (or probability) of
    correctly classifying cases or of
    [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
    cases:

    \(a\) from
    [`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md):
    `acc = (prev x sens) + [(1 - prev) x spec]`

    \(b\) from
    [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md):
    `acc = dec_cor/N = (hi + cr)/(hi + mi + fa + cr)`

    When frequencies in
    [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) are
    not rounded, (b) coincides with (a).

    Values range from 0 (no correct prediction) to 1 (perfect
    prediction).

2.  `wacc`: Weighted accuracy, as a weighted average of the sensitivity
    [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (aka.
    hit rate
    [`HR`](https://hneth.github.io/riskyr/dev/reference/sens.md),
    [`TPR`](https://hneth.github.io/riskyr/dev/reference/sens.md),
    [`power`](https://hneth.github.io/riskyr/dev/reference/sens.md) or
    [`recall`](https://hneth.github.io/riskyr/dev/reference/sens.md))
    and the the specificity
    [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (aka.
    [`TNR`](https://hneth.github.io/riskyr/dev/reference/spec.md)) in
    which [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md)
    is multiplied by a weighting parameter `w` (ranging from 0 to 1) and
    [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) is
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
    [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) (aka.
    [`precision`](https://hneth.github.io/riskyr/dev/reference/PPV.md))
    and the sensitivity
    [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (aka.
    hit rate
    [`HR`](https://hneth.github.io/riskyr/dev/reference/sens.md),
    [`TPR`](https://hneth.github.io/riskyr/dev/reference/sens.md),
    [`power`](https://hneth.github.io/riskyr/dev/reference/sens.md) or
    [`recall`](https://hneth.github.io/riskyr/dev/reference/sens.md)):

    `f1s = 2 * (PPV * sens) / (PPV + sens)`

    See [Wikipedia: F1 score](https://en.wikipedia.org/wiki/F1_score)
    for additional information.

Note that some accuracy metrics can be interpreted as probabilities
(e.g., `acc`) and some as correlations (e.g., `mcc`).

Also, accuracy can be viewed as a probability (e.g., the ratio of or
link between
[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md) and
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)) or as a
frequency type (containing
[`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md) and
[`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)).

`comp_accu_prob` computes exact accuracy metrics from probabilities.
When input frequencies were rounded (see the default of `round = TRUE`
in
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
and
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md))
the accuracy metrics computed by `comp_accu` correspond these rounded
values.

## References

Consult [Wikipedia: Confusion
matrix](https://en.wikipedia.org/wiki/Confusion_matrix) for additional
information.

## See also

[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) for all
accuracy metrics;
[`comp_accu_freq`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md)
computes accuracy metrics from frequencies;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) for basic
numeric parameters;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) for
current frequency information;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for current
text settings;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) for current
color settings;
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) for a
table of the current population.

Other metrics:
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md),
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md)

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/dev/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/dev/reference/comp_FOR.md),
[`comp_NPV()`](https://hneth.github.io/riskyr/dev/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/dev/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

## Examples

``` r
comp_accu_prob()  # => accuracy metrics for prob of current scenario
#> $acc
#> [1] 0.775
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] 0.8
#> 
#> $mcc
#> [1] 0.5303301
#> 
#> $f1s
#> [1] 0.6538462
#> 
comp_accu_prob(prev = .2, sens = .5, spec = .5)  # medium accuracy, but cr > hi.
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
#> [1] 0.2857143
#> 

# Extreme cases:
comp_accu_prob(prev = NaN, sens = NaN, spec = NaN)  # returns list of NA values
#> Warning: Please enter a valid set of essential probabilities.
#> $acc
#> [1] NA
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] NA
#> 
#> $mcc
#> [1] NA
#> 
#> $f1s
#> [1] NA
#> 
comp_accu_prob(prev = 0, sens = NaN, spec = 1)      # returns list of NA values
#> Warning: Please enter a valid set of essential probabilities.
#> $acc
#> [1] NA
#> 
#> $w
#> [1] 0.5
#> 
#> $wacc
#> [1] NA
#> 
#> $mcc
#> [1] NA
#> 
#> $f1s
#> [1] NA
#> 
comp_accu_prob(prev = 0, sens = 0, spec = 1)     # perfect acc = 1, but f1s is NaN
#> Warning: accu$mcc: A denominator of 0 was corrected to 1, resulting in mcc = 0.
#> $acc
#> [1] 1
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
#> [1] NaN
#> 
comp_accu_prob(prev = .5, sens = .5, spec = .5)  # random performance
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
comp_accu_prob(prev = .5, sens = 1,  spec = 1)   # perfect accuracy
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
comp_accu_prob(prev = .5, sens = 0,  spec = 0)   # zero accuracy, but f1s is NaN
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
comp_accu_prob(prev = 1,  sens = 1,  spec = 0)   # perfect, but see wacc (0.5) and mcc (0)
#> Warning: accu$mcc: A denominator of 0 was corrected to 1, resulting in mcc = 0.
#> $acc
#> [1] 1
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
#> [1] 1
#> 

# Effects of w:
comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 1/2)  # equal weights to sens and spec
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
#> [1] 0.5454545
#> 
comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 2/3)  # more weight on sens: wacc up
#> $acc
#> [1] 0.5
#> 
#> $w
#> [1] 0.6666667
#> 
#> $wacc
#> [1] 0.5333333
#> 
#> $mcc
#> [1] 0
#> 
#> $f1s
#> [1] 0.5454545
#> 
comp_accu_prob(prev = .5, sens = .6, spec = .4, w = 1/3)  # more weight on spec: wacc down
#> $acc
#> [1] 0.5
#> 
#> $w
#> [1] 0.3333333
#> 
#> $wacc
#> [1] 0.4666667
#> 
#> $mcc
#> [1] 0
#> 
#> $f1s
#> [1] 0.5454545
#> 

# Contrasting comp_accu_freq and comp_accu_prob:
# (a) comp_accu_freq (based on rounded frequencies):
freq1 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4)   # => rounded frequencies!
accu1 <- comp_accu_freq(freq1$hi, freq1$mi, freq1$fa, freq1$cr)  # => accu1 (based on rounded freq).
# accu1

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

# A list containing current accuracy information.

`accu` contains current accuracy information returned by the
corresponding generating function
[`comp_accu_prob`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md).

## Usage

``` r
accu
```

## Details

Current metrics include:

1.  [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md):
    Overall accuracy as the probability (or proportion) of correctly
    classifying cases or of
    [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
    cases:

    See [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) for
    definition and explanations.

    [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) values
    range from 0 (no correct prediction) to 1 (perfect prediction).

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

Notes:

- Accuracy metrics describe the *correspondence* of decisions (or
  predictions) to actual conditions (or truth).

  There are several possible interpretations of accuracy:

  1.  as *probabilities* (i.e.,
      [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) being
      the probability or proportion of correct classifications, or the
      ratio
      [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)/[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)),

  2.  as *frequencies* (e.g., as classifying a population of
      [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
      individuals into cases of
      [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
      vs.
      [`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)),

  3.  as *correlations* (e.g., see `mcc` in `accu`).

- Computing exact accuracy values based on probabilities (by
  [`comp_accu_prob`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md))
  may differ from accuracy values computed from (possibly rounded)
  frequencies (by
  [`comp_accu_freq`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md)).

  When frequencies are rounded to integers (see the default of
  `round = TRUE` in
  [`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
  and
  [`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md))
  the accuracy metrics computed by `comp_accu_freq` correspond to these
  rounded values. Use
  [`comp_accu_prob`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md)
  to obtain exact accuracy metrics from probabilities.

## See also

The corresponding generating function
[`comp_accu_prob`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md)
computes exact accuracy metrics from probabilities;
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) defines
accuracy as a probability;
[`comp_accu_freq`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md)
computes accuracy metrics from frequencies;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) for basic
numeric parameters;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) for
current frequency information;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) for
current probability information;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for current
text settings.

Other lists containing current scenario information:
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md),
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md),
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md),
[`pal_bw`](https://hneth.github.io/riskyr/dev/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/dev/reference/pal_bwp.md),
[`pal_kn`](https://hneth.github.io/riskyr/dev/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/dev/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/dev/reference/pal_mod.md),
[`pal_org`](https://hneth.github.io/riskyr/dev/reference/pal_org.md),
[`pal_rgb`](https://hneth.github.io/riskyr/dev/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/dev/reference/pal_unikn.md),
[`pal_vir`](https://hneth.github.io/riskyr/dev/reference/pal_vir.md),
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md),
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/dev/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/dev/reference/txt_org.md)

Other metrics:
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`err`](https://hneth.github.io/riskyr/dev/reference/err.md)

## Examples

``` r
accu <- comp_accu_prob()  # => compute exact accuracy metrics (from probabilities)
accu                      # => current accuracy information
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

## Contrasting comp_accu_freq and comp_accu_prob:
# (a) comp_accu_freq (based on rounded frequencies):
freq1 <- comp_freq(N = 10, prev = 1/3, sens = 2/3, spec = 3/4)   # => rounded frequencies!
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

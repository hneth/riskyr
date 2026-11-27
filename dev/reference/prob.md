# List current probability information.

`prob` is a list of named numeric variables containing 3 essential (1
non-conditional
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) and 2
conditional
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md))
probabilities and 8 derived
([`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md) and
[`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md), as well as
6 conditional) probabilities:

## Usage

``` r
prob
```

## Details

`prob` currently contains the following probabilities:

1.  the condition's prevalence
    [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md)
    (i.e., the probability of the condition being `TRUE`):
    `prev = `[`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)`/`[`N`](https://hneth.github.io/riskyr/dev/reference/N.md).

2.  the decision's sensitivity
    [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md)
    (i.e., the conditional probability of a positive decision provided
    that the condition is `TRUE`).

3.  the decision's miss rate
    [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md)
    (i.e., the conditional probability of a negative decision provided
    that the condition is `TRUE`).

4.  the decision's specificity
    [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)
    (i.e., the conditional probability of a negative decision provided
    that the condition is `FALSE`).

5.  the decision's false alarm rate
    [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)
    (i.e., the conditional probability of a positive decision provided
    that the condition is `FALSE`).

6.  the proportion (baseline probability or rate) of the decision being
    positive
    [`ppod`](https://hneth.github.io/riskyr/dev/reference/ppod.md) (but
    not necessarily true):
    `ppod = `[`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)`/`[`N`](https://hneth.github.io/riskyr/dev/reference/N.md).

7.  the decision's positive predictive value
    [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) (i.e.,
    the conditional probability of the condition being `TRUE` provided
    that the decision is positive).

8.  the decision's false detection (or false discovery) rate
    [`FDR`](https://hneth.github.io/riskyr/dev/reference/FDR.md) (i.e.,
    the conditional probability of the condition being `FALSE` provided
    that the decision is positive).

9.  the decision's negative predictive value
    [`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) (i.e.,
    the conditional probability of the condition being `FALSE` provided
    that the decision is negative).

10. the decision's false omission rate
    [`FOR`](https://hneth.github.io/riskyr/dev/reference/FOR.md) (i.e.,
    the conditional probability of the condition being `TRUE` provided
    that the decision is negative).

11. the accuracy
    [`acc`](https://hneth.github.io/riskyr/dev/reference/acc.md) (i.e.,
    probability of correct decisions
    [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
    or correspondence of decisions to conditions).

12. the conditional probability `p_acc_hi` (i.e., the probability of
    [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md) given
    that the decision is correct
    [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)).

13. the conditional probability `p_err_fa` (i.e., the probability of
    [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) given
    that the decision is erroneous
    [`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)).

These probabilities are computed from basic probabilities (contained in
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md)) and
computed by using
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md).

The list `prob` is the probability counterpart to the list containing
frequency information
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md).

Note that inputs of extreme probabilities (of 0 or 1) may yield
unexpected values (e.g., an
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) value of
NaN when
[`is_extreme_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md)
evaluates to `TRUE`).

Key relationships between frequencies and probabilities (see
documentation of
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
or
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
for details):

- Three perspectives on a population:

  by condition / by decision / by accuracy.

- Defining probabilities in terms of frequencies:

  Probabilities can be computed as ratios between frequencies, but
  beware of rounding issues.

Functions translating between representational formats:
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md),
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
(see documentation of
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
for details).

Visualizations of current probability information are provided by
[`plot_area`](https://hneth.github.io/riskyr/dev/reference/plot_area.md),
[`plot_prism`](https://hneth.github.io/riskyr/dev/reference/plot_prism.md),
and
[`plot_curve`](https://hneth.github.io/riskyr/dev/reference/plot_curve.md).

## See also

[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric parameters;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) contains
current text information;
[`init_txt`](https://hneth.github.io/riskyr/dev/reference/init_txt.md)
initializes text information;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/dev/reference/init_pal.md)
initializes color information;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information; `prob` contains current
probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) contains
current accuracy information.

Other lists containing current scenario information:
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md),
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
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/dev/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/dev/reference/txt_org.md)

## Examples

``` r
prob <- comp_prob()  # initialize prob to default parameters
prob                 # show current values
#> $prev
#> [1] 0.25
#> 
#> $sens
#> [1] 0.85
#> 
#> $mirt
#> [1] 0.15
#> 
#> $spec
#> [1] 0.75
#> 
#> $fart
#> [1] 0.25
#> 
#> $ppod
#> [1] 0.4
#> 
#> $PPV
#> [1] 0.53125
#> 
#> $FDR
#> [1] 0.46875
#> 
#> $NPV
#> [1] 0.9375
#> 
#> $FOR
#> [1] 0.0625
#> 
#> $acc
#> [1] 0.775
#> 
#> $p_acc_hi
#> [1] 0.2741935
#> 
#> $p_err_fa
#> [1] 0.8333333
#> 
length(prob)         # 13 key probabilities (and their values)
#> [1] 13
```

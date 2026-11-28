# List current frequency information.

`freq` is a list of named numeric variables containing 11 key
frequencies (and their values):

## Usage

``` r
freq
```

## Details

1.  the population size
    [`N`](https://hneth.github.io/riskyr/reference/N.md)

2.  the number of cases for which
    [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)

3.  the number of cases for which
    [`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md)

4.  the number of cases for which
    [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)

5.  the number of cases for which
    [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)

6.  the number of cases for which
    [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)

7.  the number of cases for which
    [`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md)

8.  the number of true positives, or hits
    [`hi`](https://hneth.github.io/riskyr/reference/hi.md)

9.  the number of false negatives, or misses
    [`mi`](https://hneth.github.io/riskyr/reference/mi.md)

10. the number of false positives, or false alarms
    [`fa`](https://hneth.github.io/riskyr/reference/fa.md)

11. the number of true negatives, or correct rejections
    [`cr`](https://hneth.github.io/riskyr/reference/cr.md)

These frequencies are computed from basic parameters (contained in
[`num`](https://hneth.github.io/riskyr/reference/num.md)) and computed
by using
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md).

The list `freq` is the frequency counterpart to the list containing
probability information
[`prob`](https://hneth.github.io/riskyr/reference/prob.md).

Natural frequencies are always expressed in relation to the current
population of size [`N`](https://hneth.github.io/riskyr/reference/N.md).

Key relationships between frequencies and probabilities (see
documentation of
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md) or
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md) for
details):

- Three perspectives on a population:

  by condition / by decision / by accuracy.

- Defining probabilities in terms of frequencies:

  Probabilities can be computed as ratios between frequencies, but
  beware of rounding issues.

Functions translating between representational formats:
[`comp_prob_prob`](https://hneth.github.io/riskyr/reference/comp_prob_prob.md),
[`comp_prob_freq`](https://hneth.github.io/riskyr/reference/comp_prob_freq.md),
[`comp_freq_prob`](https://hneth.github.io/riskyr/reference/comp_freq_prob.md),
[`comp_freq_freq`](https://hneth.github.io/riskyr/reference/comp_freq_freq.md)
(see documentation of
[`comp_prob_prob`](https://hneth.github.io/riskyr/reference/comp_prob_prob.md)
for details).

Visualizations of current frequency information are provided by
[`plot_prism`](https://hneth.github.io/riskyr/reference/plot_prism.md)
and
[`plot_icons`](https://hneth.github.io/riskyr/reference/plot_icons.md).

## See also

[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric variables;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric variables;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`txt`](https://hneth.github.io/riskyr/reference/txt.md) contains
current text information;
[`init_txt`](https://hneth.github.io/riskyr/reference/init_txt.md)
initializes text information;
[`pal`](https://hneth.github.io/riskyr/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/reference/init_pal.md)
initializes color information.

Other lists containing current scenario information:
[`accu`](https://hneth.github.io/riskyr/reference/accu.md),
[`num`](https://hneth.github.io/riskyr/reference/num.md),
[`pal`](https://hneth.github.io/riskyr/reference/pal.md),
[`pal_bw`](https://hneth.github.io/riskyr/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/reference/pal_bwp.md),
[`pal_kn`](https://hneth.github.io/riskyr/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/reference/pal_mod.md),
[`pal_org`](https://hneth.github.io/riskyr/reference/pal_org.md),
[`pal_rgb`](https://hneth.github.io/riskyr/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/reference/pal_unikn.md),
[`pal_vir`](https://hneth.github.io/riskyr/reference/pal_vir.md),
[`prob`](https://hneth.github.io/riskyr/reference/prob.md),
[`txt`](https://hneth.github.io/riskyr/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/reference/txt_org.md)

## Examples

``` r
freq <- comp_freq()  # initialize freq to default parameters
freq                 # show current values
#> $N
#> [1] 1000
#> 
#> $cond_true
#> [1] 250
#> 
#> $cond_false
#> [1] 750
#> 
#> $dec_pos
#> [1] 400
#> 
#> $dec_neg
#> [1] 600
#> 
#> $dec_cor
#> [1] 774
#> 
#> $dec_err
#> [1] 226
#> 
#> $hi
#> [1] 212
#> 
#> $mi
#> [1] 38
#> 
#> $fa
#> [1] 188
#> 
#> $cr
#> [1] 562
#> 
length(freq)         # 11 known frequencies
#> [1] 11
names(freq)          # show names of known frequencies
#>  [1] "N"          "cond_true"  "cond_false" "dec_pos"    "dec_neg"   
#>  [6] "dec_cor"    "dec_err"    "hi"         "mi"         "fa"        
#> [11] "cr"        
```

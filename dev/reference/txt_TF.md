# Alternative text labels (TP, FN, FP, TN).

`txt_TF` is initialized to a set of text labels to enable an alternative
frequency naming scheme in which "hi, mi, fa, cr" are labeled as "TP,
FN, FP, TN".

## Usage

``` r
txt_TF
```

## Details

See [`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for
details and default text information.

Assign `txt <- txt_TF` to use as default text labels.

## See also

[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) contains
current text information;
[`init_txt`](https://hneth.github.io/riskyr/dev/reference/init_txt.md)
initializes text information;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/dev/reference/init_pal.md)
initializes color information.

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
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md),
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md),
[`txt_org`](https://hneth.github.io/riskyr/dev/reference/txt_org.md)

## Examples

``` r
txt_TF                     # shows all labels of txt_TF
#> $scen_lbl
#> [1] "Scenario"
#> 
#> $scen_txt
#> [1] "Description of the current scenario in a short paragraph of text. This description may include several sentences."
#> 
#> $scen_src
#> [1] "Source information"
#> 
#> $scen_apa
#> [1] "Source information in APA format"
#> 
#> $scen_lng
#> [1] "en/de"
#> 
#> $popu_lbl
#> [1] "Population"
#> 
#> $N_lbl
#> [1] "N"
#> 
#> $cond_lbl
#> [1] "True condition"
#> 
#> $cond_true_lbl
#> [1] "present"
#> 
#> $cond_false_lbl
#> [1] "absent"
#> 
#> $dec_lbl
#> [1] "Outcome"
#> 
#> $dec_pos_lbl
#> [1] "positive"
#> 
#> $dec_neg_lbl
#> [1] "negative"
#> 
#> $acc_lbl
#> [1] "Accuracy"
#> 
#> $dec_cor_lbl
#> [1] "correct"
#> 
#> $dec_err_lbl
#> [1] "incorrect"
#> 
#> $sdt_lbl
#> [1] "Cases"
#> 
#> $hi_lbl
#> [1] "TP"
#> 
#> $mi_lbl
#> [1] "FN"
#> 
#> $fa_lbl
#> [1] "FP"
#> 
#> $cr_lbl
#> [1] "TN"
#> 
txt_TF["hi_lbl"]           # shows current label for hits ("TP")
#> $hi_lbl
#> [1] "TP"
#> 
txt_TF["hi_lbl"] <- "hit"  # re-label hits (change "TP" to "hit")
```

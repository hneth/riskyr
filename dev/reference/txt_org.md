# List of original values of text elements.

`txt_org` is a copy of the initial list of text elements to define all
scenario titles and labels.

## Usage

``` r
txt_org
```

## Details

See [`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for
details and default text information.

Assign `txt <- txt_org` to re-set default text labels.

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
[`txt_TF`](https://hneth.github.io/riskyr/dev/reference/txt_TF.md)

## Examples

``` r
txt_org        # shows original text labels
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
#> [1] "Condition"
#> 
#> $cond_true_lbl
#> [1] "True"
#> 
#> $cond_false_lbl
#> [1] "False"
#> 
#> $dec_lbl
#> [1] "Decision"
#> 
#> $dec_pos_lbl
#> [1] "Positive"
#> 
#> $dec_neg_lbl
#> [1] "Negative"
#> 
#> $acc_lbl
#> [1] "Accuracy"
#> 
#> $dec_cor_lbl
#> [1] "Correct"
#> 
#> $dec_err_lbl
#> [1] "Erroneous"
#> 
#> $sdt_lbl
#> [1] "Cases"
#> 
#> $hi_lbl
#> [1] "hi"
#> 
#> $mi_lbl
#> [1] "mi"
#> 
#> $fa_lbl
#> [1] "fa"
#> 
#> $cr_lbl
#> [1] "cr"
#> 
txt_org["hi"]  # shows the original label for hits ("hi")
#> $<NA>
#> NULL
#> 
txt_org["hi"] <- "TP" # defines a new label for hits (true positives, TP)
```

# Original color palette.

`pal_org` is a copy of
[`pal`](https://hneth.github.io/riskyr/reference/pal.md) (to retrieve
original set of colors in case
[`pal`](https://hneth.github.io/riskyr/reference/pal.md) is changed).

## Usage

``` r
pal_org
```

## Details

See [`pal`](https://hneth.github.io/riskyr/reference/pal.md) for default
color information.

Assign `pal <- pal_org` to re-set default color scheme throughout the
**riskyr** package.

## See also

[`pal`](https://hneth.github.io/riskyr/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/reference/init_pal.md)
initializes color information.

Other color palettes:
[`pal_bw`](https://hneth.github.io/riskyr/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/reference/pal_bwp.md),
[`pal_crisk`](https://hneth.github.io/riskyr/reference/pal_crisk.md),
[`pal_kn`](https://hneth.github.io/riskyr/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/reference/pal_mod.md),
[`pal_rgb`](https://hneth.github.io/riskyr/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/reference/pal_unikn.md),
[`pal_vir`](https://hneth.github.io/riskyr/reference/pal_vir.md)

Other lists containing current scenario information:
[`accu`](https://hneth.github.io/riskyr/reference/accu.md),
[`freq`](https://hneth.github.io/riskyr/reference/freq.md),
[`num`](https://hneth.github.io/riskyr/reference/num.md),
[`pal`](https://hneth.github.io/riskyr/reference/pal.md),
[`pal_bw`](https://hneth.github.io/riskyr/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/reference/pal_bwp.md),
[`pal_kn`](https://hneth.github.io/riskyr/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/reference/pal_mod.md),
[`pal_rgb`](https://hneth.github.io/riskyr/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/reference/pal_unikn.md),
[`pal_vir`](https://hneth.github.io/riskyr/reference/pal_vir.md),
[`prob`](https://hneth.github.io/riskyr/reference/prob.md),
[`txt`](https://hneth.github.io/riskyr/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/reference/txt_org.md)

## Examples

``` r
pal_org        # shows all color names and current values
#>                 N         cond_true        cond_false           dec_pos 
#>       "#E6E6E6FC" "lightgoldenrod1"   "lightskyblue2"      "rosybrown3" 
#>           dec_neg           dec_cor           dec_err                hi 
#> "lightsteelblue3"      "palegreen3"      "firebrick3"      "olivedrab4" 
#>                mi                fa                cr               ppv 
#>         "tomato3"    "lightsalmon2"      "olivedrab3"         "sienna1" 
#>               npv               txt               brd                bg 
#>      "steelblue3"       "#030303FC"       "#333333FC"           "white" 
pal_org["hi"]  # shows the current color for hits (true positives, TP)
#>           hi 
#> "olivedrab4" 
pal_org["hi"] <- "gold" # defines a new color for hits (true positives, TP)
```

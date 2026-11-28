# Alternative color palette for graphs (with RGB colors).

`pal_rgb` is initialized to a vector of named elements (colors) to
define an alternative (reduced) scenario color scheme (using red, green,
and blue colors).

## Usage

``` r
pal_rgb
```

## Details

See [`pal`](https://hneth.github.io/riskyr/reference/pal.md) for default
color information.

Assign `pal <- pal_rgb` to use as default color scheme throughout the
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
[`pal_org`](https://hneth.github.io/riskyr/reference/pal_org.md),
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
[`pal_org`](https://hneth.github.io/riskyr/reference/pal_org.md),
[`pal_unikn`](https://hneth.github.io/riskyr/reference/pal_unikn.md),
[`pal_vir`](https://hneth.github.io/riskyr/reference/pal_vir.md),
[`prob`](https://hneth.github.io/riskyr/reference/prob.md),
[`txt`](https://hneth.github.io/riskyr/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/reference/txt_org.md)

## Examples

``` r
pal_rgb        # shows all color names and current values
#>              N      cond_true     cond_false        dec_pos        dec_neg 
#>    "#F2F2F2FC"    "#E6E6E6FC"    "#BFBFBFFC"    "#D9D9D9FC"    "#B3B3B3FC" 
#>        dec_cor        dec_err             hi             mi             fa 
#>    "#BFBFBFFC"    "#999999FC"   "olivedrab4"      "tomato3" "lightsalmon2" 
#>             cr            ppv            npv            txt            brd 
#>   "olivedrab3"      "sienna1"   "steelblue3"    "#000000FC"    "#1A1A1AFC" 
#>             bg 
#>        "white" 
pal_rgb["hi"]  # shows the current color for hits (true positives, TP)
#>           hi 
#> "olivedrab4" 
pal_rgb["hi"] <- "gold" # defines a new color for hits (true positives, TP)
```

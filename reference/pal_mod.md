# Modern color palette (in green/blue/orange).

`pal_mod` is initialized to a vector of named colors to define a modern
scenario color scheme (in green/blue/orange).

## Usage

``` r
pal_mod
```

## Details

See [`pal`](https://hneth.github.io/riskyr/reference/pal.md) for default
color information.

Assign `pal <- pal_mod` to use as default color scheme throughout the
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
[`pal_org`](https://hneth.github.io/riskyr/reference/pal_org.md),
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
pal_mod        # shows all color names and current values
#>                 N         cond_true        cond_false           dec_pos 
#>       "#E6E6E6FC" "lightgoldenrod1"       "#BFBFBFFC"         "#F7A97F" 
#>           dec_neg           dec_cor           dec_err                hi 
#>       "#A6A6A6FC"      "olivedrab4"      "steelblue3"         "#B8D989" 
#>                mi                fa                cr               ppv 
#>         "#73C8EA"         "#1D95C6"         "#80B139"         "#F26418" 
#>               npv               txt               brd                bg 
#>         "#1D95C6"       "#000000FC"         "#3E3F3A"           "white" 
pal_mod["hi"]  # shows the current color for hits (true positives, TP)
#>        hi 
#> "#B8D989" 
pal_mod["hi"] <- "gold" # defines a new color for hits (true positives, TP)
```

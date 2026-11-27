# Alternative color palette for unikn.

`pal_unikn` is initialized to a vector of named elements (colors) to
define an alternative (unikn) scenario color scheme.

## Usage

``` r
pal_unikn
```

## Details

See [`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) for
default color information.

Assign `pal <- pal_unikn` to use as default color scheme throughout the
**riskyr** package.

## See also

[`pal_kn`](https://hneth.github.io/riskyr/dev/reference/pal_kn.md)
contains fewer **unikn** colors;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/dev/reference/init_pal.md)
initializes color information.

Other color palettes:
[`pal_bw`](https://hneth.github.io/riskyr/dev/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/dev/reference/pal_bwp.md),
[`pal_crisk`](https://hneth.github.io/riskyr/dev/reference/pal_crisk.md),
[`pal_kn`](https://hneth.github.io/riskyr/dev/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/dev/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/dev/reference/pal_mod.md),
[`pal_org`](https://hneth.github.io/riskyr/dev/reference/pal_org.md),
[`pal_rgb`](https://hneth.github.io/riskyr/dev/reference/pal_rgb.md),
[`pal_vir`](https://hneth.github.io/riskyr/dev/reference/pal_vir.md)

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
[`pal_vir`](https://hneth.github.io/riskyr/dev/reference/pal_vir.md),
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md),
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/dev/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/dev/reference/txt_org.md)

## Examples

``` r
pal_unikn        # shows all color names and current values
#>           N   cond_true  cond_false     dec_pos     dec_neg     dec_cor 
#> "#F2F2F2FF"   "#59C7EB"   "#CCEEF9"   "#ECA0B2"   "#8290BB"   "#54BFB7" 
#>     dec_err          hi          mi          fa          cr         ppv 
#>   "#BC7A8F"   "#6AAAB7"   "#D2A6B4"   "#BC7A8F"   "#9CC6CF"   "#E0607E" 
#>         npv         txt         brd          bg 
#>   "#3E5496" "#000000FF" "#1A1A1AFF"     "white" 
pal_unikn["hi"]  # shows the current color for hits (true positives, TP)
#>        hi 
#> "#6AAAB7" 
pal_unikn["hi"] <- "grey" # defines a new color for hits (true positives, TP)
```

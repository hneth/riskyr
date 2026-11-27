# Alternative color palette for kn.

`pal_kn` is initialized to a vector of named elements (colors) to define
an alternative (kn) scenario color scheme.

## Usage

``` r
pal_kn
```

## Details

See [`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) for
default color information.

Assign `pal <- pal_kn` to use as default color scheme throughout the
**riskyr** package.

## See also

[`pal_unikn`](https://hneth.github.io/riskyr/dev/reference/pal_unikn.md)
contains more **unikn** colors;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/dev/reference/init_pal.md)
initializes color information.

Other color palettes:
[`pal_bw`](https://hneth.github.io/riskyr/dev/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/dev/reference/pal_bwp.md),
[`pal_crisk`](https://hneth.github.io/riskyr/dev/reference/pal_crisk.md),
[`pal_mbw`](https://hneth.github.io/riskyr/dev/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/dev/reference/pal_mod.md),
[`pal_org`](https://hneth.github.io/riskyr/dev/reference/pal_org.md),
[`pal_rgb`](https://hneth.github.io/riskyr/dev/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/dev/reference/pal_unikn.md),
[`pal_vir`](https://hneth.github.io/riskyr/dev/reference/pal_vir.md)

Other lists containing current scenario information:
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md),
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md),
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md),
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md),
[`pal_bw`](https://hneth.github.io/riskyr/dev/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/dev/reference/pal_bwp.md),
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

## Examples

``` r
pal_kn        # shows all color names and current values
#>           N   cond_true  cond_false     dec_pos     dec_neg     dec_cor 
#> "#F2F2F2FC" "#B3B3B3FC" "#D9D9D9FC" "#999999FC" "#BFBFBFFC" "#808080FC" 
#>     dec_err          hi          mi          fa          cr         ppv 
#> "#A6A6A6FC"   "#00A9E0"   "#A6E1F4"   "#CCEEF9"   "#59C7EB"   "#F26418" 
#>         npv         txt         brd          bg 
#>   "#008ECE" "#000000FC" "#1A1A1AFC"     "white" 
pal_kn["hi"]  # shows the current color for hits (true positives, TP)
#>        hi 
#> "#00A9E0" 
pal_kn["hi"] <- "grey" # defines a new color for hits (true positives, TP)
```

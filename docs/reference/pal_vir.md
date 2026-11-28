# Alternative color palette using viridis colors.

`pal_vir` is initialized to a vector of named elements (colors) to
define a scenario color scheme modeled on the `viridis` color scale.

## Usage

``` r
pal_vir
```

## Details

These colors are select by the Matplotlib `viridis` color map created by
Stéfan van der Walt and Nathaniel Smith. See the `viridisLite` package
(maintained by Simon Garnier) for further information.

Assign `pal <- pal_vir` to use as default color scheme throughout the
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
[`pal_rgb`](https://hneth.github.io/riskyr/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/reference/pal_unikn.md)

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
[`pal_rgb`](https://hneth.github.io/riskyr/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/reference/pal_unikn.md),
[`prob`](https://hneth.github.io/riskyr/reference/prob.md),
[`txt`](https://hneth.github.io/riskyr/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/reference/txt_org.md)

## Examples

``` r
pal_vir        # shows all color names and current values
#>           N   cond_true  cond_false     dec_pos     dec_neg     dec_cor 
#> "#B3B3B3FC" "#D9D9D9FC" "#8C8C8CFC" "#CCCCCCFC" "#999999FC" "#E6E6E6FC" 
#>     dec_err          hi          mi          fa          cr         ppv 
#> "#808080FC" "#85D54AFF" "#FDE725FF" "#2D708EFF" "#2BB07FFF" "#51C56AFF" 
#>         npv         txt         brd          bg 
#> "#433E85FF" "#000000FC" "#1A1A1AFC"     "white" 
pal_vir["hi"]  # shows the current color for hits (true positives, TP)
#>          hi 
#> "#85D54AFF" 
pal_vir["hi"] <- "green3" # defines a new color for hits (true positives, TP)
```

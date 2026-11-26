# Alternative color palette for black-and-white (greyscale) graphs.

`pal_bw` is initialized to a vector of named elements (colors) to define
an alternative (black-and-white, b/w) scenario color scheme.

## Usage

``` r
pal_bw
```

## Details

Note that `pal_bw` uses various shades of grey for frequency boxes so
that their bounds remain visible on a white background when `f_lwd = 0`
(as per default for most graphs).

See [`pal_bwp`](https://hneth.github.io/riskyr/dev/reference/pal_bwp.md)
for a stricter version that enforces black text and lines on white boxes
(e.g., for printing purposes).

See [`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) for
default color information.

Assign `pal <- pal_bw` to use as default color scheme throughout the
**riskyr** package.

## See also

[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/dev/reference/init_pal.md)
initializes color information.

Other color palettes:
[`pal_bwp`](https://hneth.github.io/riskyr/dev/reference/pal_bwp.md),
[`pal_crisk`](https://hneth.github.io/riskyr/dev/reference/pal_crisk.md),
[`pal_kn`](https://hneth.github.io/riskyr/dev/reference/pal_kn.md),
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
[`txt_TF`](https://hneth.github.io/riskyr/dev/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/dev/reference/txt_org.md)

## Examples

``` r
pal_bw        # shows all color names and current values
#>           N   cond_true  cond_false     dec_pos     dec_neg     dec_cor 
#> "#F2F2F2FC" "#E6E6E6FC" "#CCCCCCFC" "#D9D9D9FC" "#B3B3B3FC" "#BFBFBFFC" 
#>     dec_err          hi          mi          fa          cr         ppv 
#> "#999999FC" "#CCCCCCFC" "#999999FC" "#808080FC" "#B3B3B3FC" "#999999FC" 
#>         npv         txt         brd          bg 
#> "#737373FC" "#000000FC" "#1A1A1AFC"     "white" 
pal_bw["hi"]  # shows the current color for hits (true positives, TP)
#>          hi 
#> "#CCCCCCFC" 
pal_bw["hi"] <- "gold" # defines a new color for hits (true positives, TP)
```

# Modern and reduced color palette (in green/blue/bw).

`pal_mod` is initialized to a vector of named colors to define a reduced
modern scenario color scheme (in green/blue/bw).

## Usage

``` r
pal_mbw
```

## Details

See [`pal_org`](https://hneth.github.io/riskyr/dev/reference/pal_org.md)
for original color information;
[`pal_mod`](https://hneth.github.io/riskyr/dev/reference/pal_mod.md) for
a richer modern color palette; and
[`pal_bw`](https://hneth.github.io/riskyr/dev/reference/pal_bw.md) for a
more reduced black-and-white color palette.

Assign `pal <- pal_mbw` to use as default color scheme throughout the
**riskyr** package.

## See also

[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/dev/reference/init_pal.md)
initializes color information;
[`pal_org`](https://hneth.github.io/riskyr/dev/reference/pal_org.md) for
original color palette;
[`pal_mod`](https://hneth.github.io/riskyr/dev/reference/pal_mod.md) for
a richer modern color palette;
[`pal_bw`](https://hneth.github.io/riskyr/dev/reference/pal_bw.md) for a
more reduced black-and-white color palette.

Other color palettes:
[`pal_bw`](https://hneth.github.io/riskyr/dev/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/dev/reference/pal_bwp.md),
[`pal_crisk`](https://hneth.github.io/riskyr/dev/reference/pal_crisk.md),
[`pal_kn`](https://hneth.github.io/riskyr/dev/reference/pal_kn.md),
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
[`pal_kn`](https://hneth.github.io/riskyr/dev/reference/pal_kn.md),
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
pal_mbw        # shows all color names and current values
#>            N    cond_true   cond_false      dec_pos      dec_neg      dec_cor 
#>  "#E6E6E6FC"  "#CCCCCCFC"  "#BFBFBFFC"  "#D9D9D9FC"  "#A6A6A6FC" "olivedrab4" 
#>      dec_err           hi           mi           fa           cr          ppv 
#> "steelblue3"    "#B8D989"    "#73C8EA"    "#1D95C6"    "#80B139"    "#F26418" 
#>          npv          txt          brd           bg 
#>    "#1D95C6"  "#000000FC"    "#3E3F3A"      "white" 
pal_mbw["hi"]  # shows the current color for hits (true positives, TP)
#>        hi 
#> "#B8D989" 
pal_mbw["hi"] <- "gold" # defines a new color for hits (true positives, TP)
```

# Alternative color palette for black-and-white graphs (for printing purposes).

`pal_bwp` is initialized to a vector of named elements (colors) to
define a strict (black-and-white, b/w) scenario color scheme that is
suited for printing graphs in black-and-white.

## Usage

``` r
pal_bwp
```

## Details

`pal_bwp` is a stricter version of the greyscale palette
[`pal_bw`](https://hneth.github.io/riskyr/reference/pal_bw.md) that
enforces black text and lines on white boxes. Thus, the bounds of
frequency boxes are invisible on white backgrounds unless the default of
`f_lwd = 0` is changed (e.g., to `f_lwd = 1`).

Some background colors (of frequencies) are also used as foreground
colors (of probabilities, e.g., in
[`plot_curve`](https://hneth.github.io/riskyr/reference/plot_curve.md)
and
[`plot_plane`](https://hneth.github.io/riskyr/reference/plot_plane.md)).
For this reason, the plotting functions detect and adjust colors and/or
line settings when `pal_bwp` is used.

See [`pal_bw`](https://hneth.github.io/riskyr/reference/pal_bw.md) for a
more permissible black-and-white palette that uses various shades of
grey for frequency boxes so that their bounds remain visible on a white
background when `f_lwd = 0` (as per default for most graphs).

See [`pal`](https://hneth.github.io/riskyr/reference/pal.md) for default
color information.

Assign `pal <- pal_bwp` to use as default color scheme throughout the
**riskyr** package.

## See also

[`pal`](https://hneth.github.io/riskyr/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/reference/init_pal.md)
initializes color information.

Other color palettes:
[`pal_bw`](https://hneth.github.io/riskyr/reference/pal_bw.md),
[`pal_crisk`](https://hneth.github.io/riskyr/reference/pal_crisk.md),
[`pal_kn`](https://hneth.github.io/riskyr/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/reference/pal_mod.md),
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
[`pal_kn`](https://hneth.github.io/riskyr/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/reference/pal_mod.md),
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
pal_bwp        # shows all color names and current values
#>          N  cond_true cond_false    dec_pos    dec_neg    dec_cor    dec_err 
#>    "white"    "white"    "white"    "white"    "white"    "white"    "white" 
#>         hi         mi         fa         cr        ppv        npv        txt 
#>    "white"    "white"    "white"    "white"    "black"    "black"    "black" 
#>        brd         bg 
#>    "black"    "white" 
pal_bwp["hi"]  # shows the current color for hits (true positives, TP)
#>      hi 
#> "white" 
pal_bwp["hi"] <- "gold" # defines a new color for hits (true positives, TP)
```

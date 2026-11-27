# Plot a mosaic plot of population frequencies.

`plot_mosaic` drew a mosaic plot that represents the proportions of
frequencies in the current population as relatives sizes of rectangular
areas.

## Usage

``` r
plot_mosaic(
  prev = num$prev,
  sens = num$sens,
  mirt = NA,
  spec = num$spec,
  fart = NA,
  N = num$N,
  by = "cddc",
  show_accu = TRUE,
  w_acc = 0.5,
  title_lbl = txt$scen_lbl,
  col_sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])
)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (i.e.,
  the probability of condition being `TRUE`).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `TRUE`). `sens` is optional when its complement `mirt` is
  provided.

- mirt:

  The decision's miss rate
  [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `TRUE`). `mirt` is optional when its complement `sens` is
  provided.

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `FALSE`). `spec` is optional when its complement `fart`
  is provided.

- fart:

  The decision's false alarm rate
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `FALSE`). `fart` is optional when its complement `spec`
  is provided.

- N:

  The number of individuals in the population.

- by:

  A character code specifying the perspective (or categories by which
  the population is split into subsets) with 3 options:

  1.  `"cddc"` ... by condition x decision;

  2.  `"dccd"` ... by decision x condition;

  3.  `"cdac"` ... by condition x accuracy.

- show_accu:

  Option for showing current and exact accuracy metrics
  [`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md) in the
  plot.

- w_acc:

  Weighting parameter `w` used to compute weighted accuracy.

- title_lbl:

  Text label for current plot title.

- col_sdt:

  Colors for cases of 4 essential frequencies. Default:
  `col_sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"])`.

## Details

`plot_mosaic` is deprecated – please use
[`plot_area`](https://hneth.github.io/riskyr/dev/reference/plot_area.md)
instead.

## See also

[`plot_area`](https://hneth.github.io/riskyr/dev/reference/plot_area.md)
is the new version of this function.

Other visualization functions:
[`plot.riskyr()`](https://hneth.github.io/riskyr/dev/reference/plot.riskyr.md),
[`plot_area()`](https://hneth.github.io/riskyr/dev/reference/plot_area.md),
[`plot_bar()`](https://hneth.github.io/riskyr/dev/reference/plot_bar.md),
[`plot_crisk()`](https://hneth.github.io/riskyr/dev/reference/plot_crisk.md),
[`plot_curve()`](https://hneth.github.io/riskyr/dev/reference/plot_curve.md),
[`plot_fnet()`](https://hneth.github.io/riskyr/dev/reference/plot_fnet.md),
[`plot_icons()`](https://hneth.github.io/riskyr/dev/reference/plot_icons.md),
[`plot_plane()`](https://hneth.github.io/riskyr/dev/reference/plot_plane.md),
[`plot_prism()`](https://hneth.github.io/riskyr/dev/reference/plot_prism.md),
[`plot_tab()`](https://hneth.github.io/riskyr/dev/reference/plot_tab.md),
[`plot_tree()`](https://hneth.github.io/riskyr/dev/reference/plot_tree.md)

## Examples

``` r
plot_mosaic()  # plot with default options
#> Function 'plot_mosaic' is deprecated; using 'plot_area' instead.

```

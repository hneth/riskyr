# Plot a tree diagram of frequencies and probabilities.

`plot_tree` drew a tree diagram of frequencies (as nodes) and
probabilities (as edges).

## Usage

``` r
plot_tree(
  prev = num$prev,
  sens = num$sens,
  mirt = NA,
  spec = num$spec,
  fart = NA,
  N = freq$N,
  round = TRUE,
  by = "cd",
  area = "no",
  p_lbl = "num",
  show_accu = TRUE,
  w_acc = 0.5,
  title_lbl = txt$scen_lbl,
  popu_lbl = txt$popu_lbl,
  cond_true_lbl = txt$cond_true_lbl,
  cond_false_lbl = txt$cond_false_lbl,
  dec_pos_lbl = txt$dec_pos_lbl,
  dec_neg_lbl = txt$dec_neg_lbl,
  hi_lbl = txt$hi_lbl,
  mi_lbl = txt$mi_lbl,
  fa_lbl = txt$fa_lbl,
  cr_lbl = txt$cr_lbl,
  col_txt = grey(0.01, alpha = 0.99),
  cex_lbl = 0.85,
  col_boxes = pal,
  col_border = grey(0.33, alpha = 0.99),
  lwd = 1.5,
  box_lwd = 1.5,
  col_shadow = grey(0.11, alpha = 0.99),
  cex_shadow = 0
)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/reference/prev.md) (i.e., the
  probability of condition being `TRUE`, aka. *prior*).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/reference/sens.md) (i.e., the
  conditional probability of a positive decision provided that the
  condition is `TRUE`). `sens` is optional when its complement `mirt` is
  provided.

- mirt:

  The decision's miss rate
  [`mirt`](https://hneth.github.io/riskyr/reference/mirt.md) (i.e., the
  conditional probability of a negative decision provided that the
  condition is `TRUE`). `mirt` is optional when its complement `sens` is
  provided.

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/reference/spec.md) (i.e., the
  conditional probability of a negative decision provided that the
  condition is `FALSE`). `spec` is optional when its complement `fart`
  is provided.

- fart:

  The decision's false alarm rate
  [`fart`](https://hneth.github.io/riskyr/reference/fart.md) (i.e., the
  conditional probability of a positive decision provided that the
  condition is `FALSE`). `fart` is optional when its complement `spec`
  is provided.

- N:

  The number of individuals in the population.

- round:

  A Boolean option specifying whether computed frequencies are rounded
  to integers. Default: `round = TRUE`.

- by:

  A character code specifying the perspective (or category by which the
  population is split into subsets) with 3 options:

  1.  `"cd"` ... by condition;

  2.  `"dc"` ... by decision;

  3.  `"ac"` ... by accuracy.

- area:

  A character code specifying the area of the boxes (or their relative
  sizes) with 3 options:

  1.  `"no"` ... all boxes are shown with the same size;

  2.  `"sq"` ... boxes are squares with area sizes scaled proportional
      to frequencies (default);

  3.  `"hr"` ... boxes are horizontal rectangles with area sizes scaled
      proportional to frequencies.

- p_lbl:

  A character code specifying the type of probability information (on
  edges) with 4 options:

  1.  `"nam"` ... names of probabilities;

  2.  `"num"` ... numeric values of probabilities (rounded to 3
      decimals, default);

  3.  `"mix"` ... names of essential probabilities, values of
      complements;

  4.  `"min"` ... minimal labels: names of essential probabilities.

- show_accu:

  Option for showing current accuracy metrics
  [`accu`](https://hneth.github.io/riskyr/reference/accu.md) on the
  margin of the plot.

- w_acc:

  Weighting parameter `w` used to compute weighted accuracy `w_acc` in
  [`comp_accu_freq`](https://hneth.github.io/riskyr/reference/comp_accu_freq.md).

  Various other options allow the customization of text labels and
  colors:

- title_lbl:

  Text label for current plot title.

- popu_lbl:

  Text label for current population
  [`popu`](https://hneth.github.io/riskyr/reference/popu.md).

- cond_true_lbl:

  Text label for current cases of
  [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md).

- cond_false_lbl:

  Text label for current cases of
  [`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md).

- dec_pos_lbl:

  Text label for current cases of
  [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md).

- dec_neg_lbl:

  Text label for current cases of
  [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md).

- hi_lbl:

  Text label for hits
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md).

- mi_lbl:

  Text label for misses
  [`mi`](https://hneth.github.io/riskyr/reference/mi.md).

- fa_lbl:

  Text label for false alarms
  [`fa`](https://hneth.github.io/riskyr/reference/fa.md).

- cr_lbl:

  Text label for correct rejections
  [`cr`](https://hneth.github.io/riskyr/reference/cr.md).

- col_txt:

  Color for text labels (in boxes).

- cex_lbl:

  Scaling factor for text labels (in boxes and on arrows).

- col_boxes:

  Colors of boxes (a single color or a vector with named colors matching
  the number of current boxes). Default: Current color information
  contained in [`pal`](https://hneth.github.io/riskyr/reference/pal.md).

- col_border:

  Color of borders. Default: `col_border = grey(.33, alpha = .99)`.

- lwd:

  Width of arrows.

- box_lwd:

  Width of boxes.

- col_shadow:

  Color of box shadows. Default: `col_shadow = grey(.11, alpha = .99)`.

- cex_shadow:

  Scaling factor of shadows (values \> 0 showing shadows). Default:
  `cex_shadow = 0`.

## Value

Nothing (NULL).

## Details

`plot_tree` is deprecated – please use
[`plot_prism`](https://hneth.github.io/riskyr/reference/plot_prism.md)
instead.

## See also

[`plot_prism`](https://hneth.github.io/riskyr/reference/plot_prism.md)
is the new version of this function.

Other visualization functions:
[`plot.riskyr()`](https://hneth.github.io/riskyr/reference/plot.riskyr.md),
[`plot_area()`](https://hneth.github.io/riskyr/reference/plot_area.md),
[`plot_bar()`](https://hneth.github.io/riskyr/reference/plot_bar.md),
[`plot_crisk()`](https://hneth.github.io/riskyr/reference/plot_crisk.md),
[`plot_curve()`](https://hneth.github.io/riskyr/reference/plot_curve.md),
[`plot_fnet()`](https://hneth.github.io/riskyr/reference/plot_fnet.md),
[`plot_icons()`](https://hneth.github.io/riskyr/reference/plot_icons.md),
[`plot_mosaic()`](https://hneth.github.io/riskyr/reference/plot_mosaic.md),
[`plot_plane()`](https://hneth.github.io/riskyr/reference/plot_plane.md),
[`plot_prism()`](https://hneth.github.io/riskyr/reference/plot_prism.md),
[`plot_tab()`](https://hneth.github.io/riskyr/reference/plot_tab.md)

## Examples

``` r
plot_tree()  # frequency tree with current default options (by = "cd")
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.


# alternative perspectives:
plot_tree(by = "dc")  # tree by decision
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.

plot_tree(by = "ac")  # tree by accuracy
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.


# See plot_prism for details and additional options.
```

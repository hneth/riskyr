# Plot an icon array of a population.

`plot_icons` plots a population of which individual's condition has been
classified correctly or incorrectly as icons from a sufficient and valid
set of 3 essential probabilities
([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md), and
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or its
complement
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or its
complement
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)) or
existing frequency information
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) and a
population size of
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) individuals.

## Usage

``` r
plot_icons(
  prev = num$prev,
  sens = num$sens,
  mirt = NA,
  spec = num$spec,
  fart = NA,
  N = freq$N,
  sample = FALSE,
  arr_type = "array",
  by = "all",
  ident_order = c("hi", "mi", "fa", "cr"),
  icon_types = 22,
  icon_size = NULL,
  icon_brd_lwd = 1.5,
  block_d = NULL,
  border_d = 0.1,
  block_size_row = 10,
  block_size_col = 10,
  nblocks_row = NULL,
  nblocks_col = NULL,
  fill_array = "left",
  fill_blocks = "rowwise",
  lbl_txt = txt,
  main = txt$scen_lbl,
  sub = "type",
  title_lbl = NULL,
  cex_lbl = 0.9,
  col_pal = pal,
  transparency = 0.5,
  mar_notes = FALSE,
  ...
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

  The decision's specificity value
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

  The number of individuals in the population. A suitable value of
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) is computed,
  if not provided. If N is 100,000 or greater it is reduced to 10,000
  for the array types if the frequencies allow it.

- sample:

  Boolean value that determines whether frequency values are sampled
  from `N`, given the probability values of `prev`, `sens`, and `spec`.
  Default: `sample = FALSE`.

- arr_type:

  The icons can be arranged in different ways resulting in different
  types of displays:

  1.  `arr_type = "array"`: Icons are plotted in a classical icon array
      (default). Icons can be arranged in blocks using `block_d`. The
      order of filling the array can be customized using `fill_array`
      and `fill_blocks`.

  2.  `arr_type = "shuffledarray"`: Icons are plotted in an icon array,
      but positions are shuffled (randomized). Icons can be arranged in
      blocks using `block_d`. The order of filling the array can be
      customized using `fill_array` and `fill_blocks`.

  3.  `arr_type = "mosaic"`: Icons are ordered like in a mosaic plot.
      The area size displays the relative proportions of their
      frequencies.

  4.  `arr_type = "fillequal"`: Icons are positioned into equally sized
      blocks. Thus, their density reflects the relative proportions of
      their frequencies.

  5.  `arr_type = "fillleft"`: Icons are randomly filled from the left.

  6.  `arr_type = "filltop"`: Icons are randomly filled from the top.

  7.  `arr_type = "scatter"`: Icons are randomly scattered into the
      plot.

- by:

  A character code specifying a perspective to split the population into
  subsets, with 4 options:

  1.  `"all"`: by condition (cd) and by decision (dc):

      [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
      [`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
      [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
      [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) cases
      (default);

  2.  `"cd"`: by condition (cd) only:

      [`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)
      vs.
      [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
      cases;

  3.  `"dc"`: by decision (dc) only:

      [`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)
      vs.
      [`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)
      cases;

  4.  `"ac"`: by accuracy (ac) only:

      [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
      vs.
      [`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)
      cases.

- ident_order:

  The order in which icon identities (i.e., hi, mi, fa, and cr) are
  plotted. Default: `ident_order = c("hi", "mi", "fa", "cr")`

- icon_types:

  specifies the appearance of the icons as a vector. Default:
  `icon_types = 11` (i.e., squares with border). Accepts values from 1
  to 25 (see [`?points`](https://rdrr.io/r/graphics/points.html)).

- icon_size:

  specifies the size of the icons via `cex` Default: `icon_size = NULL`
  for automatic calculation.

- icon_brd_lwd:

  specifies the border width of icons (if applicable). Default:
  `icon_brd_lwd = 1.5`. Set to `NA` for no border.

- block_d:

  The distance between blocks. Default: `block_d = NULL` for automatic
  calculation; (does not apply to "filleft", "filltop", and "scatter")

- border_d:

  The distance of icons to the border. Default: `border_d = 0.1`.

  Additional options for controlling the arrangement of arrays (for
  `arr_type = "array"` and `"shuffledarray"`):

- block_size_row:

  specifies how many icons should be in each block row. Default:
  `block_size_row = 10`.

- block_size_col:

  specifies how many icons should be in each block column. Default:
  `block_size_col = 10`.

- nblocks_row:

  Number of blocks per row. Default: `nblocks_row = NULL` for automatic
  calculation.

- nblocks_col:

  Number of blocks per column. Default: `nblocks_col = NULL` for
  automatic calculation.

- fill_array:

  specifies how the blocks are filled into the array. Options:
  `fill_array = "left"` (default) vs. `"top"`.

- fill_blocks:

  specifies how icons within blocks are filled. Options:
  `fill_blocks = "rowwise"` (default) and `"colwise"`.

  Generic text and color options:

- lbl_txt:

  Default label set for text elements. Default:
  `lbl_txt = `[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md).

- main:

  Text label for main plot title. Default: `main = txt$scen_lbl`.

- sub:

  Text label for plot subtitle (on 2nd line). Default: `sub = "type"`
  shows information on current plot type.

- title_lbl:

  **Deprecated** text label for current plot title. Replaced by `main`.

- cex_lbl:

  Scaling factor for text labels. Default: `cex_lbl = .90`.

- col_pal:

  Color palette. Default:
  `col_pal = `[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md).

- transparency:

  Specifies the transparency for overlapping icons (not for
  `arr_type = "array"` and `"shuffledarray"`).

- mar_notes:

  Boolean option for showing margin notes. Default: `mar_notes = FALSE`.

- ...:

  Other (graphical) parameters.

## Value

Nothing (NULL).

## Details

If probabilities are provided, a new list of natural frequencies
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) is
computed by
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md).
By contrast, if no probabilities are provided, the values currently
contained in
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) are used.
By default,
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
rounds frequencies to nearest integers to avoid decimal values in
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md).

## See also

Other visualization functions:
[`plot.riskyr()`](https://hneth.github.io/riskyr/dev/reference/plot.riskyr.md),
[`plot_area()`](https://hneth.github.io/riskyr/dev/reference/plot_area.md),
[`plot_bar()`](https://hneth.github.io/riskyr/dev/reference/plot_bar.md),
[`plot_crisk()`](https://hneth.github.io/riskyr/dev/reference/plot_crisk.md),
[`plot_curve()`](https://hneth.github.io/riskyr/dev/reference/plot_curve.md),
[`plot_fnet()`](https://hneth.github.io/riskyr/dev/reference/plot_fnet.md),
[`plot_mosaic()`](https://hneth.github.io/riskyr/dev/reference/plot_mosaic.md),
[`plot_plane()`](https://hneth.github.io/riskyr/dev/reference/plot_plane.md),
[`plot_prism()`](https://hneth.github.io/riskyr/dev/reference/plot_prism.md),
[`plot_tab()`](https://hneth.github.io/riskyr/dev/reference/plot_tab.md),
[`plot_tree()`](https://hneth.github.io/riskyr/dev/reference/plot_tree.md)

## Examples

``` r
# Basics:
plot_icons(N = 1000)  # icon array with default settings (arr_type = "array")

plot_icons(arr_type = "shuffledarray", N = 1000)  # icon array with shuffled IDs


# Sampling:
plot_icons(N = 1000, prev = 1/2, sens = 2/3, spec = 6/7, sample = TRUE)


# array types:
plot_icons(arr_type = "mosaic",    N = 1000)  # areas as in mosaic plot

plot_icons(arr_type = "fillequal", N = 1000)  # areas of equal size (probability as density)

plot_icons(arr_type = "fillleft",  N = 1000)  # icons filled from left to right (in columns)

plot_icons(arr_type = "filltop",   N = 1000)  # icons filled from top to bottom (in rows)

plot_icons(arr_type = "scatter",   N = 1000)  # icons randomly scattered


# by:
plot_icons(N = 1000, by = "all")  # hi, mi, fa, cr (TP, FN, FP, TN) cases

plot_icons(N = 1000, by = "cd", main = "Cases by condition")  # (hi + mi) vs. (fa + cr)

plot_icons(N = 1000, by = "dc", main = "Cases by decision")   # (hi + fa) vs. (mi + cr)

plot_icons(N = 1000, by = "ac", main = "Cases by accuracy")   # (hi + cr) vs. (fa + mi)


# Custom icon types and colors:
plot_icons(N = 800, arr_type = "array", icon_types = c(21, 22, 23, 24),
           block_d = 0.5, border_d = 0.5, col_pal = pal_vir)


plot_icons(N = 800, arr_type = "shuffledarray", icon_types = c(21, 23, 24, 22),
           block_d = 0.5, border_d = 0.5)


plot_icons(N = 800, arr_type = "fillequal", icon_types = c(21, 22, 22, 21),
           icon_brd_lwd = .5, cex = 1, cex_lbl = 1.1)


# Text and color options:
plot_icons(N = 1000, prev = .5, sens = .5, spec = .5, arr_type = "shuffledarray",
           main = "My title", sub = NA, lbl_txt = txt_TF, col_pal = pal_vir, mar_notes = TRUE)


plot_icons(N = 1000, prev = .5, sens = .5, spec = .5, arr_type = "shuffledarray",
           main = "Green vs. red", col_pal = pal_rgb, transparency = .5)

```

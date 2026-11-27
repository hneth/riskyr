# Plot a 2 x 2 contingency table of population frequencies.

`plot_tab` plots a 2 x 2 contingency table (aka. confusion table) of 4
classification cases
([`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)) and
corresponding row and column sums.

## Usage

``` r
plot_tab(
  prev = num$prev,
  sens = num$sens,
  mirt = NA,
  spec = num$spec,
  fart = NA,
  N = num$N,
  by = "cddc",
  p_split = "v",
  area = "no",
  scale = "p",
  round = TRUE,
  sample = FALSE,
  f_lbl = "num",
  f_lbl_sep = NA,
  f_lbl_sum = f_lbl,
  f_lbl_hd = "nam",
  f_lwd = 0,
  gaps = c(NA, NA),
  brd_w = 0.1,
  p_lbl = NA,
  arr_c = -3,
  col_p = c(grey(0.15, 0.99), "yellow", "yellow"),
  brd_dis = 0.3,
  lbl_txt = txt,
  main = txt$scen_lbl,
  sub = "type",
  title_lbl = NULL,
  cex_lbl = 0.9,
  cex_p_lbl = NA,
  col_pal = pal,
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

  The number of individuals in the population. A suitable value of
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) is computed,
  if not provided. Note:
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) is not
  represented in the plot, but used for computing frequency information
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) from
  current probabilities
  [`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md).

- by:

  A character code specifying 2 perspectives that split the population
  into subsets, with 6 options:

  1.  `"cddc"`: by condition (cd) and by decision (dc) (default);

  2.  `"cdac"`: by condition (cd) and by accuracy (ac);

  3.  `"dccd"`: by decision (dc) and by condition (cd);

  4.  `"dcac"`: by decision (dc) and by accuracy (ac);

  5.  `"accd"`: by accuracy (ac) and by condition (cd);

  6.  `"acdc"`: by accuracy (ac) and by decision (dc).

- p_split:

  Primary perspective for population split, with 2 options:

  1.  `"v"`: vertical (default);

  2.  `"h"`: horizontal.

  Note: In contrast to
  [`plot_area`](https://hneth.github.io/riskyr/dev/reference/plot_area.md),
  this setting only determines which 3 probability links are shown
  (e.g., when `p_link = "def"`).

- area:

  A character code specifying the shape of the main area, with 4
  options:

  1.  `"sq"`: main area is scaled to square;

  2.  `"no"`: no scaling (rectangular area fills plot size; default).

- scale:

  Scale probabilities (but not table cell dimensions) either by exact
  probability or by (rounded or non-rounded) frequency, with 2 options:

  1.  `"p"`: scale main area dimensions by exact probability (default);

  2.  `"f"`: re-compute probabilities from (rounded or non-rounded)
      frequencies and scale main area dimensions by their frequency.

  Note: `scale` setting matters for the display of probability values
  and for area plots with small population sizes
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) when
  `round = TRUE`.

- round:

  A Boolean option specifying whether computed frequencies are rounded
  to integers. Default: `round = TRUE`.

- sample:

  Boolean value that determines whether frequency values are sampled
  from `N`, given the probability values of `prev`, `sens`, and `spec`.
  Default: `sample = FALSE`.

- f_lbl:

  Type of label for showing frequency values in 4 main areas, with 6
  options:

  1.  `"def"`: abbreviated names and frequency values (default);

  2.  `"abb"`: abbreviated frequency names only (as specified in code);

  3.  `"nam"`: names only (as specified in `lbl_txt = txt`);

  4.  `"num"`: numeric frequency values only;

  5.  `"namnum"`: names (as specified in `lbl_txt = txt`) and numeric
      values;

  6.  `"no"`: no frequency labels (same for `f_lbl = NA` or `NULL`).

- f_lbl_sep:

  Label separator for main frequencies (used for
  `f_lbl = "def" OR "namnum"`). Use `f_lbl_sep = ":\n"` to add a line
  break between name and numeric value. Default: `f_lbl_sep = NA` (set
  to `" = "` or `":\n"` based on `f_lbl`).

- f_lbl_sum:

  Type of label for showing frequency values in summary cells, with same
  6 options as `f_lbl` (above). Default: `f_lbl_sum = "def"`:
  abbreviated names and numeric values.

- f_lbl_hd:

  Type of label for showing frequency values in header, with same 6
  options as `f_lbl` (above). Default: `f_lbl_hd = "nam"`: names only
  (as specified in `lbl_txt = txt`).

- f_lwd:

  Line width of areas. Default: `f_lwd = 1`.

- gaps:

  Size of gaps (as binary numeric vector) specifying the widths of
  vertical and horizontal gaps between 2 x 2 table and sums (in bottom
  row and right column). Default: `gaps = c(.05, .06)`.

- brd_w:

  Border width for showing 2 perspective summaries on top and left
  borders of main area (as a proportion of area size) in a range
  `0 <= brd_w <= 1`. Default: `brd_w = .10`.

- p_lbl:

  Type of label for showing 3 key probability links and values, with 7
  options:

  1.  `"def"`: show links and abbreviated names and probability values;

  2.  `"abb"`: show links and abbreviated probability names;

  3.  `"nam"`: show links and probability names (as specified in code);

  4.  `"num"`: show links and numeric probability values;

  5.  `"namnum"`: show links with names and numeric probability values;

  6.  `"no"`: show links with no labels;

  7.  `NA`: no link (same for `p_lbl = NULL`, default).

- arr_c:

  Arrow code for symbols at ends of probability links (as a numeric
  value `-3 <= arr_c <= +6`), with the following options:

  - `-1` to `-3`: points at one/other/both end/s;

  - `0`: no symbols;

  - `+1` to `+3`: V-arrow at one/other/both end/s;

  - `+4` to `+6`: T-arrow at one/other/both end/s.

  Default: `arr_c = -3` (points at both ends).

- col_p:

  Colors of probability links (as vector of 3 colors). Default:
  `col_p = c(grey(.15, .99), "yellow", "yellow")`.

- brd_dis:

  Distance of probability links from cell center (as a constant).
  Default: `brd_dis = .30`. Note: Adjust to avoid overlapping labels.

- lbl_txt:

  Default label set for text elements. Default:
  `lbl_txt = `[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md).

- main:

  Text label for main plot title. Default: `main = txt$scen_lbl`.

- sub:

  Text label for the subtitle of the plot (shown below the `main`
  title). Default: `sub = "type"` shows information on current plot
  type.

- title_lbl:

  **Deprecated** text label for current plot title. Replaced by `main`.

- cex_lbl:

  Scaling factor for text labels (frequencies and headers). Default:
  `cex_lbl = .90`.

- cex_p_lbl:

  Scaling factor for text labels (probabilities). Default:
  `cex_p_lbl = cex_lbl - .05`.

- col_pal:

  Color palette. Default:
  `col_pal = `[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md).

- mar_notes:

  Boolean option for showing margin notes. Default: `mar_notes = FALSE`.

- ...:

  Other (graphical) parameters.

## Value

Nothing (NULL).

## Details

`plot_tab` computes its frequencies
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) from a
sufficient and valid set of 3 essential probabilities
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

`plot_tab` is derived from
[`plot_area`](https://hneth.github.io/riskyr/dev/reference/plot_area.md),
but does not scale the dimensions of table cells.

## See also

[`plot_area`](https://hneth.github.io/riskyr/dev/reference/plot_area.md)
for plotting mosaic plot (scaling area dimensions);
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color settings;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) contains
current text settings.

Other visualization functions:
[`plot.riskyr()`](https://hneth.github.io/riskyr/dev/reference/plot.riskyr.md),
[`plot_area()`](https://hneth.github.io/riskyr/dev/reference/plot_area.md),
[`plot_bar()`](https://hneth.github.io/riskyr/dev/reference/plot_bar.md),
[`plot_crisk()`](https://hneth.github.io/riskyr/dev/reference/plot_crisk.md),
[`plot_curve()`](https://hneth.github.io/riskyr/dev/reference/plot_curve.md),
[`plot_fnet()`](https://hneth.github.io/riskyr/dev/reference/plot_fnet.md),
[`plot_icons()`](https://hneth.github.io/riskyr/dev/reference/plot_icons.md),
[`plot_mosaic()`](https://hneth.github.io/riskyr/dev/reference/plot_mosaic.md),
[`plot_plane()`](https://hneth.github.io/riskyr/dev/reference/plot_plane.md),
[`plot_prism()`](https://hneth.github.io/riskyr/dev/reference/plot_prism.md),
[`plot_tree()`](https://hneth.github.io/riskyr/dev/reference/plot_tree.md)

## Examples

``` r
## Basics:
# (1) Plotting global freq and prob values:
plot_tab()

plot_tab(area = "sq", f_lwd = 3, col_pal = pal_rgb)

plot_tab(f_lbl = "namnum", f_lbl_sep = " = ", brd_w = .10, f_lwd = .5)


# (2) Computing local freq and prob values:
plot_tab(prev = .5, sens = 4/5, spec = 3/5, N = 10, f_lwd = 1)


# (3) Rounding and sampling:
plot_tab(N = 100, prev = 1/3, sens = 2/3, spec = 6/7, round = FALSE)

plot_tab(N = 100, prev = 1/3, sens = 2/3, spec = 6/7, sample = TRUE)


## Plot versions:
# by x p_split [yields (3 x 2) x 2] = 12 versions]:
plot_tab(by = "cddc", p_split = "v", p_lbl = "def")  # v01 (see v07)

plot_tab(by = "cdac", p_split = "v", p_lbl = "def")  # v02 (see v11)

plot_tab(by = "cddc", p_split = "h", p_lbl = "def")  # v03 (see v05)

plot_tab(by = "cdac", p_split = "h", p_lbl = "def")  # v04 (see v09)


# plot_tab(by = "dccd", p_split = "h", p_lbl = "def")  # v07 (v01 rotated)
# plot_tab(by = "dccd", p_split = "v", p_lbl = "def")  # v05 (v03 rotated)
plot_tab(by = "dcac", p_split = "v", p_lbl = "def")    # v06 (see v12)

plot_tab(by = "dcac", p_split = "h", p_lbl = "def")    # v08 (see v10)


# plot_tab(by = "accd", p_split = "v", p_lbl = "def")  # v09 (v04 rotated)
# plot_tab(by = "acdc", p_split = "v", p_lbl = "def")  # v10 (v08 rotated)
# plot_tab(by = "accd", p_split = "h", p_lbl = "def")  # v11 (v02 rotated)
# plot_tab(by = "acdc", p_split = "h", p_lbl = "def")  # v12 (v06 rotated)

## Explore labels and links:
# plot_tab(f_lbl = "abb", p_lbl = NA)  # abbr. labels, no probability links
# plot_tab(f_lbl = "num", f_lbl_sum = "abb", p_lbl = "num", f_lbl_hd = "abb")
plot_tab(f_lbl = "def", f_lbl_sum = "def", p_lbl = "def", f_lbl_hd = "nam")

plot_tab(f_lbl = "namnum", f_lbl_sep = " = ",
         f_lbl_sum = "namnum", f_lbl_hd = "num", p_lbl = "namnum")


## Misc. options:
plot_tab(area = "sq")        # area: square

# plot_tab(main = "")     # no titles
# plot_tab(mar_notes = TRUE)   # show margin notes
plot_tab(by = "cddc", gaps = c(.08, .00), area = "sq")      # gaps

# plot_tab(by = "cddc", gaps = c(.02, .08), p_split = "h")  # gaps

# Showing prob as lines:
plot_tab(prev = 1/4, sens = 6/7, spec = 3/5, N = 100,
         by = "cddc", p_split = "v", col_pal = pal_rgb,
         p_lbl = "def", brd_dis = .25, arr_c = +3, lwd = 2)


# Custom text labels and colors:
plot_tab(prev = .5, sens = 4/5, spec = 3/5, N = 10,
         by = "cddc", p_split = "v", area = "no",
         main = "Main title", sub = "The subtitle", lbl_txt = txt_TF,  # custom text
         f_lbl = "namnum", f_lbl_sep = ":\n", f_lbl_sum = "num", f_lbl_hd  = "nam",
         col_pal = pal_vir, f_lwd = 3)  # custom colors

plot_tab(prev = .5, sens = 3/5, spec = 4/5, N = 10,
         by = "cddc", p_split = "h", area = "sq",
         main = NA, sub = NA, lbl_txt = txt_org,  # custom text
         f_lbl = "namnum", f_lbl_sep = ":\n", f_lbl_sum = "num", f_lbl_hd  = "nam",
         col_pal = pal_kn, f_lwd = 1)  # custom colors


## Note some differences to plot_area (i.e., area/mosaic plot):
# In plot_tab:
# (1) p_split does not matter (except for selecting different prob links):
plot_tab(by = "cddc", p_split = "v")  # v01 (see v07)

plot_tab(by = "cddc", p_split = "h")  # v03 (see v05)

# (2) scale does not matter for dimensions (which are constant),
#     BUT matters for values shown in prob links and on margins:
plot_tab(N = 5, prev = .3, sens = .9, spec = .5,
         by = "cddc", scale = "p", p_lbl = "def", round = TRUE)  # (a) exact prob values

plot_tab(N = 5, prev = .3, sens = .9, spec = .5,
         by = "cddc", scale = "f", p_lbl = "def", round = TRUE)  # (b) prob from rounded freq!

plot_tab(N = 5, prev = .3, sens = .9, spec = .5,
         by = "cddc", scale = "f", p_lbl = "def", round = FALSE) # (c) same values as (a)

```

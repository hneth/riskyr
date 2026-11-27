# Plot a plane of selected values (e.g., PPV or NPV) as a function of sensitivity and specificity.

`plot_plane` draws a 3D-plane of selected values (e.g., predictive
values [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) or
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md)) as a
function of a decision's sensitivity
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) and
specificity value
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) for a
given prevalence
([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md)).

## Usage

``` r
plot_plane(
  prev = num$prev,
  sens = num$sens,
  mirt = NA,
  spec = num$spec,
  fart = NA,
  what = "PPV",
  what_col = pal,
  line_col = "grey85",
  sens_range = c(0, 1),
  spec_range = c(0, 1),
  step_size = 0.05,
  show_points = TRUE,
  point_col = "yellow",
  theta = -45,
  phi = 0,
  p_lbl = "def",
  lbl_txt = txt,
  main = txt$scen_lbl,
  sub = "type",
  title_lbl = NULL,
  cex_lbl = 0.85,
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

- what:

  A character code that specifies one metric to be plotted as a plane.
  Currently available options are `c("PPV", "NPV", "ppod", "acc")`.
  Default: `what = "PPV"`.

- what_col:

  Color for surface facets corresponding to the metric specified in
  `what`. Default: `what_col` uses color corresponding to `what` in
  current `col_pal`.

- line_col:

  Color for lines between surface facets. Default:
  `line_col = "grey85"`.

- sens_range:

  Range (minimum and maximum) of
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) values
  on x-axis (i.e., values in `c(0, 1)` range). Default:
  `sens_range = c(0, 1)`.

- spec_range:

  Range (minimum and maximum) of
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) values
  on y-axis (i.e., values in `c(0, 1)` range). Default:
  `spec_range = c(0, 1)`.

- step_size:

  Sets the granularity of the
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md)-by-[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)
  grid. (in range `.01 <= step_size <= 1`). Default: `step_size = .05`.

- show_points:

  Boolean option for showing the current value of the selected metric
  for the current conditions
  ([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)) as a
  point on the plane. Default: `show_points = TRUE`.

- point_col:

  Fill color for showing current value on plane. Default:
  `point_col = "yellow"`.

- theta:

  Horizontal rotation angle (used by
  [`persp`](https://rdrr.io/r/graphics/persp.html)). Default:
  `theta = -45`.

- phi:

  Vertical rotation angle (used by
  [`persp`](https://rdrr.io/r/graphics/persp.html)). Default: `phi = 0`.

- p_lbl:

  Type of label for shown probability values, with the following
  options:

  1.  `"abb"`: show abbreviated probability names;

  2.  `"def"`: show abbreviated probability names and values (default);

  3.  `"nam"`: show only probability names (as specified in code);

  4.  `"num"`: show only numeric probability values;

  5.  `"namnum"`: show names and numeric probability values;

  6.  `"no"`: hide labels (same for `p_lbl = NA` or `NULL`).

- lbl_txt:

  Labels and text elements. Default:
  `lbl_txt = `[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md).

- main:

  Text label for main plot title. Default: `main = txt$scen_lbl`.

- sub:

  Text label for plot subtitle (on 2nd line). Default: `sub = "type"`
  shows information on current plot type.

- title_lbl:

  **Deprecated** text label for current plot title. Replaced by `main`.

- cex_lbl:

  Scaling factor for the size of text labels (e.g., on axes, legend,
  margin text). Default: `cex_lbl = .85`.

- col_pal:

  Color palette (if what_col is unspecified). Default:
  `col_pal = `[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md).

- mar_notes:

  Boolean value for showing margin notes. Default: `mar_notes = FALSE`.

- ...:

  Other (graphical) parameters.

## Details

`plot_plane` is a generalization of `plot_PV3d` (see legacy code) that
allows for additional dependent values.

## See also

[`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md)
computes the current population;
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) contains
the current population;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) for basic
numeric parameters;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for current
text settings;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) for current
color settings

Other visualization functions:
[`plot.riskyr()`](https://hneth.github.io/riskyr/dev/reference/plot.riskyr.md),
[`plot_area()`](https://hneth.github.io/riskyr/dev/reference/plot_area.md),
[`plot_bar()`](https://hneth.github.io/riskyr/dev/reference/plot_bar.md),
[`plot_crisk()`](https://hneth.github.io/riskyr/dev/reference/plot_crisk.md),
[`plot_curve()`](https://hneth.github.io/riskyr/dev/reference/plot_curve.md),
[`plot_fnet()`](https://hneth.github.io/riskyr/dev/reference/plot_fnet.md),
[`plot_icons()`](https://hneth.github.io/riskyr/dev/reference/plot_icons.md),
[`plot_mosaic()`](https://hneth.github.io/riskyr/dev/reference/plot_mosaic.md),
[`plot_prism()`](https://hneth.github.io/riskyr/dev/reference/plot_prism.md),
[`plot_tab()`](https://hneth.github.io/riskyr/dev/reference/plot_tab.md),
[`plot_tree()`](https://hneth.github.io/riskyr/dev/reference/plot_tree.md)

## Examples

``` r
# Basics:
plot_plane()              # => default plot (what = "PPV")

# same as:
# plot_plane(what = "PPV")  # => plane of PPV

plot_plane(what = "NPV")  # => plane of NPV

plot_plane(what = "ppod") # => plane of ppod

plot_plane(what = "acc")  # => plane of acc


# Plane with/out points:
# plot_plane(prev = .5, sens = NA, spec = NA, what = "ppv")           # plane with 0 points
plot_plane(prev = .5, sens = c(.2, .5, .8), spec = .6, what = "npv")  # plane with 3 points
#> Multiple sens/spec values provided: Plotting multiple points on plane.


# Zooming into sens and spec ranges:
# plot_plane(prev = .02, sens = c(.8, .9), spec = c(.8, .8, .9, .9))  # default ranges
plot_plane(prev = .02, sens = c(.8, .9), spec = c(.8, .8, .9, .9),
           sens_range = c(.7, 1), spec_range = c(.7, 1), step_size = .02)  # zooming in
#> Multiple sens/spec values provided: Plotting multiple points on plane.


# Options:
# plot_plane(main = "No point and smaller labels", show_points = FALSE, cex_lbl = .60)

plot_plane(main = "Testing plot colors", what_col = "royalblue4", line_col = "sienna2")

plot_plane(main = "Testing b/w plot", what = "npv", what_col = "white", line_col = "black")

plot_plane(main = "Testing color pal_bwp", col_pal = pal_bwp)


plot_plane(step_size = .333, what_col = "firebrick")    # => coarser granularity + color

plot_plane(step_size = .025, what_col = "chartreuse4")  # => finer granularity + color

plot_plane(what_col = "steelblue4", theta = -90, phi = 50)  # => rotated, from above

```

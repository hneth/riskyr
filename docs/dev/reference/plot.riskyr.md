# Plot a riskyr scenario.

`plot.riskyr` is a method that allows to generate different plot types
from a `"riskyr"` object.

## Usage

``` r
# S3 method for class 'riskyr'
plot(x = NULL, type = "prism", main = NULL, sub = NULL, ...)
```

## Arguments

- x:

  A `riskyr` object, usually a result of a call to
  [`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md).
  Pre-defined
  [`scenarios`](https://hneth.github.io/riskyr/dev/reference/scenarios.md)
  are also of type `riskyr`.

- type:

  The type of plot to be generated.

- main:

  Text label for main plot title. Default: `main = NULL` (using
  `x$scen_lbl` per default).

- sub:

  Text label for plot subtitle (on 2nd line). Default: `sub = NULL`
  (using `sub = "type"` shows plot type).

  The following plot types are currently available:

  1.  `type = "prism"` or `type = "net"` or `type = "tree"`: Risk
      information is plotted in a network diagram of frequencies and
      probabilities (default). See
      [`plot_prism`](https://hneth.github.io/riskyr/dev/reference/plot_prism.md)
      for further options.

  2.  `type = "tab"` or `type = "ftab"`: Risk information is plotted as
      a 2-by-2 frequency or contingency table. See
      [`plot_tab`](https://hneth.github.io/riskyr/dev/reference/plot_tab.md)
      for further options.

  3.  `type = "area"` or `type = "mosaic"`: Risk information is plotted
      as a mosaic plot (scaled area). See
      [`plot_area`](https://hneth.github.io/riskyr/dev/reference/plot_area.md)
      for further options.

  4.  `type = "bar"` or `type = "fbar"`: Risk information is plotted as
      a bar chart. See
      [`plot_bar`](https://hneth.github.io/riskyr/dev/reference/plot_bar.md)
      for further options.

  5.  `type = "icons"` or `type = "iconarray"`: The underlying
      population is plotted as an array of icons. See
      [`plot_icons`](https://hneth.github.io/riskyr/dev/reference/plot_icons.md)
      for further options.

  6.  `type = "curve"` or `type = "curves"`: Draws curves of selected
      values (including
      [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md),
      [`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md)) See
      [`plot_curve`](https://hneth.github.io/riskyr/dev/reference/plot_curve.md)
      for further options.

  7.  `type = "plane"` or `type = "planes"`: Draws a 3D-plane of
      selected values (e.g., predictive values
      [`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) or
      [`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md)) See
      [`plot_plane`](https://hneth.github.io/riskyr/dev/reference/plot_plane.md)
      for further options.

- ...:

  Additional parameters to be passed to the underlying plotting
  functions.

## Details

`plot.riskyr` also uses the text settings specified in the "riskyr"
object.

## See also

[`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)
initializes a `riskyr` scenario.

Other visualization functions:
[`plot_area()`](https://hneth.github.io/riskyr/dev/reference/plot_area.md),
[`plot_bar()`](https://hneth.github.io/riskyr/dev/reference/plot_bar.md),
[`plot_crisk()`](https://hneth.github.io/riskyr/dev/reference/plot_crisk.md),
[`plot_curve()`](https://hneth.github.io/riskyr/dev/reference/plot_curve.md),
[`plot_fnet()`](https://hneth.github.io/riskyr/dev/reference/plot_fnet.md),
[`plot_icons()`](https://hneth.github.io/riskyr/dev/reference/plot_icons.md),
[`plot_mosaic()`](https://hneth.github.io/riskyr/dev/reference/plot_mosaic.md),
[`plot_plane()`](https://hneth.github.io/riskyr/dev/reference/plot_plane.md),
[`plot_prism()`](https://hneth.github.io/riskyr/dev/reference/plot_prism.md),
[`plot_tab()`](https://hneth.github.io/riskyr/dev/reference/plot_tab.md),
[`plot_tree()`](https://hneth.github.io/riskyr/dev/reference/plot_tree.md)

Other riskyr scenario functions:
[`riskyr()`](https://hneth.github.io/riskyr/dev/reference/riskyr.md),
[`summary.riskyr()`](https://hneth.github.io/riskyr/dev/reference/summary.riskyr.md)

## Examples

``` r
# Select a scenario (from list of scenarios):
s1 <- scenarios$n1  # select scenario 1 from scenarios
plot(s1)  # default plot (type = "prism")


# Plot types currently available:
plot(s1, type = "prism")                # prism/network diagram (default)
plot(s1, type = "tree", by = "cd")      # tree diagram (only 1 perspective)

plot(s1, type = "area")                 # area/mosaic plot

plot(s1, type = "tab")                  # 2x2 frequency/contingency table

plot(s1, type = "bar", dir = 2)         # bar plot

plot(s1, type = "icons")                # icon array

plot(s1, type = "curve", what = "all")  # curves as fn. of prev

plot(s1, type = "plane", what = "NPV")  # plane as function of sens & spec

plot(s1, type = "default")              # unknown type: use default plot
#> Unknown plot type (in plot.riskyr): Using type = 'prism'.

```

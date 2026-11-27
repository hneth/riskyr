# Plot bar charts of population frequencies.

`plot_bar` draws bar charts that represent the proportions of
frequencies in the current population
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) as
relatives sizes of rectangular areas.

## Usage

``` r
plot_bar(
  prev = num$prev,
  sens = num$sens,
  mirt = NA,
  spec = num$spec,
  fart = NA,
  N = num$N,
  by = "all",
  dir = 1,
  scale = "f",
  round = TRUE,
  sample = FALSE,
  f_lbl = "num",
  f_lwd = 1,
  lty = 0,
  lbl_txt = txt,
  main = txt$scen_lbl,
  sub = "type",
  title_lbl = NULL,
  col_pal = pal,
  mar_notes = FALSE,
  ...
)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (i.e.,
  the probability of condition being `TRUE`, aka. *prior*).

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

  The number of individuals in the population. (This value is not
  represented in the plot, but used when new frequency information
  [`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) and a
  new population table
  [`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) are
  computed from scratch from current probabilities.)

- by:

  A character code specifying the perspective (or the dimension by which
  the population is split into 2 subsets) with the following options:

  1.  `by = "cd"`: by condition;

  2.  `by = "dc"`: by decision;

  3.  `by = "ac"`: by accuracy;

  4.  `by = "all"` combines perspectives (5 bars, default).

- dir:

  Number of directions in which bars are plotted. Options:

  1.  `dir = 1`: uni-directional bars (all up, default);

  2.  `dir = 2`: bi-directional bars (up vs. down).

- scale:

  Scale the heights of bars either by current frequencies
  (`scale = "f"`) or by exact probabilities (`scale = "p"`). Default:
  `scale = "f"`. For large population sizes
  [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) and when
  `round = FALSE`, both settings yield the same bar heights.

- round:

  Boolean option specifying whether computed frequencies are to be
  rounded to integers. Default: `round = TRUE`.

- sample:

  Boolean value that determines whether frequency values are sampled
  from `N`, given the probability values of `prev`, `sens`, and `spec`.
  Default: `sample = FALSE`.

- f_lbl:

  Type of frequency labels, as character code with the following
  options:

  1.  `f_lbl = "nam"`: names;

  2.  `f_lbl = "num"`: numeric values (default);

  3.  `f_lbl = "abb"`: abbreviated names;

  4.  `f_lbl = NA/NULL/"no"`: no labels;

  5.  `f_lbl = "any"`: abbreviated names and numeric values (abb = num).

- f_lwd:

  Line width of frequency box (border). Values of `NA/NULL/0` set `lwd`
  to invisible `tiny_lwd <- .001` and `lty <- 0` (`"blank"`). Default:
  `f_lwd = 1`.

- lty:

  Line type of frequency box (border). Values of `NA/NULL/0` set `lty`
  to `lty <- 0`. Default: `lty = 0` (i.e., no line).

- lbl_txt:

  Current text information (for labels, titles, etc.). Default:
  `lbl_txt = `[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md)
  (see
  [`init_txt`](https://hneth.github.io/riskyr/dev/reference/init_txt.md)).

- main:

  Text label for main plot title. Default: `main = txt$scen_lbl`.

- sub:

  Text label for plot subtitle (on 2nd line). Default: `sub = "type"`
  shows information on current plot type.

- title_lbl:

  **Deprecated** text label for current plot title. Replaced by `main`.

- col_pal:

  Current color palette. Default:
  `col_pal = `[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md)
  (see
  [`init_pal`](https://hneth.github.io/riskyr/dev/reference/init_pal.md)).

- mar_notes:

  Boolean option for showing margin notes. Default: `mar_notes = FALSE`.

- ...:

  Other (graphical) parameters (e.g., `cex`, `font`, `lty`, etc.).

## Details

If a sufficient and valid set of 3 essential probabilities
([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md), and
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or its
complement
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or its
complement
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)) is
provided, new frequency information
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) and a new
population table
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) are
computed from scratch. Otherwise, the existing population
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) is shown.

By default, `plot_bar` uses current frequencies (i.e., rounded or not
rounded, depending on the value of `round`) as bar heights, rather than
using exact probabilities to scale bar heights (i.e., default scaling is
`scale = "f"`). Using the option `scale = "p"` scales bar heights by
probabilities (e.g., showing bars for non-natural frequencies even when
frequencies are rounded). When `round = FALSE`, bar heights for
`scale = "f"` and for `scale = "p"` are identical.

The distinction between `scale = "f"` and `scale = "p"` matters mostly
for small populations sizes
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) (e.g., when
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)` < 100`). For
rounded and small frequency values (e.g.,
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md)` < 10`)
switching from `scale = "f"` to `scale = "p"` yields different plots.

`plot_bar` contrasts compound frequencies along 1 dimension (height).
See
[`plot_mosaic`](https://hneth.github.io/riskyr/dev/reference/plot_mosaic.md)
for 2-dimensional visualizations (as areas) and various `box`) options
in
[`plot_tree`](https://hneth.github.io/riskyr/dev/reference/plot_tree.md)
and
[`plot_fnet`](https://hneth.github.io/riskyr/dev/reference/plot_fnet.md)
for related functions.

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
[`plot_crisk()`](https://hneth.github.io/riskyr/dev/reference/plot_crisk.md),
[`plot_curve()`](https://hneth.github.io/riskyr/dev/reference/plot_curve.md),
[`plot_fnet()`](https://hneth.github.io/riskyr/dev/reference/plot_fnet.md),
[`plot_icons()`](https://hneth.github.io/riskyr/dev/reference/plot_icons.md),
[`plot_mosaic()`](https://hneth.github.io/riskyr/dev/reference/plot_mosaic.md),
[`plot_plane()`](https://hneth.github.io/riskyr/dev/reference/plot_plane.md),
[`plot_prism()`](https://hneth.github.io/riskyr/dev/reference/plot_prism.md),
[`plot_tab()`](https://hneth.github.io/riskyr/dev/reference/plot_tab.md),
[`plot_tree()`](https://hneth.github.io/riskyr/dev/reference/plot_tree.md)

## Examples

``` r
# Basics:
# (1) Using global prob and freq values:
plot_bar()


# (2) Providing values:
plot_bar(prev = .33, sens = .75, spec = .66, main = "Test 1")

plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, main = "Test 2")  # by "all" (default)


# (3) Rounding and sampling:
plot_bar(N = 100, prev = 1/3, sens = 2/3, spec = 6/7, area = "hr", round = FALSE)
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter

plot_bar(N = 100, prev = 1/3, sens = 2/3, spec = 6/7, area = "hr", sample = TRUE, scale = "freq")
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter
#> Warning: "area" is not a graphical parameter


# Perspectives (by):
# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd",
#          main = "Test 3a")  # by condition
plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "cd", dir = 2,
         main = "Test 3b", f_lbl = "num")  # bi-directional


# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc",
#          main = "Test 4a")  # by decision
plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "dc", dir = 2,
         main = "Test 4b", f_lbl = "num")  # bi-directional


# plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac",
#          main = "Test 5a")  # by accuracy
plot_bar(N = 1000, prev = .33, sens = .75, spec = .60, by = "ac", dir = 2,
         main = "Test 5b", f_lbl = "num")  # bi-directional


# Customize colors and text:
plot_bar(dir = 1, f_lbl = "num", col_pal = pal_org)

# plot_bar(dir = 2, f_lbl = "nam", col_pal = pal_bw)

# Frequency labels (f_lbl):
# plot_bar(f_lbl = "def")  # default labels: name = num
plot_bar(f_lbl = "nam")  # name only

plot_bar(f_lbl = "num")  # numeric value only

# plot_bar(f_lbl = "abb")  # abbreviated name
# plot_bar(f_lbl = NA)     # no labels (NA/NULL/"no")

# Scaling and rounding effects:
plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
         scale = "f", round = TRUE,
         main = "Rounding (1)") # => Scale by freq and round freq.

plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
         scale = "p", round = TRUE,
         main = "Rounding (2)") # => Scale by prob and round freq.

plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
         scale = "f", round = FALSE,
         main = "Rounding (3)") # => Scale by freq and do NOT round freq.

plot_bar(N = 3, prev = .1, sens = .7, spec = .6, dir = 2,
         scale = "p", round = FALSE,
         main = "Rounding (4)") # => Scale by prob and do NOT round freq.

```

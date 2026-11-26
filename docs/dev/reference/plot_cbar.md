# Plot cumulative risk dynamics (as bar chart)

`plot_cbar` plots the results of cumulative risk dynamics as a bar chart
(with percentages of risk event counts for each period t on a horizontal
bar).

## Usage

``` r
plot_cbar(
  r = 0.5,
  t = NA,
  N = 100,
  horizontal = TRUE,
  sort = FALSE,
  N_max = 100,
  bar_width = 0.5,
  show_trans = 1,
  show_ev = TRUE,
  show_n = FALSE,
  show_bin = FALSE,
  colors = c("firebrick", "grey96", "green4", "grey40")
)
```

## Arguments

- r:

  risk (probability of occurrence per time period). A non-scalar vector
  allows for different risk values at different times (and
  `t = length(r)`).

- t:

  time periods/rounds. Default: `t = NA`, setting `t <- length(r)`. A
  scalar `r` and numeric `t` sets `r <- rep(r, t)`.

- N:

  population size. Default: `N = 100` expresses risks as percentages,
  `N = 1` as probabilities, else frequencies.

- horizontal:

  logical: Draw horizontal vs. vertical bars?

- sort:

  logical: Sort outputs by number of event occurrences? Default:
  `sort = FALSE`.

- N_max:

  maximum N value plotted (for zooming in for small `r` values). Default
  value should be set to population size `N`.

- bar_width:

  width of (horizontal/vertical) bar per time period. Default:
  `bar_width = .50`.

- show_trans:

  numeric: Show transition polygons (between bars)? Values of 0/1/2/3
  focus on no/new/remaining/both risk segments, respectively. Default:
  `show_trans = 1` (i.e., focus on increasing risk segments).

- show_ev:

  logical: Show number of risky event occurrence (as bar label)?
  Default: `show_ev = TRUE`.

- show_n:

  logical: Show population frequency of risky event occurrences (as bar
  label)? Default: `show_n = FALSE`.

- show_bin:

  logical: Show risky event history as binary state representation (as
  bar label)? Default: `show_bin = FALSE`.

- colors:

  A vector of color values (for risk event frequency being `"hi"`,
  `"lo"`, `"no"`, and `"bd"` for borders, respectively). Default:
  `colors = c("firebrick", "grey96", "green4", "grey40")`.

## Value

data of p-values, named by number of event occurrences (invisibly, as
list of named vectors, for each time period t).

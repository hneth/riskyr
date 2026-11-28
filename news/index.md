# Changelog

## riskyr 0.6.0

![riskyr
cube](https://raw.githubusercontent.com/hneth/riskyr/master/inst/pix/riskyr_cube.png)

**riskyr 0.6.0** was released [on
CRAN](https://CRAN.R-project.org/package=riskyr) on 2026-11-30. This
version adds some functionality, but mostly improves robustness and
reduces redundancy.

Changes since last release:

### Minor changes

- Add `aux_line` argument to
  [`plot_curve()`](https://hneth.github.io/riskyr/reference/plot_curve.md)
  for showing diagonal auxiliary lines (for comparing PPV and NPV curves
  to corresponding baselines).

- Add
  [`is_valid_freq_set()`](https://hneth.github.io/riskyr/reference/is_valid_freq_set.md)
  for verifying a set of 4 essential frequencies (`hi`, `mi`, `fa`,
  `cr`) — a frequency analog to
  [`is_valid_prob_set()`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md)
  — that (by default) also accepts exact (non-integer) frequencies.

- Add input validation to functions computing frequencies and
  probabilities:

  - Functions computing metrics from frequencies (e.g.,
    [`comp_prob_freq()`](https://hneth.github.io/riskyr/reference/comp_prob_freq.md))
    now verify their `hi`, `mi`, `fa`, and `cr` inputs;  
  - Functions computing metrics from probabilities (e.g.,
    [`comp_acc()`](https://hneth.github.io/riskyr/reference/comp_acc.md))
    now verify their `prev`, `sens`, and `spec` inputs;  
  - Invalid or incomplete input sets now yield `NA` (with a warning)
    instead of silently computing invalid values.

### Details

- Reduce redundancy:
  - Consolidate input-verification logic into internal helpers;
  - Use `@inheritParams` to remove duplicated documentation of shared
    probability (`prev`, `sens`, `spec`, `mirt`, `fart`) and frequency
    (`hi`, `mi`, `fa`, `cr`) parameters;  
  - Add an internal helper for combining plot titles and subtitles (used
    by all plot functions).

&nbsp;

- Maintenance:
  - Add a test suite for verifying core functionality (based on
    **testthat**);  
  - Add doi
    [10.32614/CRAN.package.riskyr](https://doi.org/10.32614/CRAN.package.riskyr)
    to citation;  
  - Update documentation and `README`.

The current development version of **riskyr** is available at
<https://github.com/hneth/riskyr/>.

------------------------------------------------------------------------

## riskyr 0.5.0

CRAN release: 2025-09-15

**riskyr 0.5.0** was released [on
CRAN](https://CRAN.R-project.org/package=riskyr) on 2025-09-15. This
version fixes minor issues for CRAN compatibility.

Changes since last release:

### Details

- Maintenance:
  - Add GitHub actions (R-CMD-check and Rhub workflows)
  - Update `README` and online documentation

&nbsp;

- Updated citation info (using
  [`bibentry()`](https://rdrr.io/r/utils/bibentry.html) for CRAN
  compatibility)

- Use hexagon version of logo

------------------------------------------------------------------------

## riskyr 0.4.0

CRAN release: 2022-08-15

**riskyr 0.4.0** was released [on
CRAN](https://CRAN.R-project.org/package=riskyr) on August 15, 2022.

Changes since last release:

### Major changes

- Enable *sampling* when computing `freq` from `prob` (i.e., *by
  description*, given `N` and 3 essential probabilities) \[2021-03\]:

  - add a `sample` argument to
    [`comp_freq()`](https://hneth.github.io/riskyr/reference/comp_freq.md),
    [`comp_freq_prob()`](https://hneth.github.io/riskyr/reference/comp_freq_prob.md),
    and
    [`riskyr()`](https://hneth.github.io/riskyr/reference/riskyr.md);  
  - add a `sample` argument to 6 key plots:  
    [`plot_area()`](https://hneth.github.io/riskyr/reference/plot_area.md),
    [`plot_bar()`](https://hneth.github.io/riskyr/reference/plot_bar.md),
    [`plot_fnet()`](https://hneth.github.io/riskyr/reference/plot_fnet.md),
    [`plot_icons()`](https://hneth.github.io/riskyr/reference/plot_icons.md),
    [`plot_prism()`](https://hneth.github.io/riskyr/reference/plot_prism.md),
    and
    [`plot_tab()`](https://hneth.github.io/riskyr/reference/plot_tab.md).

&nbsp;

- Allow conversions/translations between data and descriptions:

  - [`write_popu()`](https://hneth.github.io/riskyr/reference/write_popu.md)
    creates population data from a description (a `riskyr` scenario);
  - [`read_popu()`](https://hneth.github.io/riskyr/reference/read_popu.md)
    creates a description (a `riskyr` scenario) from population data.

&nbsp;

- Add a function for plotting cumulative risk curves:

  - [`plot_crisk()`](https://hneth.github.io/riskyr/reference/plot_crisk.md)
    plots curves and auxiliary information for visualizing cumulative
    risks and risk increments;
  - `pal_crisk` provides a corresponding color palette (as a named
    vector).

&nbsp;

- Add
  [`FFTrees_riskyr()`](https://hneth.github.io/riskyr/reference/FFTrees_riskyr.md)
  to convert `FFTrees` objects into corresponding `riskyr` objects.

### Minor changes

- Add `main` and `sub` arguments to all plots (and deprecate the
  previous `title_lbl` argument).
- Add
  [`is_matrix()`](https://hneth.github.io/riskyr/reference/is_matrix.md)
  to verify a 2x2 matrix (as a 2x2 numeric contingency table).
- Add
  [`is_integer()`](https://hneth.github.io/riskyr/reference/is_integer.md)
  to verify integer values.

### Details

- Fix bug in
  [`plot_bar()`](https://hneth.github.io/riskyr/reference/plot_bar.md)
  that caused incorrect bar labels.
- Update URLs in `README.md` and all vignettes to <https://riskyr.org/>.
- Increase options and robustness for labeling frequencies and
  probabilities in plots.
- Reduce widths of freq boxes in
  [`plot_prism()`](https://hneth.github.io/riskyr/reference/plot_prism.md)
  to reduce overlaps.
- Rename labels in `txt_TF` to avoid confusion, as “True condition”
  (i.e., X) was `FALSE` (now “absent”) for “True negatives” (`TN`/`cr`
  cases).

------------------------------------------------------------------------

## riskyr 0.3.0

CRAN release: 2021-03-23

**riskyr 0.3.0** was released [on
CRAN](https://CRAN.R-project.org/package=riskyr) on March 23, 2021.

Changes since last release:

### Major changes

- Add
  [`plot_fnet()`](https://hneth.github.io/riskyr/reference/plot_fnet.md)
  for plotting *frequency nets* ([Binder et al.,
  2020](https://www.frontiersin.org/articles/10.3389/fpsyg.2020.00750/full))
  \[2020-12\].

### Minor changes

#### Changes to existing visualization functions

- Change default setting of `mar_notes` in all plotting functions: Using
  `mar_notes = FALSE` as those details are not needed and can be
  distracting in visualizations.

- Change default arguments for
  [`plot_area()`](https://hneth.github.io/riskyr/reference/plot_area.md)
  and
  [`plot_tab()`](https://hneth.github.io/riskyr/reference/plot_tab.md):  
  Change default settings from `f_lbl_hd = "abb"` to `f_lbl_hd = "nam"`
  (as this makes more sense for **riskyrApp**).

- New options for
  [`plot_curve()`](https://hneth.github.io/riskyr/reference/plot_curve.md):  
  Plotting probability curves as a function of prevalence does not
  require any specific prevalence value. Thus, setting the `prev`
  argument to either `NA` or to a vector of several probabilities is now
  supported. Setting the new `prev_range` argument to a range within
  `c(0, 1)` allows zooming into more specific ranges of `prev` values
  (on x-axis).

- New options for
  [`plot_plane()`](https://hneth.github.io/riskyr/reference/plot_plane.md):  
  Plotting a probability plane as a function of sensitivity and
  specificity does not require specifying all values. Thus, setting the
  `sens` and `spec` arguments to either `NA` or to a vector of several
  probabilities is now supported. Setting the new `sens_range` and
  `spec_range` arguments to ranges within `c(0, 1)` allows zooming into
  more specific ranges of `sens` values (on x-axis) and `spec` values
  (on y-axis).

- New options for
  [`plot_prism()`](https://hneth.github.io/riskyr/reference/plot_prism.md):  
  Use the new `p_lwd` and `p_scale` arguments to allow scaling the
  widths of probability links by current probability values.

#### Data and functions

- In `data`: Remove data files `df_scenarios.RData` and
  `df_scenarios.csv`, as they were redundant to `df_scenarios.rda`
  (loaded from `\data`).

- Improve `make_cond_lbl()` to allow for `NA` or vectors of several
  values (for `prev`, `sens`, and `spec`).

- Add `is_prob_range()` function to verify a range of two probability
  values (to check new arguments of
  [`plot_curve()`](https://hneth.github.io/riskyr/reference/plot_curve.md)
  and
  [`plot_plane()`](https://hneth.github.io/riskyr/reference/plot_plane.md)).

### Micro changes

#### Colors

- Using colors of `pal` for fill colors of symbols in `plot_icon()`.

- Add more color palettes:

  - Add `pal_bwp` (a strict b+w color palette suited for printing
    purposes) and corresponding special cases to major plotting
    functions.
  - Add `pal_unikn` (based on color definitions of the **unikn**
    package).

&nbsp;

- Changes to color palettes:

  - Add a background color `pal[["bg"]]` to all palettes and plots (to
    preempt different system defaults).  
  - Adopt `pal_mod` – rather than `pal_mbw` – as default color scheme
    `pal` (to highlight `cond_true` and `dec_pos` cases in default
    plots).

#### Details

- Add reference to article on theoretical background ([Neth et al.,
  2021](https://doi.org/10.3389/fpsyg.2020.567817)).

- Bug fix in
  [`plot_prism()`](https://hneth.github.io/riskyr/reference/plot_prism.md):
  Allow plotting simple trees for `nchar(by) == 2`.

- Bug fix in
  [`plot_icons()`](https://hneth.github.io/riskyr/reference/plot_icons.md):
  Enforced 2 different symbol types for icon arrays with a binary
  perspective (`by = cd` or `dc` or `ac`).

------------------------------------------------------------------------

## riskyr 0.2.0

CRAN release: 2019-01-03

**riskyr 0.2.0** was released [on
CRAN](https://CRAN.R-project.org/package=riskyr) on January 03, 2019.

Changes since last release:

### Major changes

#### Additional resources

- Introduce `riskyrApp` version:  
  To use selected `riskyr` functions without the need for coding an
  updated version of `riskyrApp` is available at
  <https://github.com/hneth/riskyrApp/> (R Shiny code) and at
  <https://riskyr.org/> (interactive online version).

- Using `pkgdown`:  
  Provide package documentation online at
  <https://hneth.github.io/riskyr/> (latest release version) and
  <https://hneth.github.io/riskyr/dev/> (current development version).

#### Visualization functions

- Retiring obsolete functions:  
  The plotting functions
  [`plot_fnet()`](https://hneth.github.io/riskyr/reference/plot_fnet.md)
  and
  [`plot_tree()`](https://hneth.github.io/riskyr/reference/plot_tree.md)
  are replaced by
  [`plot_prism()`](https://hneth.github.io/riskyr/reference/plot_prism.md),
  and
  [`plot_mosaic()`](https://hneth.github.io/riskyr/reference/plot_mosaic.md)
  is replaced by
  [`plot_area()`](https://hneth.github.io/riskyr/reference/plot_area.md).
  This improves functionality (e.g., by providing more consistent
  options across different plotting functions) and removes dependencies
  on external packages.

- New
  [`plot_prism()`](https://hneth.github.io/riskyr/reference/plot_prism.md)
  function:  
  Show a scenario as double frequency tree (by 3 x 2 perspectives) or a
  frequency tree (in 3 perspectives) with many additional options;
  combines and replaces the older
  [`plot_fnet()`](https://hneth.github.io/riskyr/reference/plot_fnet.md)
  and
  [`plot_tree()`](https://hneth.github.io/riskyr/reference/plot_tree.md)
  functions (and removes dependency on the **diagram** package).

- New
  [`plot_area()`](https://hneth.github.io/riskyr/reference/plot_area.md)
  function:  
  Show a scenario as a mosaic plot of relative proportions (in 3 x 2 x 2
  possible versions, with many additional options); replaces the older
  [`plot_mosaic()`](https://hneth.github.io/riskyr/reference/plot_mosaic.md)
  function (and removes dependencies on the **grid** and **vcd**
  packages).

- New
  [`plot_tab()`](https://hneth.github.io/riskyr/reference/plot_tab.md)
  function:  
  Show a scenario as contingency table of frequencies (with row and
  column sums, and options for showing probabilities); a variant of
  [`plot_area()`](https://hneth.github.io/riskyr/reference/plot_area.md)
  that does not scale area sizes.

- New
  [`plot_bar()`](https://hneth.github.io/riskyr/reference/plot_bar.md)
  function:  
  Show scenario frequencies as vertical bars (in various
  configurations).

#### Under the hood

- Create `plot_util.R` collection of graphical utility functions:  
  Define a new `box` object type and various functions for plotting,
  labeling, and linking them in graphs (to remove dependencies on and
  limitations imposed by other packages).

- Update
  [`riskyr()`](https://hneth.github.io/riskyr/reference/riskyr.md)
  function:  
  As an alternative to providing 3 essential frequencies, it is now
  possible to define a scenario from 4 essential frequencies (and check
  for consistency with given probabilities).

### Minor changes

#### Changes to existing visualization functions

- Improve
  [`plot_icons()`](https://hneth.github.io/riskyr/reference/plot_icons.md)
  function:  
  Show icons separated into 2 subsets by 3 perspectives (condition,
  decision, accuracy), using the same `by` argument as the other
  plotting functions.

- Improve
  [`plot_curve()`](https://hneth.github.io/riskyr/reference/plot_curve.md)
  and
  [`plot_plane()`](https://hneth.github.io/riskyr/reference/plot_plane.md)
  functions:  
  Update variable names (to snake_case) and add arguments (e.g.,
  `col_pal`, `lbl_txt` `mar_notes`, etc.) for consistency with newer
  plotting functions.

- Add `scale` argument:  
  The new plotting functions feature a `scale` argument that allows
  scaling the size or areas of boxes either by (exact) probability or by
  (possibly rounded) frequency. When using `scale = "f"`, the
  probabilities shown are also re-computed from (possibly rounded)
  frequencies.

- Improve
  [`plot_fnet()`](https://hneth.github.io/riskyr/reference/plot_fnet.md)
  function:  
  Change argument `box.cex` to `cex.lbl` to ensure consistency with
  `plot_curves()` and
  [`plot_plane()`](https://hneth.github.io/riskyr/reference/plot_plane.md)
  (and use it to scale arrow labels accordingly). Add a warning when
  using deprecated argument.

- Improve
  [`plot_mosaic()`](https://hneth.github.io/riskyr/reference/plot_mosaic.md)
  function:  
  Change Boolean `vsplit` argument to `by = "cd"` vs. `by = "dc"` to
  ensure consistency with
  [`plot_fnet()`](https://hneth.github.io/riskyr/reference/plot_fnet.md)
  and
  [`plot_tree()`](https://hneth.github.io/riskyr/reference/plot_tree.md).
  Add a warning when using deprecated argument.

- Improve `mar_notes` and `plot_mar` arguments:  
  Use consistent plot margins and options for showing margin notes for
  all plots.

#### Default objects (lists and vectors)

- Improve `pal` and `freq` arguments:  
  Use more consistent color and frequency names (e.g., `cond_true`,
  `dec_pos`, and `dec_cor` are now names of frequencies and the colors
  corresponding to these frequencies).

- Improve `freq` object:  
  Add a 3rd perspective (by accuracy or by correspondence of decision to
  condition) and corresponding frequency pair of `dec.cor` and `dec.err`
  (i.e., `hi + cr` vs. `mi + fa` as the diagonal of 4 SDT cases). This
  increases the number of frequencies in `freq` from 9 to 11. Also added
  corresponding labels in `init_txt.R` and colors in `init_pal.R`.

- Improve `prob` object:  
  Include accuracy metrics in probabilities (in `prob` and summary
  functions).

- Improve `pal` and `txt` objects:  
  Add multiple color palettes and text labeling schemes (see
  [`?pal`](https://hneth.github.io/riskyr/reference/pal.md) and
  [`?txt`](https://hneth.github.io/riskyr/reference/txt.md) for
  details).

#### Other changes

- Improve
  [`read_popu()`](https://hneth.github.io/riskyr/reference/read_popu.md)
  function:  
  Read a data frame `popu` and interpret is as a `riskyr` scenario,
  allows creating scenarios from raw data.

- Add `comp_accu.R` file:  
  Compute exact accuracy values (not approximations, when using
  `comp_accu_freq` on rounded `freq` values) by using the new function
  [`comp_accu_prob()`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md)
  to compute the list `accu` from probabilities. Signal rounding when
  showing accuracy based on rounded frequencies in plots (when
  `show.accu == TRUE` and `round == TRUE`).

### Micro changes

#### General changes

- More consistent argument and variable names (using snake_case).

- Many additions and corrections in documentation, examples, and
  vignettes.

#### Details

- [`plot_icons()`](https://hneth.github.io/riskyr/reference/plot_icons.md):
  Bug fix to also swap symbols in legend when the symbol order is
  changed manually.

- `txt_def`: Simplify some default text labels (e.g., for current
  population, condition, and decision).

- `.onAttach`: Cast dice to display probabilistic (i.e., risk-related)
  start-up messages.

------------------------------------------------------------------------

## riskyr 0.1.0

CRAN release: 2018-02-19

**riskyr 0.1.0** was released [on
CRAN](https://CRAN.R-project.org/package=riskyr) on February 19, 2018.

------------------------------------------------------------------------

### References

![riskyr
cube](https://raw.githubusercontent.com/hneth/riskyr/master/inst/pix/riskyr_cube.png)

To cite **riskyr** in derivations and publications please use:

- Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2026).  
  riskyr: Rendering Risk Literacy more Transparent.  
  Social Psychology and Decision Sciences, University of Konstanz,
  Germany.  
  Computer software (R package version 0.6.0, Nov 30, 2026).  
  Retrieved from <https://CRAN.R-project.org/package=riskyr>.  
  doi
  [10.32614/CRAN.package.riskyr](https://doi.org/10.32614/CRAN.package.riskyr)

The following **article** provides details on the conceptual and
theoretical background:

- Neth, H., Gradwohl, N., Streeb, D., Keim, D.A., & Gaissmaier, W.
  (2021). Perspectives on the 2x2 matrix: Solving semantically distinct
  problems based on a shared structure of binary contingencies.  
  *Frontiers in Psychology*, *11*, 567817. doi
  [10.3389/fpsyg.2020.567817](https://doi.org/10.3389/fpsyg.2020.567817)

------------------------------------------------------------------------

\[`NEWS.md` updated on 2026-11-30.\]

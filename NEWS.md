
# Current version

The current development version (0.2.0.9008+) is available at <https://github.com/hneth/riskyr/>. 


# riskyr 0.2.0.9008+

Log of changes since last release:

## Major changes

- none so far


## Minor changes

### Changes to existing visualization functions 

- New options for `plot_curve` [2019-01]:  
Plotting probability curves as a function of prevalence does not require any specific prevalence value. 
Thus, setting the `prev` argument to either `NA` or to a vector of several probabilities is now supported. 
Setting the new `prev_range` argument to a range within `c(0, 1)` allows zooming into more specific ranges of `prev` values (on x-axis).  

- New options for `plot_plane` [2019-01]:  
Plotting a probability plane as a function of sensitivity and specificity does not require specifying all values. 
Thus, setting the `sens` and `spec` arguments to either `NA` or to a vector of several probabilities is now supported. 
Setting the new `sens_range` and `spec_range` arguments to ranges within `c(0, 1)` allows zooming into more specific ranges of `sens` values (on x-axis) and `spec` values (on y-axis). 

- New options for `plot_prism` [2019-01]:  
Using the new `p_lwd` and `p_scale` arguments allows scaling the widths of probability links by current probability values.  

### Other changes

- `make_cond_lbl` [2019-01]:  
The condition label now allows for `NA` or vectors of several values (for `prev`, `sens`, and `spec`).  

- `is_prob_range` [2019-01]:
Utility function to verify a range of 2 probability values (to check new arguments of `plot_curve` and `plot_plane`).  

## Micro changes

### New functionality

- Add color palettes [2019-01]:   
Add `pal_bwp` (a strict b+w color palette suited for printing purposes) and corresponding special cases to major plotting functions. 

### General changes

- Change default arguments for `plot_area` and `plot_tab` [2019-01]:   
Change default settings from  `f_lbl_hd = "abb"` to `f_lbl_hd = "nam"` (as this makes more sense for riskyrApp).  

- Changes to color palettes [2019-01]:  
Add a background color `pal[["bg"]]` to all palettes and plots (to preempt different system defaults).  
Adopt `pal_mod` -- rather than `pal_mbw` -- as default color scheme `pal` (to highlight `cond_true` and `dec_pos` cases in default plots).  

### Details

- Bug fix in `plot_icons` [2019-01]:   
Enforce 2 different symbol types for icon arrays with a binary perspective (`by = cd` or `dc` or `ac`).   

---------- 

# riskyr 0.2.0

riskyr 0.2.0 was ready to be released on December 20, 2018, and published on CRAN on January 03, 2019.

Log of changes since last release:

## Major changes

### Additional resources 

- New `riskyrApp` version [2018-12]:   
To use selected `riskyr` functions without the need for coding 
an updated version of `riskyrApp` is available 
at <https://github.com/hneth/riskyrApp> (R Shiny code) and 
at <http://riskyr.org> (interactive online version). 

- Using `pkgdown` [2018-12]:   
Provide package documentation online at <https://hneth.github.io/riskyr> (latest release version) and <https://hneth.github.io/riskyr/dev/> (current development version). 


### Visualization functions

- Retiring obsolete functions [2018-12]:  
The functions `plot_fnet` and `plot_tree` are replaced by `plot_prism`, and `plot_mosaic` is replaced by `plot_area`. This improves functionality (e.g., by providing more consistent options across different plotting functions) and removes dependencies on external packages. 

- New `plot_prism` function [2018-11]:   
Show a scenario as double frequency tree (by 3 x 2 perspectives) or a frequency tree (in 3 perspectives) with many additional options; replaces the older `plot_fnet` and `plot_tree` functions (and removes dependency on the `diagram` package).

- New `plot_area` function [2018-10]:   
Show a scenario as a mosaic plot of relative proportions (in 3 x 2 x 2 possible versions, with many additional options); replaces the older `plot_mosaic` function (and removes dependencies on the `grid` and `vcd` packages).

- New `plot_tab` function [2018-10]:   
Show a scenario as contingency table of frequencies (with row and column sums, and options for showing probabilities); 
a variant of `plot_area` that does not scale area sizes. 

- New `plot_bar` function [2018-08]:   
Show scenario frequencies as vertical bars (in various configurations). 

### Under the hood

- Create `plot_util.R` collection of graphical utility functions [2018-08]:   
Define a new `box` object type and various functions for plotting, labeling, and linking them in graphs 
(to remove dependencies on and limitations imposed by other packages). 

- Updated `riskyr` function [2018-03]:   
As an alternative to providing 3 essential frequencies, it is now possible to define a scenario from 4 essential frequencies (and check for consistency with given probabilities). 


## Minor changes

### Changes to existing visualization functions 

- Improved `plot_icons` function [2018-12]:   
Show icons separated into 2 subsets by 3 perspectives (condition, decision, accuracy), using the same `by` argument as the other plotting functions.

- `plot_curve` and `plot_plane` functions [2018-11]:   
Update variable names (to snake_case) and add arguments (e.g., `col_pal`, `lbl_txt` `mar_notes`, etc.) for consistency with newer plotting functions. 

- `scale` argument [2018-10]:   
The new plotting functions feature a `scale` argument that allows scaling the size or areas of boxes either by (exact) probability or by (possibly rounded) frequency. When using `scale = "f"`, the probabilities shown are also re-computed from (possibly rounded) frequencies. 
- `plot_fnet` [2018-02]:   
Change argument `box.cex` to `cex.lbl` to ensure consistency with `plot_curves` and `plot_plane` (and use it to scale arrow labels accordingly). Added warning when using deprecated argument.

- `plot_mosaic` [2018-02]:   
Change Boolean `vsplit` argument to `by = "cd"` vs. `by = "dc"` to ensure consistency with `plot_fnet` and `plot_tree`. Added warning when using deprecated argument.

- `mar_notes` and `plot_mar` [2018-09]:   
Use consistent plot margins and options for showing margin notes for all plots.


### Default objects (lists and vectors)

- `pal` and `freq` [2018-12]:   
Use more consistent color and frequency names (e.g., `cond_true`, `dec_pos`, and `dec_cor` are now names of frequencies and the colors corresponding to these frequencies). 

- `freq` [2018-07]:   
Add a 3rd perspective (by accuracy or by correspondence of decision to condition) and corresponding frequency pair of `dec.cor` and `dec.err` (i.e., `hi + cr` vs. `mi + fa` as the diagonal of 4 SDT cases). This increases the number of frequencies in `freq` from 9 to 11. Also added corresponding labels in `init_txt.R` and colors in `init_pal.R`. 

- `prob` [2018-09]:   
Include accuracy metrics in probabilities (in `prob` and summary functions). 

- `pal` and `txt` [2018-10]:   
Add multiple color palettes and text labeling schemes (see `?pal` and `?txt` for details). 


### Other changes 

- `read_popu` [2018-11]:   
Read a data frame `popu` and interpret is as a `riskyr` scenario, allows creating scenarios from raw data. 

- `comp_accu.R` [2018-08]:   
Compute exact accuracy values (not approximations, when using `comp_accu_freq` on rounded `freq` values) by using the new function `comp_accu_prob` to compute the list `accu` from probabilities. Signal rounding when showing accuracy based on rounded frequencies in plots (when `show.accu == TRUE` and `round == TRUE`). 


## Micro changes

### General changes

- More consistent argument and variable names (using snake_case).

- Many additions and corrections in documentation, examples, and vignettes. 

### Details 

- `plot_icons` [2018-03]:   
Bug fix to also swap symbols in legend when the symbol order is changed manually.

- `txt_def` [2018-02]:   
Simplify some default text labels (e.g., for current population, condition, and decision). 

- `.onAttach` [2018-02]:   
Cast dice to display probabilistic (i.e., risk-related) start-up messages. 

---------- 

# riskyr 0.1.0

- Initial release on CRAN: <https://CRAN.R-project.org/package=riskyr> [2018-02-19] 

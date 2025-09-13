
# riskyr 0.5.0

<img src = "./inst/pix/logo.png" alt = "riskyr" align = "right" width = "180" style = "width: 180px; float: right; border:20;"/>

**riskyr 0.5.0** was released [on CRAN](https://CRAN.R-project.org/package=riskyr) on 2025-09-15. 
This version fixes minor issues for CRAN compatibility. 

Changes since last release:  


<!-- Major: --> 

## Major changes

- none yet 


<!-- Minor: --> 

## Minor changes

- New hexagon version of logo


<!-- Micro/details: --> 


## Details

- Updated citation info (using `bibentry()` for CRAN compatibility) 
- Package maintenance:
    - Add GitHub actions (R-CMD-check and Rhub workflows)
    - Update `README` and online documentation 

<!-- Blank line. --> 


<!-- Development version: --> 

The current development version of **riskyr** is available at <https://github.com/hneth/riskyr/>. 

<!-- Previous version: --> 

---------- 


# riskyr 0.4.0

**riskyr 0.4.0** was released [on CRAN](https://CRAN.R-project.org/package=riskyr) on August 15, 2022. 

Changes since last release:  


## Major changes

<!-- Sampling from prob: -->     

- Enable _sampling_ when computing `freq` from `prob` (i.e., _by description_, given `N` and 3 essential probabilities) [2021-03]: 

    - add a `sample` argument to `comp_freq()`, `comp_freq_prob()`, and `riskyr()`;  
    - add a `sample` argument to 6 key plots:  
    `plot_area()`, `plot_bar()`, `plot_fnet()`, `plot_icons()`, `plot_prism()`, and `plot_tab()`.  


<!-- Conversion data vs. description: --> 

- Allow conversions/translations between data and descriptions:

    - `write_popu()` creates population data from a description (a `riskyr` scenario); 
    - `read_popu()` creates a description (a `riskyr` scenario) from population data. 


<!-- Cumulative risks (plot_crisk): -->  

- Add a function for plotting cumulative risk curves:

    - `plot_crisk()` plots curves and auxiliary information for visualizing cumulative risks and risk increments; 
    - `pal_crisk` provides a corresponding color palette (as a named vector).  


<!-- FFTrees: -->

- Add `FFTrees_riskyr()` to convert `FFTrees` objects into corresponding `riskyr` objects. 


<!-- Minor: --> 

## Minor changes

- Add `main` and `sub` arguments to all plots (and deprecate the previous `title_lbl` argument). 
- Add `is_matrix()` to verify a 2x2 matrix (as a 2x2 numeric contingency table). 
- Add `is_integer()` to verify integer values.


<!-- Micro/details: --> 

## Details 

- Fix bug in `plot_bar()` that caused incorrect bar labels.
- Update URLs in `README.md` and all vignettes to <https://riskyr.org/>. 
- Increase options and robustness for labeling frequencies and probabilities in plots. 
- Reduce widths of freq boxes in `plot_prism()` to reduce overlaps.
- Rename labels in `txt_TF` to avoid confusion, as "True condition" (i.e., X) was `FALSE` (now "absent") for "True negatives" (`TN`/`cr` cases). 

<!-- Previous version: --> 


---------- 


# riskyr 0.3.0

**riskyr 0.3.0** was released [on CRAN](https://CRAN.R-project.org/package=riskyr) on March\ 23, 2021. 

Changes since last release:


## Major changes

- Add `plot_fnet()` for plotting _frequency nets_ ([Binder et al., 2020](https://www.frontiersin.org/articles/10.3389/fpsyg.2020.00750/full)) [2020-12]. 


## Minor changes

### Changes to existing visualization functions 

- Change default setting of `mar_notes` in all plotting functions: 
Using `mar_notes = FALSE` as those details are not needed and can be distracting in visualizations. 

- Change default arguments for `plot_area()` and `plot_tab()`:   
Change default settings from `f_lbl_hd = "abb"` to `f_lbl_hd = "nam"` (as this makes more sense for **riskyrApp**).  

- New options for `plot_curve()`:  
Plotting probability curves as a function of prevalence does not require any specific prevalence value. 
Thus, setting the `prev` argument to either `NA` or to a vector of several probabilities is now supported. 
Setting the new `prev_range` argument to a range within `c(0, 1)` allows zooming into more specific ranges of `prev` values (on x-axis).  

- New options for `plot_plane()`:  
Plotting a probability plane as a function of sensitivity and specificity does not require specifying all values. 
Thus, setting the `sens` and `spec` arguments to either `NA` or to a vector of several probabilities is now supported. 
Setting the new `sens_range` and `spec_range` arguments to ranges within `c(0, 1)` allows zooming into more specific ranges of `sens` values (on x-axis) and `spec` values (on y-axis). 

- New options for `plot_prism()`:  
Use the new `p_lwd` and `p_scale` arguments to allow scaling the widths of probability links by current probability values.  


### Data and functions 

- In `data`: Remove data files `df_scenarios.RData` and `df_scenarios.csv`, as they were redundant to `df_scenarios.rda` (loaded from `\data`). 

- Improve `make_cond_lbl()` to allow for `NA` or vectors of several values (for `prev`, `sens`, and `spec`).  

- Add `is_prob_range()` function to verify a range of two probability values (to check new arguments of `plot_curve()` and `plot_plane()`).  

<!-- Blank line. --> 


## Micro changes

### Colors 

- Using colors of `pal` for fill colors of symbols in `plot_icon()`. 

- Add more color palettes:   

    - Add `pal_bwp` (a strict b+w color palette suited for printing purposes) and corresponding special cases to major plotting functions. 
    - Add `pal_unikn` (based on color definitions of the **unikn** package). 

<!-- Blank line. --> 

- Changes to color palettes:  

    - Add a background color `pal[["bg"]]` to all palettes and plots (to preempt different system defaults).  
    - Adopt `pal_mod` -- rather than `pal_mbw` -- as default color scheme `pal` (to highlight `cond_true` and `dec_pos` cases in default plots).  

<!-- Blank line. --> 


### Details

- Add reference to article on theoretical background ([Neth et al., 2021](https://doi.org/10.3389/fpsyg.2020.567817)).  

- Bug fix in `plot_prism()`: Allow plotting simple trees for `nchar(by) == 2`.

- Bug fix in `plot_icons()`: Enforced 2 different symbol types for icon arrays with a binary perspective (`by = cd` or `dc` or `ac`).   

<!-- Previous version: --> 


---------- 


# riskyr 0.2.0

**riskyr 0.2.0** was released [on CRAN](https://CRAN.R-project.org/package=riskyr) on January 03, 2019.

Changes since last release:  

## Major changes

### Additional resources 

- Introduce `riskyrApp` version:   
To use selected `riskyr` functions without the need for coding 
an updated version of `riskyrApp` is available 
at <https://github.com/hneth/riskyrApp/> (R Shiny code) and 
at [https://riskyr.org/](https://riskyr.org/) (interactive online version). 

- Using `pkgdown`:   
Provide package documentation online at <https://hneth.github.io/riskyr/> (latest release version) and <https://hneth.github.io/riskyr/dev/> (current development version). 


### Visualization functions

- Retiring obsolete functions:  
The plotting functions `plot_fnet()` and `plot_tree()` are replaced by `plot_prism()`, and `plot_mosaic()` is replaced by `plot_area()`. This improves functionality (e.g., by providing more consistent options across different plotting functions) and removes dependencies on external packages. 

- New `plot_prism()` function:   
Show a scenario as double frequency tree (by 3 x 2 perspectives) or a frequency tree (in 3 perspectives) with many additional options; combines and replaces the older `plot_fnet()` and `plot_tree()` functions (and removes dependency on the **diagram** package).

- New `plot_area()` function:   
Show a scenario as a mosaic plot of relative proportions (in 3 x 2 x 2 possible versions, with many additional options); replaces the older `plot_mosaic()` function (and removes dependencies on the **grid** and **vcd** packages).

- New `plot_tab()` function:   
Show a scenario as contingency table of frequencies (with row and column sums, and options for showing probabilities); 
a variant of `plot_area()` that does not scale area sizes. 

- New `plot_bar()` function:   
Show scenario frequencies as vertical bars (in various configurations). 


### Under the hood

- Create `plot_util.R` collection of graphical utility functions:   
Define a new `box` object type and various functions for plotting, labeling, and linking them in graphs 
(to remove dependencies on and limitations imposed by other packages). 

- Update `riskyr()` function:   
As an alternative to providing 3 essential frequencies, it is now possible to define a scenario from 4 essential frequencies (and check for consistency with given probabilities). 


## Minor changes

### Changes to existing visualization functions 

- Improve `plot_icons()` function:   
Show icons separated into 2 subsets by 3 perspectives (condition, decision, accuracy), using the same `by` argument as the other plotting functions.

- Improve `plot_curve()` and `plot_plane()` functions:   
Update variable names (to snake_case) and add arguments (e.g., `col_pal`, `lbl_txt` `mar_notes`, etc.) for consistency with newer plotting functions. 
- Add `scale` argument:   
The new plotting functions feature a `scale` argument that allows scaling the size or areas of boxes either by (exact) probability or by (possibly rounded) frequency. When using `scale = "f"`, the probabilities shown are also re-computed from (possibly rounded) frequencies. 

- Improve `plot_fnet()` function:   
Change argument `box.cex` to `cex.lbl` to ensure consistency with `plot_curves()` and `plot_plane()` (and use it to scale arrow labels accordingly). Add a warning when using deprecated argument.

- Improve `plot_mosaic()` function:   
Change Boolean `vsplit` argument to `by = "cd"` vs. `by = "dc"` to ensure consistency with `plot_fnet()` and `plot_tree()`. Add a warning when using deprecated argument.

- Improve `mar_notes` and `plot_mar` arguments:   
Use consistent plot margins and options for showing margin notes for all plots.


### Default objects (lists and vectors)

- Improve `pal` and `freq` arguments:   
Use more consistent color and frequency names (e.g., `cond_true`, `dec_pos`, and `dec_cor` are now names of frequencies and the colors corresponding to these frequencies). 

- Improve `freq` object:   
Add a 3rd perspective (by accuracy or by correspondence of decision to condition) and corresponding frequency pair of `dec.cor` and `dec.err` (i.e., `hi + cr` vs. `mi + fa` as the diagonal of 4 SDT cases). This increases the number of frequencies in `freq` from 9 to 11. Also added corresponding labels in `init_txt.R` and colors in `init_pal.R`. 

- Improve `prob` object:   
Include accuracy metrics in probabilities (in `prob` and summary functions). 

- Improve `pal` and `txt` objects:   
Add multiple color palettes and text labeling schemes (see `?pal` and `?txt` for details). 


### Other changes 

- Improve `read_popu()` function:   
Read a data frame `popu` and interpret is as a `riskyr` scenario, allows creating scenarios from raw data. 

- Add `comp_accu.R` file:   
Compute exact accuracy values (not approximations, when using `comp_accu_freq` on rounded `freq` values) by using the new function `comp_accu_prob()` to compute the list `accu` from probabilities. Signal rounding when showing accuracy based on rounded frequencies in plots (when `show.accu == TRUE` and `round == TRUE`). 


## Micro changes

### General changes

- More consistent argument and variable names (using snake_case).

- Many additions and corrections in documentation, examples, and vignettes. 


### Details 

- `plot_icons()`: Bug fix to also swap symbols in legend when the symbol order is changed manually.

- `txt_def`: Simplify some default text labels (e.g., for current population, condition, and decision). 

- `.onAttach`: Cast dice to display probabilistic (i.e., risk-related) start-up messages. 

<!-- Previous version: --> 

---------- 


# riskyr 0.1.0

**riskyr 0.1.0** was released [on CRAN](https://CRAN.R-project.org/package=riskyr) on February 19, 2018. 


<!-- References: --> 

---------- 

## References

To cite **riskyr** in derivations and publications please use:

- Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2025). 
riskyr: Rendering Risk Literacy more Transparent.  
  Social Psychology and Decision Sciences, University of Konstanz, Germany. Computer software (R package version 0.5.0, Sep 15, 2025). 
  Retrieved from <https://CRAN.R-project.org/package=riskyr>.  


<!-- Background article: -->

The following **article** provides details on the conceptual and theoretical background: 

- Neth, H., Gradwohl, N., Streeb, D., Keim, D.A., & Gaissmaier, W. (2021). 
Perspectives on the 2x2 matrix: Solving semantically distinct problems based on a shared structure of binary contingencies.  
  _Frontiers in Psychology_, _11_, 567817. 
  doi: [10.3389/fpsyg.2020.567817](https://doi.org/10.3389/fpsyg.2020.567817) 


<!-- Footer: -->

---------- 

[`NEWS.md` updated on 2025-09-15.]

<!-- eof -->

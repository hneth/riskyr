
# Current version

The most recent development version (riskyr 0.1.0.928) is available at <https://github.com/hneth/riskyr/>. 

# Changes

Log of changes since last release: 

## Major changes

- `plot_prism` function [2018-11-05]: 
Show a scenario as double frequency tree (in 3 x 2 possible versions, with many additional options); 
replaces `plot_fnet` function (and removes dependencies on the `diagram` package).

- `plot_tab` function [2018-10-30]: 
Show a scenario as contingency table of frequencies (with row and column sums, and options for showing probabilities); 
a variant of `plot_area` that does not scale area sizes. 

- `plot_area` function [2018-10-20]: 
Show a scenario as a mosaic plot of relative proportions (in 3 x 2 x 2 possible versions, with many additional options); 
replaces `plot_mosaic` function (and removes dependencies on the `grid` and `vcd` packages).

- `plot_bar` function [2018-08-15]: 
Show scenario frequencies as vertical bars (in various configurations). 

- `plot_util.R` collection of graphical utility functions [2018-08-15]: 
Define a new `box` object type and various functions for plotting, labeling, and linking them 
(to remove dependencies on and limitations by other packages).

- `riskyr` function [2018-03-06]: 
Define a scenario from 4 essential frequencies (and checking for consistency with probabilities). 


## Minor changes

- `plot_curve` and `plot_plane` [2018-11-02]:
Update variable names (to `snake_case`) and add arguments (e.g., `col_pal`, `lbl_txt` `mar_notes`, etc.) for consistency with newer plotting functions. 

- `prob` [2018-09-04]: 
Include `acc` as a probability (in `prob` and summary functions). 

- `comp_accu.R` [2018-08-30]: 
Compute exact accuracy values (not approximations, when using `comp_accu_freq` on rounded `freq` values) by using the new function `comp_accu_prob` to compute the list `accu` from probabilities. Signal rounding when showing accuracy based on rounded frequencies in plots (when `show.accu == TRUE` and `round == TRUE`). 

- `freq` [2018-07-30]: 
Added a 3rd perspective (by accuracy or by correspondence of decision to condition) and corresponding frequency pair of `dec.cor` and `dec.err` (i.e., `hi + cr` vs. `mi + fa` as the diagonal of 4 SDT cases). This increases the number of frequencies in `freq` from 9 to 11. Also added corresponding labels in `init_txt.R` and colors in `init_pal.R`. 

- `plot_icons` [2018-03-05]: 
Bug fix to also swap symbols in legend when the symbol order is changed manually.

- `plot_mosaic` [2018-02-18]: 
Changed Boolean `vsplit` argument to `by = "cd"` vs. `by = "dc"` to ensure consistency with `plot_fnet` and `plot_tree`. Added warning when using deprecated argument. 

- `plot_fnet` [2018-02-17]: 
Changed argument `box.cex` to `cex.lbl` to ensure consistency with `plot_curves` and `plot_plane` (and use it to scale arrow labels accordingly). Added warning when using deprecated argument.

- `txt_def` [2018-02-17]: 
Simplified some default text labels (e.g., for current population, condition, and decision). 

- `.onAttach` [2018-02-17]: 
Cast dice to display a risk-related start-up message.


## Micro changes

- Miscellaneous additions and corrections in documentation and vignettes. 

# riskyr 0.1.0

- Initial release on CRAN: <https://CRAN.R-project.org/package=riskyr> [2018-02-19] 

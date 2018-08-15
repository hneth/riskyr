
# Current version

The most recent development version (riskyr 0.1.0.906) is available at <https://github.com/hneth/riskyr/>. 

# Changes

Log of changes since last release: 

## Major changes

- New `plot_bar` function [2018-08-14]: 
Show scenario frequencies as vertical bars (in various configurations). 

- `riskyr` function [2018-03-06]: 
Define a scenario from 4 essential frequencies (checking for consistency with probabilities). 
[Appears to work, but yet to be tested more thorougly.] 

## Minor changes

- `plot_icons` [2018-03-05]: 
Bug fix to also swap symbols in legend when the symbol order is changed manually.

- `plot_mosaic` [2018-02-18]: 
Changed Boolean `vsplit` argument to `by = "cd"` vs. `by = "dc"` to ensure consistency with `plot_fnet` and `plot_tree`. Added warning when using deprecated argument. 

- `plot_fnet` [2018-02-17]: 
Changed argument `box.cex` to `cex.lbl` to ensure consistency with `plot_curves` and `plot_plane` (and use it to scale arrow labels accordingly). Added warning when using deprecated argument.

- `txt.def` [2018-02-17]: 
Simplified some default text labels (e.g., for current population, condition, and decision). 

- `.onAttach` [2018-02-17]: 
Cast dice to display a risk-related start-up message.


## Micro changes

- Miscellaneous additions and corrections in documentation and vignettes. 

# riskyr 0.1.0

- Initial release on CRAN: <https://CRAN.R-project.org/package=riskyr> [2018-02-19] 

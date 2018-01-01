---
title: "About riskyr"
author: "SPDS, uni.kn"
date: "2018 01 01"
output: github_document
---

# About `riskyr`

The `riskyr` package provides a toolbox for transparent communication and teaching of risk literacy.

## Goal 

The `riskyr` package aims to render risk literacy more transparent by providing a set of risk literacy tools and corresponding representations.

We start with some global variables:

- population size _N_
- prevalence _prev_ of some condition
- sensitivity _sens_ of some test (or probability of treatment success)
- specificity _spec_ of some test (or probability of side effects)

and provide a variety of perspectives on the consequences of and interplay between these variables:

1. tree of natural frequencies  
2. contingency table (leaves of the frequency tree)  
3. icon array (with a population vs. sample view, sorted or randomized)  
4. curves of PPV and NPV (as a function of prevalence or sensitivity/specificity)
5. fact boxes (with additional details on benefits and harms of tests or treatments)

All visualizations will be interactive and use a common color scheme.

A set of examples illustrates cases with known data from the literature.

## Notes

The easiest way to quickly incorporate text into this shiny app would be to change the contents of `justamarkdownfile.md` (actually, this file contains what's displayed here).   
+ Nice CSS designs are available at [Bootswatch](https://bootswatch.com/3/).    
+ Bootstrap also provides [these icons](https://www.w3schools.com/icons/bootstrap_icons_glyphicons.asp), so let's use them.   
+ For even more visualisation or branding, we can use or create icons from [here](https://www.flaticon.com/authors/vectors-market).    
+ Static content goes in the www directory.    

## To Do

### Done

- read 4 basic variables from input
- use inputs to construct representation as data frame
- display data frame as table
- display 2x2 contingency table
- display contingency table as mosaic plot
- use `diagram` to represent tree of natural frequencies
- define some basic colors (e.g, for hi, fa, mi, cr, but also PPV and NPV)   
- provide 2D plot for curves of PPV/NPV by `prev` range
- provide 3D plot for planes of PPV/NPV by `sens` and `spec` (for given `prev`)
- provide options to show current PPV/NPV in plots
- add option to load environments (parameter sets of examples and real studies (from `.csv` file)

### Yet To Do

#### Basic functionality:

- enable slider inputs of population size _N_ on log scale (to allow precise small and large numbers)
- enable precise inputs of parameters `prev`, `sens`, and `spec` (e.g. as ratios) 
- define plot of icon array
- provide options for 
     - shuffling current population and 
     - sampling from population 
  in data table and icon array)
  
#### For UI:

- add current colors and metrics to tabs of nf `tree`, contingency `table`, and mosaic plot.
- make current set of colors user customizable 
- allow saving and exporting current parameters set (environments)
- allow saving and exports of graphs 

#### For background info:

- add definitions, verbal explanations, and examples (and corresponding help and tooltips)
- add more environments from existing examples (into `.csv` file)
- use logo of uni.kn
- add quizz to test knowledge

#### For package and app development:

- restructure code according to recommendations on 
    - http://r-pkgs.had.co.nz/ and 
    - https://deanattali.com/2015/04/21/r-package-shiny-app/ 
- provide explanatory help vignettes

## Contact

Feel free to contact us at http://spds.uni-konstanz.de.

<!-- eof -->
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

- population size `N`
- prevalence `prev` of some condition
- sensitivity `sens` of some test (or probability of treatment success)
- specificity `spec` of some test (or probability of side effects)

and provide a variety of perspectives on (and representations of) the consequences of and interplay between these variables:

1. a table of individual cases
2. an icon array (with a population vs. sample view, sorted or randomized) 
3. a tree of natural frequencies  
4. a 2x2 confusion/contingency table (corresponding to the leaves of the frequency tree)  
5. a mosaic plot (illustrating the confusion table) 
6. curves of PPV and NPV (as a function of `prev`) 
7. planes of PPV and NPV (as a function of `sens` and `spec` for given `prev`)
8. fact boxes (with additional details on benefits and harms of tests or treatments)

All visualizations shall be interactive and use a common color scheme.

A set of examples illustrates cases with known data from the literature.

## Notes

The easiest way to quickly incorporate text into this shiny app would be to change the contents of `about_riskyr.md` (actually, this file contains what's displayed here).   

+ All static content goes in the www directory.
+ Nice CSS designs are available at [Bootswatch](https://bootswatch.com/3/).    
+ Bootstrap also provides [these icons](https://www.w3schools.com/icons/bootstrap_icons_glyphicons.asp), so let's use them.   
+ For even more visualisation or branding, we can use or create icons from [here](https://www.flaticon.com/authors/vectors-market).    

## Agenda

### Done

- read environment (4 basic variables) from input
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

### Yet to do

#### A. Basic functionality:

User inputs:

- display 4 current environment parameters on left column
- make current title smaller and add current source information in 2nd text field
- enable slider inputs of population size _N_ on log scale (to allow precise small and large numbers)
- enable precise inputs of parameters `prev`, `sens`, and `spec` (e.g. as ratios) 

Representations:

- add current metrics (accuracy measures, PPV/NPV) to tabs of nf `tree`, contingency `table`, and mosaic plot  
- define plot and options for _icon array_
- provide options for 
     - shuffling current population and 
     - sampling from population 
  in data table and icon array)

#### B. User interface:

- add current colors to tabs of nf `tree`, contingency `table`, and mosaic plot.
- make current set of colors user customizable 
- allow saving and exporting current parameters set (environments)
- allow saving and exports of graphs 

#### C. Background info:

- add definitions, verbal explanations, and examples (and corresponding help and tooltips)
- add more environments from existing examples (into `.csv` file)
- use logo of uni.kn
- add quizz to test knowledge

#### D. Package and app development:

- restructure code according to recommendations on 
    - http://r-pkgs.had.co.nz/ and 
    - https://deanattali.com/2015/04/21/r-package-shiny-app/ 
- provide explanatory help vignettes

## Contact

Contact us at [spds.uni.kn](http://spds.uni-konstanz.de).
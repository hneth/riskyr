---
title: "About riskyr"
author: "SPDS, uni.kn"
date: "2018 01 01"
output: github_document
---

# riskyr

The _riskyr_ package provides a toolbox for transparent communication and teaching of risk literacy.

## Goal 

We aim to render risk literacy more transparent by developing and providing a set of risk literacy tools (and corresponding representations). 

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

For UI:

- read 4 basic variables from input
- use inputs to construct representation as data frame
- display data frame as table
- display 2x2 contingency table
- display contingency table as mosaic plot
- use `diagram` to represent tree of natural frequencies
- define colors (for hi, fa, mi, cr)    
- define icon array
- provide options for shuffling and sampling population
- use logo of uni.kn
- add some dropdown menu tied to `switch` to have parameters from real studies (placed in `.csv` file in the static folder)

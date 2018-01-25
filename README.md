riskyr
======
SPDS, uni.kn
2017 12 20

# The `riskyr` package

A toolbox for rendering risk literacy more transparent


## Motivation

> Solving a problem simply means representing it<br>
> so as to make the solution transparent. (H.A. Simon)[^1]

[^1]: Simon, H.A. (1996). _The Sciences of the Artificial_ (3rd ed.). The MIT Press, Cambridge, MA. (p. 132).


The basic assumptions and motivations behind `riskyr` are the following:

- Effective training in risk literacy requires simple and transparent representations. 

- We provide a set of tools that allow alternative views on the interplay between probabilities and frequencies. Different representations are not isolated, but complement and explain each other.

- All visualizations can be explored interactively, providing immediate feedback on the effect of changes in parameters. By providing many options, users can customize representations to suit their goals and needs.


## Rationale

We begin with some basic variables:

-   a population size `N`
-   a prevalence `prev` of some condition
-   a sensitivity `sens` of some test (or probability of treatment success)
-   a specificity `spec` of some test (or probability of side effects)

and provide a variety of _perspectives_ on (and representations of) the _consequences_ of and _interplay_ between these variables:

1.  a _data table_ of individual cases  
2.  an _icon array_ (with a population vs. sample view, sorted or randomized)  
3.  a _tree of natural frequencies_  
4.  a _2x2 confusion/contingency table_ (corresponding to the leaves of the frequency tree)  
5.  a _mosaic plot_ that illustrates the proportions of the confusion table  
6.  a curve of _predictive values_ (PPV and NPV) as a function of `prev`  
7.  a plane of _predictive values_ (PPV and NPV) as a function of `sens` and `spec` for a given `prev`  
    <!-- 8. fact boxes (with additional details on benefits and harms of tests or treatments)  -->

A _library of scenarios_ illustrates example cases with known data from the literature.


## Package and Application

Our objective is to collect and develop a set of basic risk literacy tools in R.  To maximise impact, we split our efforts into two complementary projects:

1. The `riskyr` package renders risk literacy more transparent by providing a set of risk-related tools and corresponding representations.

2. The corresponding R Shiny app `riskyrApp` allows using the `riskyr` toolbox in an interactive fashion without any coding.

The combination of package and application facilitates risk communication and supports instruction and training efforts in promoting risk literacy.


## ToDo

Things to implement in the near future:

- Restructure to define 5 basic vs. 4 derived probabilities, 9 basic frequencies, initialize `num`, 9 probabilities in `probs`, and 9 frequencies in `freq`.

- Miss rate `mirt` (as a complement to `sens`), to complete the quadruple of probabilities (yielding the 4 SDT cases or leaves of natural frequency tree).

- `hi`, `mi`, `fa`, `rt` as 4 basic frequencies (corresponding to rates).

- functions that compute all basic probabilities (`prev`, `sens`, `spec`, `fart`, and `mirt`) from 4 basic frequencies (in `freq`)

- All metrics (accuracy, etc.) on 2x2 confusion table


## About

This software is released to the public domain ([CC0](https://tldrlegal.com/license/creative-commons-cc0-1.0-universal)) 
and can be freely used by anyone for any purpose. [Contact us](http://spds.uni-konstanz.de) in case you want to use, adapt, or share it.

### Contact

In case of comments or questions, please contact us at [http://spds.uni.kn](http://spds.uni-konstanz.de). 

### Reference

In APA format:

- Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2018).  
`riskyr`: A toolbox for rendering risk literacy more transparent [Computer software]. 
Retrieved from https://github.com/hneth/riskyr (R package version 0.0.0.908)

As BibTeX reference: 

    @manual{riskyr2018,
      title = {{riskyr}: A toolbox for rendering risk literacy more transparent},
      author = {Neth, Hansj\"{o}rg and Gaisbauer, Felix and Gradwohl, Nico and Gaissmaier, Wolfgang}, 
      year = 2018,
      note = {R package version 0.0.0.908},
      url = {https://github.com/hneth/riskyr}
    }

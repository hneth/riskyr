riskyr
======
SPDS, uni.kn
2018 01 06

The `riskyr` package
====================

A toolbox for transparent communication and teaching of risk literacy.

Goal
----

Develop and assemble a set of basic risk literacy tools in R: 

- The `riskyr` package renders risk literacy more transparent by providing a set of risk literacy tools and corresponding representations.

- The corresponding R Shiny app `riskyrApp` allows using the `riskyr` toolbox in an interactive fashion without any coding.

Rationale
---------

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

All visualizations can be explored interactively via the corresponding `riskyrApp` and customized extensively.

A _library of scenarios_ illustrates example cases with known data from the literature.

## About

We are still developing this software and have yet to choose a license for it. Please [contact us](http://spds.uni-konstanz.de) in case you want to use or share it.

### Contact

In case of comments or questions, please contact us at [http://spds.uni.kn](http://spds.uni-konstanz.de). 

### Reference

In APA format:

- Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2018).  
`riskyr`: A toolbox for transparent communication and teaching of risk literacy [Computer software]. 
Retrieved from https://github.com/hneth/riskyr (R package version 0.0.0.900)

As BibTeX reference: 

    @manual{riskyr2018,
      title = {{riskyr}: A toolbox for transparent communication and teaching of risk literacy},
      author = {Neth, Hansj\"{o}rg and Gaisbauer, Felix and Gradwohl, Nico and Gaissmaier, Wolfgang}, 
      year = 2018,
      note = {R package version 0.0.0.900},
      url = {https://github.com/hneth/riskyr}
    }

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


The issues addressed by `riskyr` are less of a _computational_ than of a _representational_ nature (i.e., concerning the translation between different information formats).  Whereas people find it difficult to understand and compute information expressed in terms of _probabilities_, the same information content expressed in terms of _frequencies_ is often easy to understand and compute. 

However, rather than just expressing probabilistic information in terms of frequencies, `riskyr` allows translating between formats and illustrates their relationships in transparent and interactive visualizations.


The basic assumptions and goals motivating the development of `riskyr` are the following:

1. Effective training in risk literacy requires simple tools and transparent representations. 

2. We provide a set of (computational and representational) tools that allow various calculations and translations, 
as well as alternative views on the interplay between probabilities and frequencies. 
The toolbox of functions and representations are not isolated, but complement and explain each other.

3. To facilitate active and explorative learning, all visualizations can be explored interactively, 
providing immediate feedback on the effect of changes in parameters.  
By providing many options, users can customize representations to suit their goals and needs.


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


## Features

### Ontology 

The `riskyr` universe describes the interplay between a total of 10 probabilities (3 of which are essential) 
and 9 frequencies (4 of which are essential). 

### Two perspectives

Classification results can be viewed from two perspectives, 
which correspond to two possible ways to split a population of `N` indivduals into subsets:

1. By _condition_: `TRUE` vs. `FALSE`, then by _decision_: `hi`, `mi`, `fa`, `cr`

2. By _decision_: `pos` vs. `neg`, then by _condition_: `hi`, `mi`, `fa`, `cr`


### Translating between representational formats

A scenario is represented both in terms of probabilities and in terms of frequencies. 

A set of conversion functions allow switching back and forth between both formats (i.e., compute frequencies from probabilities and probabilities from frequencies). 




## Package and Application

Our objective is to collect and develop a set of basic risk literacy tools in R.  To maximise impact, we split our efforts into two complementary projects:

1. The `riskyr` package renders risk literacy more transparent by providing a set of risk-related tools and corresponding representations.

2. The corresponding R Shiny app `riskyrApp` allows using the `riskyr` toolbox in an interactive fashion without any coding.

The combination of package and application facilitates risk communication and supports instruction and training efforts in promoting risk literacy.


## ToDo

Things to implement in the near future:

- Enhanced tree function (to include `by decision` perspective) and all 10 probabilities (as edges).

- Derived metrics (accuracy, etc.) of the 2x2 confusion table


## About

This software is released to the public domain ([CC0](https://tldrlegal.com/license/creative-commons-cc0-1.0-universal)) 
and can be freely used by anyone for any purpose. [Contact us](http://spds.uni-konstanz.de) in case you want to use, adapt, or share it.

### Contact

In case of comments or questions, please contact us at [http://spds.uni.kn](http://spds.uni-konstanz.de). 

### Reference

In APA format:

- Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2018).  
`riskyr`: A toolbox for rendering risk literacy more transparent [Computer software]. 
Retrieved from https://github.com/hneth/riskyr (R package version 0.0.0.910)

As BibTeX reference: 

    @manual{riskyr2018,
      title = {{riskyr}: A toolbox for rendering risk literacy more transparent},
      author = {Neth, Hansj\"{o}rg and Gaisbauer, Felix and Gradwohl, Nico and Gaissmaier, Wolfgang}, 
      year = 2018,
      note = {R package version 0.0.0.910},
      url = {https://github.com/hneth/riskyr}
    }

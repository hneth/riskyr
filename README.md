riskyR
================
SPDS, uni.kn
2018 01 06


The `riskyR` package
====================

A toolbox for transparent communication and teaching of risk literacy.

Goal
----

Develop and assemble a set of basic risk literacy tools in R: 

- The `riskyR` package renders risk literacy more transparent by providing a set of risk literacy tools and corresponding representations.

- The corresponding R Shiny app `riskyRapp` allows using the `riskyr` toolbox in an interactive fashion without any coding.

Rationale
---------

We begin with some basic variables:

-   a population size `N`
-   a prevalence `prev` of some condition
-   a sensitivity `sens` of some test (or probability of treatment success)
-   a specificity `spec` of some test (or probability of side effects)

and provide a variety of *perspectives* on (and representations of) the *consequences* of and *interplay* between these variables:

1.  a *data table* of individual cases
2.  an *icon array* (with a population vs. sample view, sorted or randomized)
3.  a *tree of natural frequencies*
4.  a *2x2 confusion/contingency table* (corresponding to the leaves of the frequency tree)
5.  a *mosaic plot* that illustrates the proportions of the confusion table
6.  two *curves of predictive values* (PPV and NPV) as a function of `prev`
7.  two *planes of predictive values* (PPV and NPV) as a function of `sens` and `spec` for a given `prev`
    <!-- 8. fact boxes (with additional details on benefits and harms of tests or treatments)  -->

All visualizations can be explored interactively and use a common color scheme.

A set of environments illustrates example cases with known data from the literature.


## About

We are still developing this software and have yet to choose a license for it. Please [contact us](http://spds.uni-konstanz.de) in case you want to use or share it.

### Contact

In case of comments or questions, please contact us at [http://spds.uni.kn](http://spds.uni-konstanz.de). 

### Reference

In APA format:

- Neth, H., Gaissbauer, F., Gradwohl, N., & Gaissmaier, W. (2018). 
`riskyR`: A toolbox for transparent communication and teaching of risk literacy 
[Computer software]. Retrieved from https://github.com/hneth/riskyR (R package version 0.0.1)

As BibTeX reference: 

    @manual{NethEtAl18_R,
      title = {{riskyR}: A toolbox for transparent communication and teaching of risk literacy},
      author = {Neth, Hansj\"{o}rg and Gaissbauer, Felix and Gradwohl, Nico and Gaissmaier, Wolfgang}, 
      year = 2018,
      note = {R package version 0.0.1},
      url = {https://github.com/hneth/riskyR}
    }


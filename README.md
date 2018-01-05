riskyR
================
SPDS, uni.kn
2018 01 05


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


Contact
-------

Contact us at [http://spds.uni.kn](http://spds.uni-konstanz.de).



About riskyr
================
SPDS, uni.kn
2018 01 02

About `riskyr`
==============

A toolbox for transparent communication and teaching of risk literacy.

Goal
----

The `riskyr` package renders risk literacy more transparent by providing a set of risk literacy tools and corresponding representations.

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

Notes
-----

The easiest way to quickly incorporate text into this shiny app would be to change the contents of `about_riskyr.md` (actually, this file contains what's displayed here).

-   All static content goes in the www directory.
-   Nice CSS designs are available at [Bootswatch](https://bootswatch.com/3/).
-   Bootstrap also provides [these icons](https://www.w3schools.com/icons/bootstrap_icons_glyphicons.asp), so let's use them.
-   For even more visualisation or branding, we can use or create icons from [here](https://www.flaticon.com/authors/vectors-market).

Agenda
------

### Done

-   read environment (4 basic variables) from input
-   use inputs to construct representation as data frame
-   display data frame as table
-   display 2x2 contingency table
-   display contingency table as mosaic plot
-   use `diagram` to represent tree of natural frequencies
-   define some basic colors (e.g, for hi, fa, mi, cr, but also PPV and NPV)
-   provide 2D plot for curves of PPV/NPV by `prev` range
-   provide 3D plot for planes of PPV/NPV by `sens` and `spec` (for given `prev`)
-   provide options to show current PPV/NPV in plots
-   add option to load environments (parameter sets of examples and real studies (from `.csv` file)

### Yet to do

#### A. Basic functionality:

User inputs:

-   display the 4 current environment parameters (in the left panel)
-   make current title smaller and add current source information in 2nd text field
-   enable slider inputs of population size *N* on log scale (to allow precise small and large numbers)
-   enable precise inputs of parameters `prev`, `sens`, and `spec` (e.g. as ratios)

Representations:

-   add current metrics (accuracy measures, PPV/NPV) to tabs of nf `tree`, contingency `table`, and mosaic plot
-   define plot and options for *icon array*
-   provide options for
    -   shuffling current population and
    -   sampling from population in data table and icon array)

#### B. User interface:

-   update environment name and source (used in plots) when loading a data set
-   add current colors to tabs of nf `tree`, contingency `table`, and mosaic plot.
-   make current set of colors user customizable
-   allow saving and exporting current parameters set (environments)
-   allow saving and exports of graphs

#### C. Background info:

-   add definitions and explanations
-   add help information and tooltips
-   add more environments from existing examples (into `.csv` file)
-   use logos for app and institutions
-   add a quizz to test user knowledge (or motivate to explore app)

#### D. Package and app development:

-   choose a license for code sharing
-   restructure code according to recommendations on
    -   <http://r-pkgs.had.co.nz/> and
    -   <https://deanattali.com/2015/04/21/r-package-shiny-app/>
-   provide explanatory help vignettes

Contact
-------

Contact us at [http://spds.uni.kn](http://spds.uni-konstanz.de).

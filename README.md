
<!-- README.md is generated from README.Rmd. Please only edit the latter (.Rmd) file! -->
<!-- Status badges: -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/riskyr)](https://CRAN.R-project.org/package=riskyr) [![Build Status](https://travis-ci.org/hneth/riskyr.svg?branch=master)](https://travis-ci.org/hneth/riskyr) [![Downloads](http://cranlogs.r-pkg.org/badges/riskyr?color=brightgreen)](http://www.r-pkg.org/pkg/riskyr) [![Rdoc](http://www.rdocumentation.org/badges/version/riskyr)](http://www.rdocumentation.org/packages/riskyr)

<!-- riskyr logo: -->
riskyr <img src = "logo.png" align = "right" alt = "riskyr" width = "160" />
============================================================================

<!-- riskyr logo: -->
<!-- 
<a href = "https://github.com/hneth/riskyr">
<img alt = "riskyr logo" title = "riskyr" src = "./inst/pix/riskyr_cube.png" width = "180px" align = "right" style = "float:right; border:20; width:180px;"/>
</a> 
-->
<!-- <img src = "./inst/pix/riskyr_cube_s.png" alt = "riskyr" align = "right" style = "float: right; border:20;"/> -->
<!-- ![riskyr](./inst/pix/riskyr_cube_s.png) -->
<!-- knitr::include_graphics("./inst/pix/riskyr_cube_s.png") -->
### A toolbox for rendering risk literacy more transparent

Risk-related information — like the prevalence of conditions and the sensitivity and specificity of diagnostic tests or treatment decisions — can be expressed in terms of probabilities or frequencies. By providing a toolbox of methods and metrics, `riskyr` computes, translates, and displays risk-related information in a variety of ways. Offering multiple complementary perspectives on the interplay between key parameters renders teaching and training of risk literacy more transparent.

Motivation
----------

> Solving a problem simply means representing it <br> so as to make the solution transparent. (H.A. Simon)[1]

<!-- Risk perception as representational effects: -->
The goals of `riskyr` are less of a *computational* and more of a *representational* nature, by addressing the expression in and translation between different formats of risk-related information. Whereas people find it difficult to understand and compute information expressed in terms of probabilities, the same information is easier to understand and compute when expressed in terms of frequencies. But rather than just expressing probabilities in terms of frequencies, `riskyr` allows translating between formats and illustrates the relationships between different representations in a variety of ways. Switching between and interacting with different representations fosters transparency and boosts human understanding of risk-related information.[2]

<!-- Defining "risk" (in footnote): -->
<!-- On "riskyr": -->
Basic assumptions and goals driving the current development of `riskyr` include:

1.  Effective training in risk literacy requires transparent representations, smart strategies, and simple tools.

2.  We aim for a set of (computational and representational) tools that facilitate various calculations, translations between formats, and a range of alternative views on the interplay between probabilities and frequencies.

3.  A variety of visualizations that illustrate the interplay of parameters and metrics facilitate active and explorative learning. It is particularly helpful to view relationships from alternative perspectives and to observe the change of one parameter as a function of others.

Based on these assumptions and goals, we provide a range of computational and representational tools. Importantly, the objects and functions in the `riskyr` toolbox are not isolated, but complement, explain, and support each other. All functions and visualizations can also be used separately and explored interactively, providing immediate feedback on the effect of changes in parameter values. By providing a variety of customization options, users can explore and design representations of risk-related information that suit their personal needs and goals.

Installation and version information
------------------------------------

-   The current release of `riskyr` is available from [CRAN](https://CRAN.R-project.org/) at <https://CRAN.R-project.org/package=riskyr>:

``` r
install.packages("riskyr")  # install riskyr from CRAN client
library("riskyr")           # load to use the package
```

-   The most recent development version can be installed from its [GitHub](https://github.com) repository at <https://github.com/hneth/riskyr/>:

``` r
# install.packages("devtools")
devtools::install_github("hneth/riskyr")
```

-   An interactive online version is available at <http://riskyr.org>.

-   The package documentation is available at <https://hneth.github.io/riskyr/>.

Quick start guide
-----------------

<!-- 1: Defining a scenario -->
### Defining a scenario

`riskyr` is designed to address problems like the following:[3]

<!-- Example: -->
> **Screening for hustosis**
>
> A screening device for detecting the clinical condition of *hustosis* is developed. The current device is very good, but not perfect. We have the following information:
> 1. About 4% of the people of the general population suffer from *hustosis*.
> 2. If someone suffers from hustosis, there is a chance of 80% that he or she will test positively for the condition.
> 3. If someone is free from hustosis, there is a chance of 5% that he or she will still test positively for the condition.
>
> Mr. and Ms. Smith have both been screened with the device:
> - Mr. Smith tested positively (i.e., received a diagnosis of hustosis).
> - Ms. Smith tested negatively (i.e., was judged to be free of hustosis).
>
> Please answer the following questions:
> - What is the probability that Mr. Smith actually suffers from hustosis?
> - What is the probability that Ms. Smith is actually free of hustosis?

#### Probabilities provided

The first challenge in solving such problems is in understanding the information that is being provided. The problem description provides three essential probabilities:

1.  The condition's *prevalence* (in the general population) is 4%: `prev = .04`.
2.  The device's or diagnostic decision's *sensitivity* is 80%: `sens = .80`.
3.  The device's or diagnostic decision's *false alarm rate* is 5%: `fart = .05`, implying a *specificity* of (100% − 5%) = 95%: `spec = .95`.

#### Understanding the questions asked

The second challenge here lies in understanding the questions that are being asked — and in realizing that their answers are *not* simply the decision's sensitivity or specificity values. Instead, we are asked to provide two *conditional* probabilities:

-   The conditional probability of suffering from the condition given a positive test result, <br>aka. the *positive predictive value* (`PPV`).
-   The conditional probability of being free of the condition given a negative test result, <br>aka. the *negative predictive value* (`NPV`).

#### Translating into frequencies

One of the best tricks in risk literacy education is to translate probabilistic information into frequencies.[4] To do this, we imagine a representative sample of `N = 1000` individuals. Rather than asking about the probabilities for Mr. and Ms. Smith, we could re-frame the questions as:

> Assuming a representative sample of 1000 individuals:
> - What proportion of individuals with a positive test result actually suffer from hustosis?
> - What proportion of individuals with a negative test result are actually free of hustosis?

#### Using `riskyr`

Here is how `riskyr` allows you to view and solve such problems:

``` r
library(riskyr)  # loads the package
```

#### Creating a scenario from probabilities

We define a new `riskyr` scenario (called `hustosis`) with the information provided by our problem:

``` r
hustosis <- riskyr(scen_lbl = "Example", 
                   cond_lbl = "Hustosis",
                   dec_lbl = "Screening test",
                   popu_lbl = "Sample", 
                   N = 1000,  # population size
                   prev = .04, sens = .80, spec = (1 - .05)  # 3 probabilities
                   )
```

By providing the argument `N = 1000` we define the scenario for a target population of 1000 people. If we leave this parameter unspecified (or `NA`), `riskyr` will automatically pick a suitable value of `N`.

#### Summary

To obtain a quick overview of key parameter values, we ask for the `summary` of `hustosis`:

``` r
summary(hustosis)  # summarizes key parameter values: 
```

The summary distinguishes between probabilities, frequencies, and accuracy information. In `Probabilities` we find the answer to both of our questions that take into account all the information provided above:

-   The conditional probability that Mr. Smith actually suffers from hustosis given his positive test result is 40% (as `PPV = 0.400`).

-   The conditional probability that Ms. Smith is actually free of hustosis given her negative test result is 99.1% (as `NPV = 0.991`).

If find these answers surprising, you are an ideal candidate for additional insights into the realm of risk literacy. A key component of `riskyr` is to analyze and view a scenario from a variety of different perspectives. To get you started immediately, we only illustrate some introductory commands here and focus on different types of visualizations. (Call `riskyr.guide()` for various vignettes that provide more detailed information.)

#### Creating a scenario from frequencies

Rather than defining our `hustosis` scenario by providing 3 essential probabilities (`prev`, `sens`, and `spec`), we could define the same scenario by providing 4 essential frequencies (`hi`, `mi`, `fa`, and `cr`) as follows:

``` r
hustosis_2 <- riskyr(scen_lbl = "Example", 
                     cond_lbl = "Hustosis",
                     dec_lbl = "Screening test",
                     popu_lbl = "Sample", 
                     hi = 32, mi = 8, fa = 48, cr = 912  # 4 key frequencies
                     )
```

As we took the values of these frequencies from the `summary` of `hustosis`, the `hustosis_2` scenario should contain exactly the same information as `hustosis`:

``` r
all.equal(hustosis, hustosis_2)  # do both contain the same information? 
#> [1] TRUE
```

### Visualizations

Various visualizations of `riskyr` scenarios can be created by a range of plotting functions.

#### Prism plot

The default type of plot used in `riskyr` is a *prism plot* (or network diagram) that shows key frequencies of a scenario as nodes and key probabilities as edges linking the nodes:

``` r
plot(hustosis)  # default plot

# => internally calls plot_prism(...) with many additional arguments:
# plot(hustosis, type = "prism", by = "cddc", area = "no", f_lbl = "num", p_lbl = "mix")
```

![Prism plot](inst/pix/README-ex1_prism-1.png)

#### Tree diagram

A *tree diagram* is the upper half of a prism plot, which can be obtained by plotting a scenario with 1 of 3 perspectives:

1.  by condition (`by = "cd"`), to split the population into *TRUE* vs. *FALSE* (`cond.true` vs. `cond.false`) cases;
2.  by decision (`by = "dc"`), to split the population into *negative* vs. *positive* (`dec.neg` vs. `dec.pos`) decisions;
3.  by accuracy (`by = "ac"`), to split the population into *correct* vs. *incorrect* (`dec.cor` vs. `dec.err`) decisions.

For instance, the following command plots a frequency tree by decisions:

``` r
plot(hustosis, by = "dc")  # plot a tree diagram (by decision)
```

![Tree diagram](inst/pix/README-ex1_tree-1.png)

This particular tree splits the population of `N = 1000` individuals into two subgroups *by decision* (`by = "dc"`) and contains the answer to the second (frequency) version of our questions:

-   The proportion of individuals with a positive test result who actually suffer from hustosis is the frequency of "true positive" cases (shown in darker green) divided by "decision positive" cases (shown in purple): `32/80 = .400` (corresponding to our value of `PPV` above).
-   The proportion of individuals with a negative test result who are actually free from hustosis is the frequency of "true negative" cases (shown in lighter green) divided by "decision negative" cases (shown in blue): `912/920 = .991` (corresponding to our value of `NPV` above, except for minimal differences due to rounding).

Of course, the frequencies of these ratios were already contained in the `hustosis` summary above. But the representation in the form of a tree diagram makes it easier to understand the decomposition of the population into subgroups and to see which frequencies are required to answer a particular question.

#### Icon array

An icon array shows the classification result for each of `N = 1000` individuals in our population:

``` r
plot(hustosis, type = "icons")   # plot an icon array 
```

![Icon array](inst/pix/README-ex1_icons-1.png)

While this particular icon array is highly regular (as both the icons and classification types are ordered), `riskyr` provides many different versions of this type of graph. This allows viewing the probability of diagnostic outcomes as either frequency, area, or density (see `?plot_icons` for details and examples).

#### Area plot

An area plot (or mosaic plot) offers a way of expressing classification results as the relationship between areas. Here, the entire population is represented as a square and the probability of its subgroups as the size of rectangles (see `?plot_area` for details and examples):

``` r
plot(hustosis, type = "area")  # plot an area/mosaic plot (by = "cddc")
```

![Area/mosaic plot](inst/pix/README-ex1_area-1.png)

#### Table plot

When not scaling the size of rectangles by their relative frequencies or probabilities, we can plot basic scenario information as a 2-by-2 confusion (or contingency) table (see `?plot_tab` for details and examples):

``` r
plot(hustosis, type = "tab")  # plot 2x2 confusion table (by = "cddc") 
```

![Table plot](inst/pix/README-ex1_tab-1.png)

#### Bar plot

A *bar plot* allows comparing relative frequencies as the heights of bars (see `?plot_bar` for details and examples):

``` r
plot(hustosis, type = "bar", f_lbl = "abb")  # plot bar chart (by "all" perspectives): 
```

![Bar plot](inst/pix/README-ex1_bar-1.png)

#### Curves

By adopting a functional perspective, we can ask how the values of some probabilities (e.g., the predictive values `PPV` and `NPV`) change as a function of another (e.g., the condition's prevalence `prev`, see `?plot_curve` for details and examples):

``` r
plot(hustosis, type = "curve", uc = .05)   # plot probability curves (by prevalence):
```

![Probability curves](inst/pix/README-ex1_curve-1.png)

#### Planes

When parameter values systematically depend on two other parameters, we can plot this as a plane in a 3D cube. The following graph plots the `PPV` as a function of the sensitivity (`sens`) and specificity (`spec`) of our test for a given prevalence (`prev`, see `?plot_plane` for details and examples):

``` r
plot(hustosis, type = "plane")  # plot probability plane (by sens x spec):
```

![Probability plane](inst/pix/README-ex1_plane-1.png)

The L-shape of this plane reveals a real problem with our current test: Given a prevalence of 4% for hustosis in our target population, the `PPV` remains very low for the majority of the possible range of sensitivity and specificity values. To achieve a high `PPV`, the key requirement for our test is an extremely high specificity. Although our current specificity value of 95% (`spec = .95`) may sound pretty good, it is still not high enough to yield a `PPV` beyond 40%.

<!--

#### Plots currently under development

The current development version of `riskyr` (version 0.1.0.930+) replaces some of the above plots with a set of more powerful and more integrated functions. To use them and preview their effects, install the development version and explore the following commands:


```r
## Install current development version: 
# install.packages("devtools")
devtools::install_github("hneth/riskyr")

## Preview latest functions (riskyr > v0.0.1.930):
plot(hustosis, type = "prism", by = "cddc")  
plot(hustosis, type = "tree",  by = "ac") 
plot(hustosis, type = "area",  by = "cddc")
plot(hustosis, type = "tab",   by = "cddc")
plot(hustosis, type = "bar",   dir = 2)
```

-->
<!-- 2: Loading and using pre-defined scenarios -->
### Using existing scenarios

As defining your own scenarios can be cumbersome and the literature is full of risk-related problems (often referred to as "Bayesian reasoning"), `riskyr` provides a set of — currently 24 — pre-defined scenarios (stored in a list `scenarios`). Here, we provide an example that shows how you can select and explore them.

#### Selecting a scenario

Let us assume you want to learn more about the controversy surrounding screening procedures of prostate-cancer (known as PSA screening). Scenario 10 in our collection of `scenarios` is from an article on this topic (Arkes & Gaissmaier, 2012). To select a particular scenario, simply assign it to an R object. For instance, we can assign Scenario 10 to `s10`:

``` r
s10 <- scenarios$n10  # assign pre-defined Scenario 10 to s10
```

#### Scenario summary

Our selected scenario object `s10` is a list with 30 elements, which describe it in both text and numeric variables. The following commands provide an overview of `s10` in text form:

``` r
s10$scen_lbl   # a descriptive label
#> [1] "PSA test (patients)"
s10$cond_lbl   # the current condition
#> [1] "Prostate cancer"
s10$dec_lbl    # the current decision
#> [1] "PSA-Test"
s10$popu_lbl   # the current population
#> [1] "Male patients with symptoms"
s10$scen_apa   # scenario source (APA) 
#> [1] "Arkes, H. R., & Gaissmaier, W. (2012). Psychological research and the prostate-cancer screening controversy. Psychological Science, 23(6), 547--553."

# summary(s10) # summarises a scenario
```

Generating some `riskyr` plots allows a quick visual exploration of the scenario. We only illustrate some selected plots and options here, and trust that you will play with and explore the rest for yourself.

#### Prism plots

A tree diagram is a prism plot that views the population from only one perspective, but provides a quick overview. In the following plot, the boxes are depicted as squares with area sizes that are scaled by relative frequencies (using the `area = "sq"` argument):

``` r
plot(s10, type = "tree", by = "cd", area = "sq",  # tree/prism plot with scaled squares 
     f_lbl = "def", f_lbl_sep = ":\n")            # custom frequency labels 
```

![Prism/tree plot (with scaled squares)](inst/pix/README-ex2_tree-1.png)

The prism plot (or network diagram) combines 2 tree diagrams to simultaneously provide two perspectives on a population (see Wassner et al., 2004). `riskyr` provides several variants of prism plots. To avoid redundancy to the previous tree diagram, the following version splits the population by accuracy and by decision (see the `by = "acdc"` argument). In addition, the frequencies are represented as horizontal rectangles (`area = "hr"`) so that their relative width reflect the number of people in the corresponding subgroup:

``` r
plot(s10, type = "prism", by = "acdc", area = "hr",  # prism plot with horizontal rectangles
     p_lbl = "num")                                  # numeric probability labels
```

![Prism plot (with scaled horizontal rectangles)](inst/pix/README-ex2_prism_hr-1.png)

#### Icon array

``` r
plot(s10, type = "icons", arr_type = "shuffled")   # plot a shuffled icon array 
```

<!-- ![](inst/pix/README-ex2_icons-1.png) -->
#### Area plot

``` r
plot(s10, type = "area", p_split = "v", p_lbl = "def")  # plot an area/mosaic plot (with probabilities) 
```

<!-- ![](inst/pix/README-ex2_area-1.png) -->
#### Table plot

``` r
plot(s10, type = "tab", p_split = "h", p_lbl = "def")  # plot a 2x2 table (with probabilities)
```

<!-- ![](inst/pix/README-ex2_tab-1.png) -->
#### Curves

The following curves show the values of several conditional probabilities as a function of prevalence:

``` r
plot(s10, type = "curve", what = "all", uc = .05)  # plot all curves (by prev):
```

<!-- ![Probability curves (with uncertainty)](inst/pix/README-ex2_curve-1.png) -->
Adding the argument `what = "all"` also shows the proportion of positive decisions (`ppod`) and the decision's overall accuracy (`accu`) as a function of the prevalence (`prev`). Would you have predicted their shape without seeing this graph?

#### Planes

The following surface shows the negative predictive value (NPV) as a function of sensitivity and specificity (for a given prevalence):

``` r
plot(s10, type = "plane", what = "NPV")  # plot plane (as a function of sens x spec):
```

<!-- ![Probability plane (NPV)](inst/pix/README-ex2_plane-1.png) -->
Hopefully, this brief overview managed to whet your appetite for visual exploration. If so, call `riskyr.guide()` for viewing the package vignettes and obtaining additional information.

About
-----

<!-- uni.kn logo and link to SPDS: -->
<!-- ![](./inst/pix/uniKn_logo.png) -->
<a href="https://www.spds.uni-konstanz.de/"> <img src = "./inst/pix/uniKn_logo.png" alt = "spds.uni.kn" align = "right" width = "300" style = "width: 300px; float: right; border:20;"/> <!-- <img src = "./inst/pix/uniKn_logo_s.png" alt = "spds.uni.kn" style = "float: right; border:20;"/> --> </a>

`riskyr` originated out of a series of lectures and workshops on risk literacy.
The current version (`riskyr` 0.2.0, as of Dec. 20, 2018) is still under development. Its primary designers are [Hansjörg Neth](https://www.spds.uni-konstanz.de/hans-neth), [Felix Gaisbauer](https://www.spds.uni-konstanz.de/felix-gaisbauer), [Nico Gradwohl](https://www.spds.uni-konstanz.de/nico-gradwohl), and [Wolfgang Gaissmaier](https://www.spds.uni-konstanz.de/prof-dr-wolfgang-gaissmaier), who are researchers at the department of [Social Psychology and Decision Sciences](https://www.spds.uni-konstanz.de) at the [University of Konstanz](https://www.uni-konstanz.de/en/), Germany.

The `riskyr` package is open source software written in [R](https://www.r-project.org/) and released under the [GPL 2](https://tldrlegal.com/license/gnu-general-public-license-v2) | [GPL 3](https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3)) licenses.

### Contact

We appreciate your feedback, comments, or questions.

-   Please report any `riskyr`-related issues at <https://github.com/hneth/riskyr/issues>.

-   Email us at <contact.riskyr@gmail.com> if you want to modify or share this software.

-   See <http://riskyr.org> for an interactive online version.

### Reference

<!-- riskyr logo: -->
<a href = "https://github.com/hneth/riskyr"> <img alt = "riskyr logo" title = "riskyr" src = "./inst/pix/riskyr_cube.png" width = "180px" align = "right" style = "float:right; border:20; width:180px;"/> </a> <!-- <img src = "./inst/pix/riskyr_cube_s.png" alt = "riskyr" align = "right" style = "float: right; border:20;"/> --> <!-- ![riskyr](./inst/pix/riskyr_cube_s.png) --> <!-- knitr::include_graphics("./inst/pix/riskyr_cube_s.png") -->

To cite `riskyr` in derivations and publications please use:

-   Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2018).
    `riskyr`: A toolbox for rendering risk literacy more transparent.
    Social Psychology and Decision Sciences, University of Konstanz, Germany.
    Computer software (R package version 0.2.0, Dec. 20, 2018).
    Retrieved from <https://CRAN.R-project.org/package=riskyr>.

A BibTeX entry for LaTeX users is:

    @Manual{riskyr,
      title = {riskyr: A toolbox for rendering risk literacy more transparent},
      author = {Hansjörg Neth and Felix Gaisbauer and Nico Gradwohl and Wolfgang Gaissmaier},
      year = {2018},
      organization = {Social Psychology and Decision Sciences, University of Konstanz},
      address = {Konstanz, Germany},
      note = {R package (version 0.2.0, Dec. 20, 2018)},
      url = {https://CRAN.R-project.org/package=riskyr},
      }    

Calling `citation("riskyr")` in the package also displays this information.

### References

-   Arkes, H. R., & Gaissmaier, W. (2012). Psychological research and the prostate-cancer screening controversy. *Psychological Science*, *23*, 547–553.

-   Garcia-Retamero, R., & Cokely, E. T. (2017). Designing visual aids that promote risk literacy: A systematic review of health research and evidence-based design heuristics. *Human Factors*, *59*, 582–627.

-   Gigerenzer, G. (2002). *Reckoning with risk: Learning to live with uncertainty*. London, UK: Penguin.

-   Gigerenzer, G. (2014). *Risk savvy: How to make good decisions*. New York, NY: Penguin.

-   Gigerenzer, G., & Gaissmaier, W. (2011). Heuristic decision making. *Annual Review of Psychology*, *62*, 451–482. [Available online](https://www.annualreviews.org/doi/10.1146/annurev-psych-120709-145346)

-   Gigerenzer, G., Gaissmaier, W., Kurz-Milcke, E., Schwartz, L., & Woloshin, S. (2007). Helping doctors and patients make sense of health statistics. *Psychological Science in the Public Interest*, *8*, 53–96. [Available online](https://journals.sagepub.com/doi/10.1111/j.1539-6053.2008.00033.x)

-   Gigerenzer, G., & Hoffrage, U. (1995). How to improve Bayesian reasoning without instruction: Frequency formats. *Psychological Review*, *102*, 684–704.

-   Hoffrage, U., Gigerenzer, G., Krauss, S., & Martignon, L. (2002). Representation facilitates reasoning: What natural frequencies are and what they are not. *Cognition*, *84*, 343–352.

-   Hoffrage, U., Krauss, S., Martignon, L., & Gigerenzer, G. (2015). Natural frequencies improve Bayesian reasoning in simple and complex inference tasks. *Frontiers in Psychology*, *6*, 1473.

-   Hoffrage, U., Lindsey, S., Hertwig, R., & Gigerenzer, G. (2000). Communicating statistical information. *Science*, *290*, 2261–2262.

-   Khan, A., Breslav, S., Glueck, M., & Hornbæk, K. (2015). Benefits of visualization in the mammography problem. *International Journal of Human-Computer Studies*, *83*, 94–113.

-   Kurzenhäuser, S., & Hoffrage, U. (2002). Teaching Bayesian reasoning: An evaluation of a classroom tutorial for medical students. *Medical Teacher*, *24*, 516–521.

-   Kurz-Milcke, E., Gigerenzer, G., & Martignon, L. (2008). Transparency in risk communication. *Annals of the New York Academy of Sciences*, *1128*, 18–28.

-   Micallef, L., Dragicevic, P., & Fekete, J.-D. (2012). Assessing the effect of visualizations on Bayesian reasoning through crowd-sourcing. *IEEE Transactions on Visualization and Computer Graphics*, *18*, 2536–2545.

-   Neth, H., & Gigerenzer, G. (2015). Heuristics: Tools for an uncertain world. In R. Scott & S. Kosslyn (Eds.), *Emerging trends in the social and behavioral sciences*. New York, NY: Wiley Online Library. [Available online](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.726.1656&rep=rep1&type=pdf)

-   Sedlmeier, P., & Gigerenzer, G. (2001). Teaching Bayesian reasoning in less than two hours. *Journal of Experimental Psychology: General*, *130*, 380–400.

-   Wassner, C., Martignon, L., & Biehler, R. (2004). Bayesianisches Denken in der Schule. *Unterrichtswissenschaft*, *32*, 58–96.

<!-- eof -->

[1] Simon, H.A. (1996). *The Sciences of the Artificial* (3rd ed.). The MIT Press, Cambridge, MA. (p. 132).

[2] To clarify our notion of "risk" in this context, we need to distinguish it from its everyday usage as anything implying a chance of danger or harm.
In basic research on judgment and decision making and the more applied fields of risk perception and risk communication, the term *risk* typically refers to decisions or events for which the options and their consequences are known and probabilities for all possible outcomes can be provided.
For our present purposes, the notion of risk-related information refers to any scenario in which some events of interest are determined by probabilities. While it is important that quantitative (estimates of) probabilities are provided, their origin, reliability and validity is not questioned here. Thus, the probabilities provided can be based on clinical intuition, on recordings of extensive experience, or on statistical simulation models (e.g., repeatedly casting dice and counting the frequencies of outcomes).
This notion of *risk* is typically contrasted with the much wider notion of *uncertainty* in which options or probabilities are unknown or cannot be quantified. (See Gigerenzer and Gaissmaier, 2011, or Neth and Gigerenzer, 2015, on this conceptual distinction and corresponding decision strategies.)

[3] See Gigerenzer (2002, 2014), Gigerenzer and Hoffrage, U. (1995), Gigerenzer et al. (2007), and Hoffrage et al. (2015) for scientific background information and similar problems. See Sedlmeier and Gigerenzer (2001) and Kurzenhäuser and Hoffrage (2002) for related training programs (with remarkable results), and Micallef et al. (2012) and Khan et al. (2015) for (rather sceptical and somewhat sobering) studies on the potential benefits of static representations for solving Bayesian problems.

[4] See Gigerenzer and Hoffrage (1995) and Hoffrage et al. (2000, 2002) on the concept of *natural frequencies*.


<!-- README.md is generated from README.Rmd. Please only edit the latter (.Rmd) file! -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/riskyr)](https://CRAN.R-project.org/package=riskyr) [![Build Status](https://travis-ci.org/hneth/riskyr.svg?branch=master)](https://travis-ci.org/hneth/riskyr) [![Downloads](http://cranlogs.r-pkg.org/badges/riskyr?color=brightgreen)](http://www.r-pkg.org/pkg/riskyr) [![Rdoc](http://www.rdocumentation.org/badges/version/riskyr)](http://www.rdocumentation.org/packages/riskyr)

riskyr
======

<!-- riskyr logo: -->
<a href = "https://github.com/hneth/riskyr"> <img alt = "riskyr logo" title = "riskyr" src = "./inst/pix/riskyr_cube.png" width = "180px" align = "right" style = "float:right; border:20; width:180px;"/> </a> <!-- <img src = "./inst/pix/riskyr_cube_s.png" alt = "riskyr" align = "right" style = "float: right; border:20;"/> --> <!-- ![riskyr](./inst/pix/riskyr_cube_s.png) --> <!-- knitr::include_graphics("./inst/pix/riskyr_cube_s.png") -->

**A toolbox for rendering risk literacy more transparent**

Risk-related information — like the prevalence of conditions and the sensitivity and specificity of diagnostic tests or treatment decisions — can be expressed in terms of probabilities or frequencies. By providing a toolbox of methods and metrics, `riskyr` computes, translates, and displays risk-related information in a variety of ways. Offering multiple complementary perspectives on the interplay between key parameters renders teaching and training of risk literacy more transparent.

Motivation
----------

> Solving a problem simply means representing it <br> so as to make the solution transparent. (H.A. Simon)[1]

<!-- Risk perception as representational effects: -->
The goals of `riskyr` are less of a *computational* and more of a *representational* nature, by addressing the expression in and translation between different formats of information. Whereas people find it difficult to understand and compute information expressed in terms of probabilities, the same information is easier to understand and compute when expressed in terms of frequencies. But rather than just expressing risk-related information in terms of frequencies, `riskyr` allows translating between formats and illustrates the relationships between different representations in a variety of ways. We hope that interacting with and switching between different representations will boost the transparency and human understanding of risk-related information.[2]

<!-- Defining "risk" (in footnote): -->
<!-- On "riskyr": -->
Basic assumptions and goals driving the current development of `riskyr` include:

1.  Effective training in risk literacy requires transparent representations, smart strategies, and simple tools.

2.  More specifically, we aim for a set of (computational and representational) tools that facilitate various calculations, translations between formats, and a range of alternative views on the interplay between probabilities and frequencies.

3.  We hypothesize that a variety of visualizations, which illustrate how parameters and metrics interact and influence each other, will facilitate active and explorative learning. It is particularly helpful to view the same or similar relationships from alternative perspectives and to observe the change of one parameter as a function of others.

Based on these assumptions and goals, we provide a range of computational and representational tools. Importantly, the objects and functions in the `riskyr` toolbox are not isolated, but complement, explain, and support each other. All functions and visualizations can also be used separately and explored interactively, providing immediate feedback on the effect of changes in parameter values. By providing a variety of customization options, users can explore and design representations of risk-related information that suit their personal needs and goals.

Installation
------------

The current release of `riskyr` is available from [CRAN](https://CRAN.R-project.org/) at <https://CRAN.R-project.org/package=riskyr>:

``` r
install.packages("riskyr")  # install riskyr from CRAN client
library("riskyr")           # load to use the package
```

The most recent development version can be installed from its [GitHub](https://github.com) repository at <https://github.com/hneth/riskyr/>:

``` r
# install.packages("devtools")
devtools::install_github("hneth/riskyr")
```

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

Let us define a new `riskyr` scenario (called `hustosis`) with the information provided by our problem:

``` r
hustosis <- riskyr(scen_lbl = "Example", 
                   cond_lbl = "Hustosis",
                   dec_lbl = "Screening test",
                   popu_lbl = "Sample", 
                   N = 1000,                                 # population size
                   prev = .04, sens = .80, spec = (1 - .05)  # 3 key probabilities
                   )
```

By providing the argument `N = 1000` we defined the scenario for a target population of 1000 people. If we left this parameter unspecified (or `NA`), `riskyr` will automatically pick a suitable value of `N` to compute frequency information.

#### Summary

To obtain a quick overview of key parameter values, we ask for the `summary` of our `hustosis` scenario:

``` r
summary(hustosis)  # summarizes key parameter values: 
#> Scenario:  Example 
#> 
#> Condition:  Hustosis 
#> Decision:  Screening test 
#> Population:  Sample 
#> N =  1000 
#> Source:  Source information 
#> 
#> Probabilities:
#> 
#>  Essential probabilities:
#> prev sens mirt spec fart 
#> 0.04 0.80 0.20 0.95 0.05 
#> 
#>  Other probabilities:
#>  ppod   PPV   NPV   FDR   FOR   acc 
#> 0.080 0.400 0.991 0.600 0.009 0.944 
#> 
#> Frequencies:
#> 
#>  by conditions:
#>  cond.true cond.false 
#>         40        960 
#> 
#>  by decision:
#> dec.pos dec.neg 
#>      80     920 
#> 
#>  by correspondence (of decision to condition):
#> dec.cor dec.err 
#>     944      56 
#> 
#>  4 essential (SDT) frequencies:
#>  hi  mi  fa  cr 
#>  32   8  48 912 
#> 
#> Accuracy:
#> 
#>  acc:
#> 0.944
```

The summary distinguishes between probabilities, frequencies, and accuracy information. In `Probabilities` we find the answer to both of our questions that take into account all the information provided above:

-   The conditional probability that Mr. Smith actually suffers from hustosis given his positive test result is 40% (as `PPV = 0.400`).

-   The conditional probability that Ms. Smith is actually free of hustosis given her negative test result is 99.1% (as `NPV = 0.991`).

In case you are surprised by these answers, you are a good candidate for additional instruction in risk literacy. One of the strengths of `riskyr` is to analyze and view the scenario from a variety of different perspectives. To get you started immediately, we provide a few introductory examples that illustrate different types of visualizations.

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

#### Tree diagram

A tree diagram is obtained by plotting a scenario with the `type = "tree"` option:

``` r
plot(hustosis, type = "tree", by = "dc")  # plot a tree diagram (by decision):
```

![](inst/pix/README-ex1_tree-1.png)

This particular tree splits the population of `N = 1000` individuals into two subgroups *by decision* (`by = "dc"`) and contains the answer to the second (frequency) version of our questions:

-   The proportion of individuals with a positive test result who actually suffer from hustosis is the frequency of "true positive" cases (shown in darker green) divided by "decision positive" cases (shown in purple): `32/80 = .400` (corresponding to our value of `PPV` above).
-   The proportion of individuals with a negative test result who are actually free from hustosis is the frequency of "true negative" cases (shown in lighter green) divided by "decision negative" cases (shown in blue): `912/920 = .991` (corresponding to our value of `NPV` above, except for minimal differences due to rounding).

Of course, the frequencies of these ratios were already contained in the `hustosis` summary above. But the representation in the form of a tree diagram makes it easier to understand the decomposition of the population into subgroups and to see which frequencies are required to answer a particular question.

#### Icon array

An icon array shows the classification result for each of `N = 1000` individuals in our population:

``` r
plot(hustosis, type = "icons")   # plot an icon array: 
```

![](inst/pix/README-ex1_icons-1.png)

While this particular icon array is highly regular (as both the icons and classification types are ordered), `riskyr` provides many different versions of this type of graph. This allows viewing the probability of diagnostic outcomes as either frequency, area, or density.

#### Mosaic plot

The mosaic plot offers a way of expressing classification results as the relationship between areas. Here, the entire population is represented as a square and the probability of its subgroups as the size of rectangles:

``` r
plot(hustosis, type = "mosaic")  # plot a mosaic plot: 
```

![](inst/pix/README-ex1_mosaic-1.png)

#### Curves

By adopting a functional perspective, we can ask how the values of some parameters (e.g., `PPV` or `NPV`) change as a function of another (e.g., `prev`):

``` r
plot(hustosis, type = "curve")   # plot probability curves (by prevalence):
```

![](inst/pix/README-ex1_curve-1.png)

#### Planes

When parameter values systematically depend on two other parameters, we can plot this as a plane in a 3D cube. The following graph plots the `PPV` as a function of the sensitivity (`sens`) and specificity (`spec`) of our test for a given prevalence (`prev`):

``` r
plot(hustosis, type = "plane", theta = -60)  # plot probability plane (as a function of sens x spec):
```

![](inst/pix/README-ex1_plane-1.png)

The L-shape of this plane reveals a real problem with our current test: Given a prevalence of 4% for hustosis in our target population, the `PPV` remains very low for the majority of the possible range of sensitivity and specificity values. To achieve a high `PPV`, the key requirement for our test is an extremely high specificity. Although our current specificity value of 95% (`spec = .95`) may sound pretty good, it is still not high enough to yield a `PPV` beyond 40%.

#### Plots currently under development

The current development version of `riskyr` (version 0.1.0.930+) replaces some of the above plots with a set of more powerful and more integrated functions. To use them and preview their effects, install the development version and explore the following commands:

``` r
## Install current development version: 
# install.packages("devtools")
devtools::install_github("hneth/riskyr")

## Preview latest functions (riskyr > v0.0.1.930):
plot(hustosis, type = "tab",   by = "cddc")
plot(hustosis, type = "prism", by = "cddc")  
plot(hustosis, type = "tree",  by = "ac") 
plot(hustosis, type = "area",  by = "cddc")
plot(hustosis, type = "bar",   dir = 2)
```

<!-- 2: Loading and using pre-defined scenarios -->
### Using existing scenarios

As defining your own scenarios can be cumbersome and the literature is full of existing problems (that study so-called Bayesian reasoning), `riskyr` provides a set of — currently 25 — pre-defined scenarios (stored in a list `scenarios`). Here, we provide an example that shows how you can select and explore them.

#### Selecting a scenario

Let us assume you want to learn more about the controversy surrounding screening prodecures of prostate-cancer (known as PSA screening). Scenario 21 in our collection of `scenarios` is from an article on this topic (Arkes & Gaissmaier, 2012). To select a particular scenario, simply assign it to an R object. For instance, we can assign Scenario 21 to `s21`:

``` r
s21 <- scenarios$n21  # assign pre-defined Scenario 21 to s21.
```

#### Summary

The following commands provide a quick overview of the scenario content in text form:

``` r
s21$scen_lbl  # shows descriptive label:
#> [1] "PSA test 1 (high prev)"
s21$cond_lbl  # shows current condition:
#> [1] "prostate cancer"
s21$dec_lbl   # shows current decision:
#> [1] "PSA test"
s21$popu_lbl  # shows current population:
#> [1] "1000 patients with symptoms diagnostic of prostate cancer taking a PSA test."
s21$scen.apa  # shows current source: 
#> NULL

summary(s21)  # shows key scenario information:
#> Scenario:  PSA test 1 (high prev) 
#> 
#> Condition:  prostate cancer 
#> Decision:  PSA test 
#> Population:  1000 patients with symptoms diagnostic of prostate cancer taking a PSA test. 
#> N =  1000 
#> Source:  Arkes & Gaissmaier (2012), p. 550 
#> 
#> Probabilities:
#> 
#>  Essential probabilities:
#> prev sens mirt spec fart 
#> 0.50 0.21 0.79 0.94 0.06 
#> 
#>  Other probabilities:
#>  ppod   PPV   NPV   FDR   FOR   acc 
#> 0.135 0.778 0.543 0.222 0.457 0.575 
#> 
#> Frequencies:
#> 
#>  by conditions:
#>  cond.true cond.false 
#>        500        500 
#> 
#>  by decision:
#> dec.pos dec.neg 
#>     135     865 
#> 
#>  by correspondence (of decision to condition):
#> dec.cor dec.err 
#>     575     425 
#> 
#>  4 essential (SDT) frequencies:
#>  hi  mi  fa  cr 
#> 105 395  30 470 
#> 
#> Accuracy:
#> 
#>  acc:
#> 0.575
```

Generating the following plots will allow you a quick visual exploration of the scenario. (We only show selected plots here, and trust that you can try out the others for yourself.)

#### Network diagram

A network diagram is a generalization of a tree diagram that simultaneously provides two perspectives on a population (see Wassner et al., 2004). `riskyr` provides several variants of network diagrams. In the following version, all frequencies are represented as colored squares and the relative sizes of their areas represent the number of people in the corresponding subgroups:

``` r
plot(s21) # plots a network diagram (by default):

## Using newer functions:
# s21$popu_lbl <- "Patients with symptoms taking PSA test"  # shorter popu_lbl
# plot(s21, type = "prism")
# plot(s21, type = "area")
# plot(s21, type = "tab")
```

![](inst/pix/README-ex2_fnet-1.png)

#### Icon array

``` r
plot(s21, type = "icons")   # plot an icon array: 
```

#### Mosaic plot

``` r
plot(s21, type = "mosaic")   # plot a mosaic plot: 
```

#### Curves

The following curves show values of conditional probabilities as a function of prevalence:

``` r
plot(s21, type = "curve", what = "all")  # plot all curves (as a function of prevalence):
```

![](inst/pix/README-ex2_curve-1.png)

Adding the argument `what = "all"` also shows the proportion of positive decisions (`ppod`) and the decision's overall accuracy (`accu`) as a function of the prevalence (`prev`). Would you have predicted their shape without seeing this graph?

#### Planes

The following surface shows the negative predictive value (NPV) as a function of sensitivity and specificity (for a given prevalence):

``` r
plot(s21, type = "plane", what = "NPV")  # plot plane (as a function of sens x spec):
```

Hopefully, this brief overview managed to whet your appetite for visual exploration. If so, call `riskyr.guide()` for viewing the package vignettes and obtaining additional information.

About
-----

<!-- uni.kn logo and link to SPDS: -->
<!-- ![](./inst/pix/uniKn_logo.png) -->
<a href="https://www.spds.uni-konstanz.de/"> <img src = "./inst/pix/uniKn_logo.png" alt = "spds.uni.kn" align = "right" width = "300" style = "width: 300px; float: right; border:20;"/> <!-- <img src = "./inst/pix/uniKn_logo_s.png" alt = "spds.uni.kn" style = "float: right; border:20;"/> --> </a>

`riskyr` originated out of a series of lectures and workshops on risk literacy in spring/summer 2017. The current version (`riskyr` 0.1.0, as of Feb. 19, 2018) is still under development. Its primary developers and designers are [Hansjörg Neth](https://www.spds.uni-konstanz.de/hans-neth), [Felix Gaisbauer](https://www.spds.uni-konstanz.de/felix-gaisbauer), and [Nico Gradwohl](https://www.spds.uni-konstanz.de/nico-gradwohl), who are researchers at the department of [Social Psychology and Decision Sciences](https://www.spds.uni-konstanz.de) at the [University of Konstanz](https://www.uni-konstanz.de/en/), Germany.

The `riskyr` package is open source software written in [R](https://www.r-project.org/) and released under the [GPL 2](https://tldrlegal.com/license/gnu-general-public-license-v2) | [GPL 3](https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3)) licenses.

Please email at <contact.riskyr@gmail.com> in case you want to use, adapt, or share this software.

### Contact

We appreciate your feedback, comments, or questions.

-   Please report any `riskyr`-related issues at <https://github.com/hneth/riskyr/issues>.

-   For general inquiries, please email us at <contact.riskyr@gmail.com>.

### Reference

<!-- riskyr logo: -->
<a href = "https://github.com/hneth/riskyr"> <img alt = "riskyr logo" title = "riskyr" src = "./inst/pix/riskyr_cube.png" width = "180px" align = "right" style = "float:right; border:20; width:180px;"/> </a> <!-- <img src = "./inst/pix/riskyr_cube_s.png" alt = "riskyr" align = "right" style = "float: right; border:20;"/> --> <!-- ![riskyr](./inst/pix/riskyr_cube_s.png) --> <!-- knitr::include_graphics("./inst/pix/riskyr_cube_s.png") -->

To cite `riskyr` in derivations and publications please use:

-   Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2018).
    `riskyr`: A toolbox for rendering risk literacy more transparent.
    Social Psychology and Decision Sciences, University of Konstanz, Germany.
    Computer software (R package version 0.1.0, Feb. 19, 2018).
    Retrieved from <https://CRAN.R-project.org/package=riskyr>.

A BibTeX entry for LaTeX users is:

    @Manual{riskyr,
      title = {riskyr: A toolbox for rendering risk literacy more transparent},
      author = {Hansjörg Neth and Felix Gaisbauer and Nico Gradwohl and Wolfgang Gaissmaier},
      year = {2018},
      organization = {Social Psychology and Decision Sciences, University of Konstanz},
      address = {Konstanz, Germany},
      note = {R package (version 0.1.0, Feb. 19, 2018)},
      url = {https://CRAN.R-project.org/package=riskyr},
      }    

Calling `citation("riskyr")` in the package also displays this information.

### References

-   Arkes, H. R., & Gaissmaier, W. (2012). Psychological research and the prostate-cancer screening controversy. *Psychological Science*, *23*, 547–553.

-   Gigerenzer, G. (2002). *Reckoning with risk: Learning to live with uncertainty*. London, UK: Penguin.

-   Gigerenzer, G. (2014). *Risk savvy: How to make good decisions*. New York, NY: Penguin.

-   Gigerenzer, G., & Gaissmaier, W. (2011). Heuristic decision making. *Annual Review of Psychology*, *62*, 451–482.

-   Gigerenzer, G., Gaissmaier, W., Kurz-Milcke, E., Schwartz, L., & Woloshin, S. (2007). Helping doctors and patients make sense of health statistics. *Psychological Science in the Public Interest*, *8*, 53–96.

-   Gigerenzer, G., & Hoffrage, U. (1995). How to improve Bayesian reasoning without instruction: Frequency formats. *Psychological Review*, *102*, 684–704.

-   Hoffrage, U., Gigerenzer, G., Krauss, S., & Martignon, L. (2002). Representation facilitates reasoning: What natural frequencies are and what they are not. *Cognition*, *84*, 343–352.

-   Hoffrage, U., Krauss, S., Martignon, L., & Gigerenzer, G. (2015). Natural frequencies improve Bayesian reasoning in simple and complex inference tasks. *Frontiers in Psychology*, *6*, 1473.

-   Hoffrage, U., Lindsey, S., Hertwig, R., & Gigerenzer, G. (2000). Communicating statistical information. *Science*, *290*, 2261–2262.

-   Khan, A., Breslav, S., Glueck, M., & Hornbæk, K. (2015). Benefits of visualization in the mammography problem. *International Journal of Human-Computer Studies*, *83*, 94–113.

-   Kurzenhäuser, S., & Hoffrage, U. (2002). Teaching Bayesian reasoning: An evaluation of a classroom tutorial for medical students. *Medical Teacher*, *24*, 516–521.

-   Kurz-Milcke, E., Gigerenzer, G., & Martignon, L. (2008). Transparency in risk communication. *Annals of the New York Academy of Sciences*, *1128*, 18–28.

-   Micallef, L., Dragicevic, P., & Fekete, J.-D. (2012). Assessing the effect of visualizations on Bayesian reasoning through crowd-sourcing. *IEEE Transactions on Visualization and Computer Graphics*, *18*, 2536–2545.

-   Neth, H., & Gigerenzer, G. (2015). Heuristics: Tools for an uncertain world. In R. Scott & S. Kosslyn (Eds.), *Emerging trends in the social and behavioral sciences*. New York, NY: Wiley Online Library.

-   Sedlmeier, P., & Gigerenzer, G. (2001). Teaching Bayesian reasoning in less than two hours. *Journal of Experimental Psychology: General*, *130*, 380–400.

-   Wassner, C., Martignon, L., & Biehler, R. (2004). Bayesianisches Denken in der Schule. *Unterrichtswissenschaft*, *32*, 58–96.

<!-- eof -->

[1] Simon, H.A. (1996). *The Sciences of the Artificial* (3rd ed.). The MIT Press, Cambridge, MA. (p. 132).

[2] It is important to clarify our notion of "risk" in this context: In basic research on judgment and decision making and the more applied fields of risk perception and risk communication, the term *risk* typically refers to decisions or events for which the options and their consequences are known and probabilities for all possible outcomes can be provided. For our present purposes, the notion of risk-related information refers to any scenario in which some events of interest are determined by probabilities. While it is important that quantitative (estimates of) probabilities are provided, their origin, reliability and validity is not questioned here. Thus, the probabilities provided can be based on clinical intuition, on recordings of extensive experience, or on statistical simulation models (e.g., repeatedly casting dice and counting the frequencies of outcomes). This notion of *risk* is typically contrasted with the much wider notion of *uncertainty* in which options or probabilities are unknown or cannot be quantified. (See Gigerenzer and Gaissmaier, 2011, or Neth and Gigerenzer, 2015, on this conceptual distinction and corresponding decision strategies.)

[3] See Gigerenzer (2002, 2014), Gigerenzer and Hoffrage, U. (1995), Gigerenzer et al. (2007), and Hoffrage et al. (2015) for scientific background information and similar problems. See Sedlmeier and Gigerenzer (2001) and Kurzenhäuser and Hoffrage (2002) for related training programs (with remarkable results), and Micallef et al. (2012) and Khan et al. (2015) for (rather sceptical and somewhat sobering) studies on the potential benefits of static representations for solving Bayesian problems.

[4] See Gigerenzer and Hoffrage (1995) and Hoffrage et al. (2000, 2002) on the concept of *natural frequencies*.

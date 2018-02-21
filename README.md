
[![Downloads](http://cranlogs.r-pkg.org/badges/FFTrees?color=brightgreen)](http://www.r-pkg.org/pkg/FFTrees)

<!-- README.md is generated from README.Rmd. Please only edit the latter (.Rmd) file! -->
`riskyr`
========

**A toolbox for rendering risk literacy more transparent.**

Starting with a condition (e.g., a disease), a corresponding decision (e.g., a clinical judgment or diagnostic test), and basic probabilities (e.g., the condition's prevalence `prev`, and the decision's sensitivity `sens` and specificity `spec`) we provide a range of functions and metrics to compute, translate, and represent risk-related information (e.g., as probabilities or frequencies for a population of `N` individuals). By offering a variety of perspectives on the interplay between key parameters, `riskyr` renders teaching and training of risk literacy more transparent.

Motivation
----------

> Solving a problem simply means representing it <br> so as to make the solution transparent. (H.A. Simon)[1]

The issues addressed by `riskyr` are less of a *computational* and more of a *representational* nature (i.e., concerning the expression in and translation between different formats of information). Whereas people tend to find it difficult to understand and compute information expressed in terms of *probabilities*, the same information is often easy to understand and compute when expressed in terms of *frequencies*. But rather than just expressing probabilistic information in terms of frequencies, `riskyr` allows translating between formats and illustrates their relationships in a variety of transparent and interactive ways.

Basic assumptions and goals driving the current development of `riskyr` include the following:

<!-- riskyr logo: -->
<a href = "https://github.com/hneth/riskyr"> <!-- <img src = "./inst/pix/riskyr_cube.png" alt = "riskyr" style = "width: 180px; float: right; border:20;"/> --> <img src = "./inst/pix/riskyr_cube_s.png" alt = "riskyr" style = "float: right; border:20;"/> </a> <!-- ![riskyr](./inst/pix/riskyr_cube.png) --> <!-- knitr::include_graphics("./inst/pix/riskyr_cube.png") -->

1.  Effective training in risk literacy requires simple tools and transparent representations.

2.  More specifically, it would be desirable to have a set of (computational and representational) tools that allow various calculations, translations (between formats), and a range of alternative views on the interplay between probabilities and frequencies.

3.  Seeing a variety of visualizations that illustrate how parameters and metrics interact and influence each other facilitates active and explorative learning. It is particularly helpful to view the same or similar relationships from alternative representations or to inspect the change of one parameter as a function of changes in other parameters.

To deliver on these assumptions and goals, we provide a range of computational and representational tools. Importantly, the objects and functions in the `riskyr` toolbox are not isolated, but complement, explain, and support each other. All functions and visualizations can be used separately and explored interactively, providing immediate feedback on the effect of changes in parameter values. By providing a variety of customization options, users can explore and design representations of risk-related information that suit their personal goals and needs.

Installation
------------

You can install the latest development version of `riskyr` from its [GitHub](https://github.com) repository at <https://github.com/hneth/riskyr>:

``` r
# install.packages("devtools")
devtools::install_github("hneth/riskyr")
```

Quick Start Guide
-----------------

### Defining a scenario

`riskyr` is designed to address problems like the following:[2]

> **Screening for hustosis**
>
> A new screening device for detecting the clinical condition of *hustosis* is developed. Its current version is very good, but not yet perfect. It has the following properties:
> 1. About 4% of the general population suffer from *hustosis*.
> 2. If someone suffers from hustosis, there is a chance of 80% that he or she will test positively for the condition.
> 3. If someone is free from hustosis, there is a chance of 4% that he or she will still test positively for the condition.
>
> Mr. and Ms. Smith have both been screened with this device:
> - Mr. Smith tested positively (i.e., received a diagnosis of hustosis).
> - Ms. Smith tested negatively (i.e., was judged to be free of hustosis).
>
> Please answer the following questions:
> - What is the probability that Mr. Smith actually suffers from hustosis?
> - What is the probability that Ms. Smith is actually free of hustosis?

#### Probabilities provided

The first challenge in solving such problems is in understanding the information provided. The problem description provides three essential probabilities:

1.  The condition's *prevalence* (in the general population) of 4%: `prev = .04`.
2.  The device's or diagnostic decision's *sensitivity* of 80%: `sens = .80`.
3.  The device's or diagnostic decision's *false alarm rate* of 4%, implying a *specificity* of (100% − 4%) = 96%: `spec = .04`.

#### Understanding the questions asked

The second challenge here lies in understanding the questions that are being asked — and in realizing that their answers are *not* simply the decision's sensitivity or specificity values. Instead, we are asked to provide two *conditional* probabilities:

-   The conditional probability of suffering from the condition given a positive test result, <br>aka. the *positive predictive value* `PPV`.
-   The conditional probability of being free of the condition given a negative test result, <br>aka. the *negative predictive value* `NPV`.

#### Translating into frequencies

One of the best tricks in risk literacy education is to translate probabilistic information into frequencies.[3] To do this, we imagine a representative sample of `N = 1000` individuals. Rather than asking about the probabilities for Mr. and Ms. Smith, we could re-frame the questions as:

> Assuming a representative sample of 1000 individuals:
> - How many individuals with a positive test result actually suffer from hustosis?
> - How many individuals with a negative test result are actually free of hustosis?

#### Using `riskyr`

Here is how `riskyr` allows you to view and solve such problems:

``` r
library(riskyr)  # load riskyr

## (1) Define your own scenario: ----------
hustosis <- riskyr(prev = .04, sens = .80, spec = (1 - .05), 
                   N = 1000, popu.lbl = "representative sample"
                   scen.lbl = "Example", cond.lbl = "Screening for hustosis", 
                   dec.lbl = "screening")

## View parameters:
summary(hustosis)
summary(hustosis, summarize = "probs")

## View graphics: 
plot(hustosis, plottype = "ftree")
plot(hustosis, plottype = "icons")
# plot(hustosis, plottype = "mosaic")
plot(hustosis, plottype = "curve")
# plot(hustosis, plottype = "plane", what = "NPV")
```

### Using existing scenarios

As defining your own scenarios can be cumbersome and the literature is full of similar problems (of so-called Bayesian reasoning), `riskyr` provides a set of (currently 25) pre-defined scenarios (stored in a list `scenarios`). Here is how you can explore them:

``` r
## (2) Inspect an existing riskyr scenario: ---------- 

## (a) PSA screening with high prevalence: ----------

s21 <- scenarios$n21  # assign pre-defined Scenario_21 to s

## View parameters:
summary(s21) # shows all scenario information
summary(s21, summarize = "prob") # shows probabilities
summary(s21, summarize = "freq") # shows frequencies

## View graphics: 
plot(s21) # plots a network diagram (by default)
plot(s21, plottype = "icons")
plot(s21, plottype = "mosaic")
plot(s21, plottype = "curve", what = "all")
plot(s21, plottype = "plane", what = "PPV")
```

What is to see?

``` r
## (b) same with low prevalence: ----------

s22 <- scenarios$n22  # assign pre-defined Scenario_22 to s22

# ... 

## Contrast two versions: 
plot(s22, plottype = "plane", what = "PPV")
plot(s22, plottype = "plane", what = "PPV")
```

About
-----

<!-- uni.kn logo and link to SPDS: -->
<!-- ![](./inst/pix/uniKn_logo.png) -->
<a href="https://www.spds.uni-konstanz.de/"> <!--<img src = "./inst/pix/uniKn_logo.png" alt = "spds.uni.kn" style = "width: 300px; float: right; border:20;"/> --> <img src = "./inst/pix/uniKn_logo_s.png" alt = "spds.uni.kn" style = "float: right; border:20;"/> </a>

`riskyr` originated out of a series of lectures and workshops on risk literacy in spring/summer 2017. The current version (`riskyr` 0.0.0.924, as of Feb. 14, 2018) is still under development. Its primary designers and developers are [Hansjörg Neth](https://www.spds.uni-konstanz.de/hans-neth), [Felix Gaisbauer](https://www.spds.uni-konstanz.de/felix-gaisbauer), and [Nico Gradwohl](https://www.spds.uni-konstanz.de/nico-gradwohl), who are researchers at the department of [Social Psychology and Decision Sciences](https://www.spds.uni-konstanz.de) at the [University of Konstanz](https://www.uni-konstanz.de/en/), Germany.

The `riskyr` package is open source software written in [R](https://www.r-project.org/) and released under the [GPL 2](https://tldrlegal.com/license/gnu-general-public-license-v2) | [GPL 3](https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3)) licenses.

Please email at <contact.riskyr@gmail.com> in case you want to use, adapt, or share this software.

### Contact

We appreciate your feedback, comments, or questions.

-   Please report any `riskyr`-related issues at <https://github.com/hneth/riskyr/issues>.

-   For general inquiries, please email us at <contact.riskyr@gmail.com>.

### Reference

To cite `riskyr` in derivations and publications use:

-   Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2018).
    `riskyr`: A toolbox for rendering risk literacy more transparent.
    Social Psychology and Decision Sciences, University of Konstanz, Germany.
    Computer software (R package version 0.0.0.924, Feb. 14, 2018).
    Retrieved from <https://github.com/hneth/riskyr>.

A BibTeX entry for LaTeX users is:

    @Manual{,
      title = {riskyr: A toolbox for rendering risk literacy more transparent},
      author = {Hansjörg Neth and Felix Gaisbauer and Nico Gradwohl and Wolfgang Gaissmaier},
      year = {2018},
      organization = {Social Psychology and Decision Sciences, University of Konstanz},
      address = {Konstanz, Germany},
      note = {R package (version 0.0.0.924, Feb. 14, 2018)},
      url = {https://github.com/hneth/riskyr},
      }    

Calling `citation("riskyr")` in the package also displays this information.

### References

-   Gigerenzer, G. (2002). *Reckoning with risk: Learning to live with uncertainty*. London, UK: Penguin.

-   Gigerenzer, G. (2014). *Risk savvy: How to make good decisions*. New York, NY: Penguin.

-   Gigerenzer, G., Gaissmaier, W., Kurz-Milcke, E., Schwartz, L., & Woloshin, S. (2007). Helping doctors and patients make sense of health statistics. *Psychological Science in the Public Interest*, *8*, 53--96.

-   Gigerenzer, G., & Hoffrage, U. (1995). How to improve Bayesian reasoning without instruction: Frequency formats. *Psychological Review*, *102*, 684--704.

-   Hoffrage, U., Gigerenzer, G., Krauss, S., & Martignon, L. (2002). Representation facilitates reasoning: What natural frequencies are and what they are not. *Cognition*, *84*, 343--352.

-   Hoffrage, U., Krauss, S., Martignon, L., & Gigerenzer, G. (2015). Natural frequencies improve Bayesian reasoning in simple and complex inference tasks. *Frontiers in Psychology*, *6*, 1473.

-   Hoffrage, U., Lindsey, S., Hertwig, R., & Gigerenzer, G. (2000). Communicating statistical information. *Science*, *290*, 2261--2262.

-   Kurzenhäuser, S., & Hoffrage, U. (2002). Teaching Bayesian reasoning: An evaluation of a classroom tutorial for medical students. *Medical Teacher*, *24*, 516--521.

-   Kurz-Milcke, E., Gigerenzer, G., & Martignon, L. (2008). Transparency in risk communication. *Annals of the New York Academy of Sciences*, *1128*, 18--28.

-   Sedlmeier, P., & Gigerenzer, G. (2001). Teaching Bayesian reasoning in less than two hours. *Journal of Experimental Psychology: General*, *130*, 380--400.

<!-- eof -->

[1] Simon, H.A. (1996). *The Sciences of the Artificial* (3rd ed.). The MIT Press, Cambridge, MA. (p. 132).

[2] See Gigerenzer (2002, 2014), Gigerenzer and Hoffrage, U. (1995), Gigerenzer et al. (2007), and Hoffrage et al. (2015) for lots of similar problems. Also, Sedlmeier and Gigerenzer (2001) and Kurzenhäuser and Hoffrage (2002) report related training programs.

[3] See Gigerenzer and Hoffrage (1995) and Hoffrage et al. (2000, 2002) on the concept of *natural frequencies*.

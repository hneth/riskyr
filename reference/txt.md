# Basic text elements.

`txt` is initialized to a list of named elements to define basic
scenario titles and labels.

## Usage

``` r
txt
```

## Details

All textual elements that specify generic labels and titles of `riskyr`
scenarios are stored as named elements (of type character) in a list
`txt`. To change an element, assign a new character object to an
existing name.

The list `txt` is used throughout the `riskyr` package unless a scenario
defines scenario-specific text labels (when using the
[`riskyr`](https://hneth.github.io/riskyr/reference/riskyr.md)
function).

Note:

- Basic text information and some numeric parameters (see
  [`num`](https://hneth.github.io/riskyr/reference/num.md) and
  [`init_num`](https://hneth.github.io/riskyr/reference/init_num.md))
  are integral parts of a `riskyr` scenario.

- By contrast, basic *color* information (see
  [`pal`](https://hneth.github.io/riskyr/reference/pal.md) and
  [`init_pal`](https://hneth.github.io/riskyr/reference/init_pal.md)) is
  not an integral part, but independently defined.

- The names of *probabilities* (see
  [`prob`](https://hneth.github.io/riskyr/reference/prob.md)) are
  currently not an integral part of `txt` and `riskyr` scenarios (but
  defined in `prob_lbl_def` and `label_prob`).

`txt` currently contains the following text labels:

1.  `scen_lbl` The current scenario title (sometimes in Title Caps).

2.  `scen_txt` A longer text description of the current scenario (which
    may extend over several lines).

3.  `scen_src` The source information for the current scenario.

4.  `scen_apa` The source information in APA format.

5.  `scen_lng` The language of the current scenario (as character code).
    Options: `"en"`: English, `"de"`: German.

6.  `popu_lbl` A general name describing the current *population*.

7.  `N_lbl` A short label for the current population
    [`popu`](https://hneth.github.io/riskyr/reference/popu.md) or
    sample.

8.  `cond_lbl` A general name for the *condition* dimension, or the
    feature (e.g., some disease) currently considered.

9.  `cond_true_lbl` A short label for the *presence* of the current
    condition or
    [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md)
    cases (the condition's true state of being TRUE).

10. `cond_false_lbl` A short label for the *absence* of the current
    condition or
    [`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md)
    cases (the condition's true state of being FALSE).

11. `dec_lbl` A general name for the *decision* dimension, or the
    judgment (e.g., some diagnostic test) currently made.

12. `dec_pos_lbl` A short label for *positive* decisions or
    [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md)
    cases (e.g., predicting the presence of the condition).

13. `dec_neg_lbl` A short label for *negative* decisions or
    [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md)
    cases (e.g., predicting the absence of the condition).

14. `acc_lbl` A general name for the *accuracy* dimension, or the
    correspondence between the condition currently considered and the
    decision judgment currently made.

15. `dec_cor_lbl` A short label for *correct* and *accurate* decisions
    or [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md)
    cases (accurate predictions).

16. `dec_err_lbl` A short label for *incorrect* decisions or
    [`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md)
    cases (erroneous predictions).

17. `sdt_lbl` A general name for all 4 *cases/categories/cells* of the
    2x2 contingency table (e.g., condition x decision, using SDT).

18. `hi_lbl` A short label for *hits* or *true positives*
    [`hi`](https://hneth.github.io/riskyr/reference/hi.md)/TP cases
    (i.e., correct decisions of the presence of the condition, when the
    condition is actually present).

19. `mi_lbl` A short label for *misses* or *false negatives*
    [`mi`](https://hneth.github.io/riskyr/reference/mi.md)/FN cases
    (i.e., incorrect decisions of the absence of the condition when the
    condition is actually present).

20. `fa_lbl` A short label for *false alarms* or *false positives*
    [`fa`](https://hneth.github.io/riskyr/reference/fa.md)/FP cases
    (i.e., incorrect decisions of the presence of the condition when the
    condition is actually absent).

21. `cr_lbl` A short label for *correct rejections* or *true negatives*
    [`cr`](https://hneth.github.io/riskyr/reference/cr.md)/TN cases
    (i.e., a correct decision of the absence of the condition, when the
    condition is actually absent).

## See also

[`init_txt`](https://hneth.github.io/riskyr/reference/init_txt.md)
initializes text information;
[`riskyr`](https://hneth.github.io/riskyr/reference/riskyr.md)
initializes a `riskyr` scenario;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`pal`](https://hneth.github.io/riskyr/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/reference/init_pal.md)
initializes color information;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information.

Other lists containing current scenario information:
[`accu`](https://hneth.github.io/riskyr/reference/accu.md),
[`freq`](https://hneth.github.io/riskyr/reference/freq.md),
[`num`](https://hneth.github.io/riskyr/reference/num.md),
[`pal`](https://hneth.github.io/riskyr/reference/pal.md),
[`pal_bw`](https://hneth.github.io/riskyr/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/reference/pal_bwp.md),
[`pal_kn`](https://hneth.github.io/riskyr/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/reference/pal_mod.md),
[`pal_org`](https://hneth.github.io/riskyr/reference/pal_org.md),
[`pal_rgb`](https://hneth.github.io/riskyr/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/reference/pal_unikn.md),
[`pal_vir`](https://hneth.github.io/riskyr/reference/pal_vir.md),
[`prob`](https://hneth.github.io/riskyr/reference/prob.md),
[`txt_TF`](https://hneth.github.io/riskyr/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/reference/txt_org.md)

## Examples

``` r
txt           # Show all elements and current labels
#> $scen_lbl
#> [1] "Scenario"
#> 
#> $scen_txt
#> [1] "Description of the current scenario in a short paragraph of text. This description may include several sentences."
#> 
#> $scen_src
#> [1] "Source information"
#> 
#> $scen_apa
#> [1] "Source information in APA format"
#> 
#> $scen_lng
#> [1] "en/de"
#> 
#> $popu_lbl
#> [1] "Population"
#> 
#> $N_lbl
#> [1] "N"
#> 
#> $cond_lbl
#> [1] "True condition"
#> 
#> $cond_true_lbl
#> [1] "present"
#> 
#> $cond_false_lbl
#> [1] "absent"
#> 
#> $dec_lbl
#> [1] "Outcome"
#> 
#> $dec_pos_lbl
#> [1] "positive"
#> 
#> $dec_neg_lbl
#> [1] "negative"
#> 
#> $acc_lbl
#> [1] "Accuracy"
#> 
#> $dec_cor_lbl
#> [1] "correct"
#> 
#> $dec_err_lbl
#> [1] "incorrect"
#> 
#> $sdt_lbl
#> [1] "Cases"
#> 
#> $hi_lbl
#> [1] "TP"
#> 
#> $mi_lbl
#> [1] "FN"
#> 
#> $fa_lbl
#> [1] "FP"
#> 
#> $cr_lbl
#> [1] "TN"
#> 
txt$scen_lbl  # Show the current scenario label (e.g., used in plot titles)
#> [1] "Scenario"
txt$scen_lbl <- "An example"  # Define a new scenario label
```

# List current values of scenario color palette.

`pal` is initialized to a vector of named elements (colors) to define
the scenario color scheme that is used throughout the **riskyr**
package.

## Usage

``` r
pal
```

## Details

All color information corresponding to the current scenario is stored as
named colors in a vector `pal`. To change a color, assign a new color to
an existing element name.

`pal` currently contains colors with the following names:

1.  `N` Color representing the *population* of
    [`N`](https://hneth.github.io/riskyr/reference/N.md) cases or
    individuals.

2.  `cond_true` Color representing cases of
    [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md),
    for which the current condition is `TRUE`.

3.  `cond_false` Color representing cases of in
    [`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md),
    for which the current condition is `FALSE`.

4.  `dec_pos` Color representing cases of
    [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md),
    for which the current decision is `positive`.

5.  `dec_neg` Color representing cases in
    [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md),
    for which the current decision is `negative`.

6.  `dec_cor` Color representing cases of correct decisions
    [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md),
    for which the current decision is `accurate`.

7.  `dec_err` Color representing cases of erroneous decisions
    [`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md),
    for which the current decision is `inaccurate`.

8.  `hi` Color representing *hits* or true positives in
    [`hi`](https://hneth.github.io/riskyr/reference/hi.md) (i.e.,
    correct cases for which the current condition is TRUE and the
    decision is positive).

9.  `mi` Color representing *misses* or false negatives in
    [`mi`](https://hneth.github.io/riskyr/reference/mi.md) (i.e.,
    incorrect cases for which the current condition is TRUE but the
    decision is negative).

10. `fa` Color representing *false alarms* or false positives in
    [`fa`](https://hneth.github.io/riskyr/reference/fa.md) (i.e.,
    incorrect cases for which the current condition is FALSE but the
    decision is positive).

11. `cr` Color representing *correct rejections* or true negatives in
    [`cr`](https://hneth.github.io/riskyr/reference/cr.md) (i.e.,
    correct cases for which the current condition is FALSE and the
    decision is negative).

12. `ppv` Color representing *positive predictive values*
    [`PPV`](https://hneth.github.io/riskyr/reference/PPV.md) (i.e., the
    conditional probability that the condition is TRUE, provided that
    the decision is positive).

13. `npv` Color representing *negative predictive values*
    [`NPV`](https://hneth.github.io/riskyr/reference/NPV.md) (i.e., the
    conditional probability that the condition is FALSE, provided that
    the decision is negative).

14. `txt` Color used for text labels.

15. `brd` Color used for borders.

16. `bg` Color used for plot background (used to set
    `par(bg = bg_col)`).

Note that color names for frequencies correspond to frequency names, but
are different for probabilities (which are written in lowercase and only
[`PPV`](https://hneth.github.io/riskyr/reference/PPV.md) and
[`NPV`](https://hneth.github.io/riskyr/reference/NPV.md) have assigned
colors).

## See also

[`init_pal`](https://hneth.github.io/riskyr/reference/init_pal.md)
initializes color information;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`txt`](https://hneth.github.io/riskyr/reference/txt.md) contains
current text information;
[`init_txt`](https://hneth.github.io/riskyr/reference/init_txt.md)
initializes text information;
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
[`txt`](https://hneth.github.io/riskyr/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/reference/txt_org.md)

## Examples

``` r
pal        # shows all color names and current values
#>                 N         cond_true        cond_false           dec_pos 
#>       "#E6E6E6FC" "lightgoldenrod1"       "#BFBFBFFC"         "#F7A97F" 
#>           dec_neg           dec_cor           dec_err                hi 
#>       "#A6A6A6FC"      "olivedrab4"      "steelblue3"         "#B8D989" 
#>                mi                fa                cr               ppv 
#>         "#73C8EA"         "#1D95C6"         "#80B139"         "#F26418" 
#>               npv               txt               brd                bg 
#>         "#1D95C6"       "#000000FC"         "#3E3F3A"           "white" 
pal["hi"]  # shows the current color for hits (true positives, TP)
#>        hi 
#> "#B8D989" 
pal["hi"] <- "gold"  # defines a new color for hits (true positives, TP)
```

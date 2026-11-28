# Initialize basic color information.

`init_pal` initializes basic color information (i.e., all colors
corresponding to functional roles in the current scenario and used
throughout the **riskyr** package).

## Usage

``` r
init_pal(
  N_col = pal_def["N"],
  cond_true_col = pal_def["cond_true"],
  cond_false_col = pal_def["cond_false"],
  dec_pos_col = pal_def["dec_pos"],
  dec_neg_col = pal_def["dec_neg"],
  dec_cor_col = pal_def["dec_cor"],
  dec_err_col = pal_def["dec_err"],
  hi_col = pal_def["hi"],
  mi_col = pal_def["mi"],
  fa_col = pal_def["fa"],
  cr_col = pal_def["cr"],
  PPV_col = pal_def["ppv"],
  NPV_col = pal_def["npv"],
  txt_col = pal_def["txt"],
  bg_col = pal_def["bg"],
  brd_col = pal_def["brd"]
)
```

## Arguments

- N_col:

  Color representing the *population* of
  [`N`](https://hneth.github.io/riskyr/reference/N.md) cases or
  individuals.

- cond_true_col:

  Color representing cases of
  [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md),
  for which the current condition is `TRUE`.

- cond_false_col:

  Color representing cases of in
  [`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md),
  for which the current condition is `FALSE`.

- dec_pos_col:

  Color representing cases of
  [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md), for
  which the current decision is `positive`.

- dec_neg_col:

  Color representing cases in
  [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md), for
  which the current decision is `negative`.

- dec_cor_col:

  Color representing cases of correct decisions
  [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md), for
  which the current decision is `accurate`.

- dec_err_col:

  Color representing cases in erroneous decisions
  [`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md), for
  which the current decision is `inaccurate`.

- hi_col:

  Color representing *hits* or true positives in
  [`hi`](https://hneth.github.io/riskyr/reference/hi.md) (i.e., correct
  cases for which the current condition is TRUE and the decision is
  positive).

- mi_col:

  Color representing *misses* or false negatives in
  [`mi`](https://hneth.github.io/riskyr/reference/mi.md) (i.e.,
  incorrect cases for which the current condition is TRUE but the
  decision is negative).

- fa_col:

  Color representing *false alarms* or false positives in
  [`fa`](https://hneth.github.io/riskyr/reference/fa.md) (i.e.,
  incorrect cases for which the current condition is FALSE but the
  decision is positive).

- cr_col:

  Color representing *correct rejections* or true negatives in
  [`cr`](https://hneth.github.io/riskyr/reference/cr.md) (i.e., correct
  cases for which the current condition is FALSE and the decision is
  negative).

- PPV_col:

  Color representing *positive predictive values*
  [`PPV`](https://hneth.github.io/riskyr/reference/PPV.md) (i.e., the
  conditional probability that the condition is TRUE, provided that the
  decision is positive).

- NPV_col:

  Color representing *negative predictive values*
  [`NPV`](https://hneth.github.io/riskyr/reference/NPV.md) (i.e., the
  conditional probability that the condition is FALSE, provided that the
  decision is negative).

- txt_col:

  Color used for text labels.

- bg_col:

  Background color of plot (used to set `par(bg = bg_col)`).

- brd_col:

  Color used for borders (e.g., around bars or boxes).

## Details

All color information of the current scenario is stored as named colors
in a list `pal`. `init_pal` allows changing colors by assigning new
colors to existing names.

## See also

[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`txt`](https://hneth.github.io/riskyr/reference/txt.md) contains
current text information;
[`init_txt`](https://hneth.github.io/riskyr/reference/init_txt.md)
initializes text information;
[`pal`](https://hneth.github.io/riskyr/reference/pal.md) contains
current color information; `init_pal` initializes color information;
[`freq`](https://hneth.github.io/riskyr/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/reference/comp_freq.md)
computes current frequency information;
[`prob`](https://hneth.github.io/riskyr/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/reference/comp_prob.md)
computes current probability information.

Other functions initializing scenario information:
[`init_num()`](https://hneth.github.io/riskyr/reference/init_num.md),
[`init_txt()`](https://hneth.github.io/riskyr/reference/init_txt.md),
[`riskyr()`](https://hneth.github.io/riskyr/reference/riskyr.md)

## Examples

``` r
init_pal()          # => define and return a vector of current (default) colors
#>                 N         cond_true        cond_false           dec_pos 
#>       "#E6E6E6FC" "lightgoldenrod1"   "lightskyblue2"      "rosybrown3" 
#>           dec_neg           dec_cor           dec_err                hi 
#> "lightsteelblue3"      "palegreen3"      "firebrick3"      "olivedrab4" 
#>                mi                fa                cr               ppv 
#>         "tomato3"    "lightsalmon2"      "olivedrab3"         "sienna1" 
#>               npv               txt               brd                bg 
#>      "steelblue3"       "#030303FC"       "#333333FC"           "white" 
length(init_pal())  # => 15 named colors
#> [1] 16
pal <- init_pal(N_col = "steelblue4")  # => change a color (stored in pal)
pal <- init_pal(brd_col = NA)          # => remove a color
```

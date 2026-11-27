# Initialize basic text elements.

`init_txt` initializes basic text elements
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) (i.e., all
titles and labels corresponding to the current scenario) that are used
throughout the `riskyr` package.

## Usage

``` r
init_txt(
  scen_lbl = txt_lbl_def$scen_lbl,
  scen_txt = txt_lbl_def$scen_txt,
  scen_src = txt_lbl_def$scen_src,
  scen_apa = txt_lbl_def$scen_apa,
  scen_lng = txt_lbl_def$scen_lng,
  popu_lbl = txt_lbl_def$popu_lbl,
  N_lbl = txt_lbl_def$N_lbl,
  cond_lbl = txt_lbl_def$cond_lbl,
  cond_true_lbl = txt_lbl_def$cond_true_lbl,
  cond_false_lbl = txt_lbl_def$cond_false_lbl,
  dec_lbl = txt_lbl_def$dec_lbl,
  dec_pos_lbl = txt_lbl_def$dec_pos_lbl,
  dec_neg_lbl = txt_lbl_def$dec_neg_lbl,
  acc_lbl = txt_lbl_def$acc_lbl,
  dec_cor_lbl = txt_lbl_def$dec_cor_lbl,
  dec_err_lbl = txt_lbl_def$dec_err_lbl,
  sdt_lbl = txt_lbl_def$sdt_lbl,
  hi_lbl = txt_lbl_def$hi_lbl,
  mi_lbl = txt_lbl_def$mi_lbl,
  fa_lbl = txt_lbl_def$fa_lbl,
  cr_lbl = txt_lbl_def$cr_lbl
)
```

## Arguments

- scen_lbl:

  The current scenario title (sometimes in Title Caps).

- scen_txt:

  A longer text description of the current scenario (which may extend
  over several lines).

- scen_src:

  The source information for the current scenario.

- scen_apa:

  Source information in APA format.

- scen_lng:

  Language of the current scenario (as character code). Options: `"en"`:
  English, `"de"`: German.

- popu_lbl:

  A general name describing the current *population*.

- N_lbl:

  A brief label for the current population
  [`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) or
  sample.

- cond_lbl:

  A general name for the *condition* dimension currently considered
  (e.g., some clinical condition).

- cond_true_lbl:

  A short label for the *presence* of the current condition or
  [`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)
  cases (the condition's true state of TRUE).

- cond_false_lbl:

  A short label for the *absence* of the current condition or
  [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
  cases (the condition's true state of FALSE).

- dec_lbl:

  A general name for the *decision* dimension (e.g., some diagnostic
  test) currently made.

- dec_pos_lbl:

  A short label for *positive* decisions or
  [`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)
  cases (e.g., predicting the presence of the condition).

- dec_neg_lbl:

  A short label for *negative* decisions or
  [`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)
  cases (e.g., predicting the absence of the condition).

- acc_lbl:

  A general name for the *accuracy* dimension (e.g., correspondence of
  decision to condition).

- dec_cor_lbl:

  A short label for *correct* decisions or
  [`dec_cor`](https://hneth.github.io/riskyr/dev/reference/dec_cor.md)
  cases (e.g., accurately predicting the condition).

- dec_err_lbl:

  A short label for *erroneous* decisions or
  [`dec_err`](https://hneth.github.io/riskyr/dev/reference/dec_err.md)
  cases (e.g., inaccurately predicting the condition).

- sdt_lbl:

  A name for the case/category/cell dimension in the 2x2 contingency
  table (SDT: condition x decision).

- hi_lbl:

  A short label for *hits* or *true positives*
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md) (i.e.,
  correct decisions of the presence of the condition, when the condition
  is actually present).

- mi_lbl:

  A short label for *misses* or *false negatives*
  [`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md) (i.e.,
  incorrect decisions of the absence of the condition when the condition
  is actually present).

- fa_lbl:

  A short label for *false alarms* or *false positives*
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) (i.e.,
  incorrect decisions of the presence of the condition when the
  condition is actually absent).

- cr_lbl:

  A short label for *correct rejections* or *true negatives*
  [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) (i.e., a
  correct decision of the absence of the condition, when the condition
  is actually absent).

## Details

All textual elements that specify titles and details of the current
scenario are stored as named elements (of type character) in a list
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md). `init_txt`
allows changing elements by assigning new character objects to existing
names.

However, you can directly specify scenario-specific text elements when
defining a scenario with the
[`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)
function.

## See also

[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for current
text settings;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) for current
color settings;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) for basic
numeric parameters.

Other functions initializing scenario information:
[`init_num()`](https://hneth.github.io/riskyr/dev/reference/init_num.md),
[`init_pal()`](https://hneth.github.io/riskyr/dev/reference/init_pal.md),
[`riskyr()`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)

## Examples

``` r
init_txt()          # defines a list of (default) text elements
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
#> [1] "Condition"
#> 
#> $cond_true_lbl
#> [1] "True"
#> 
#> $cond_false_lbl
#> [1] "False"
#> 
#> $dec_lbl
#> [1] "Decision"
#> 
#> $dec_pos_lbl
#> [1] "Positive"
#> 
#> $dec_neg_lbl
#> [1] "Negative"
#> 
#> $acc_lbl
#> [1] "Accuracy"
#> 
#> $dec_cor_lbl
#> [1] "Correct"
#> 
#> $dec_err_lbl
#> [1] "Erroneous"
#> 
#> $sdt_lbl
#> [1] "Cases"
#> 
#> $hi_lbl
#> [1] "hi"
#> 
#> $mi_lbl
#> [1] "mi"
#> 
#> $fa_lbl
#> [1] "fa"
#> 
#> $cr_lbl
#> [1] "cr"
#> 
length(init_txt())  # 21
#> [1] 21

# Customizing current text elements:
txt <- init_txt(scen_lbl = "My scenario",
                scen_src = "My source",
                N_lbl    = "My population")
```

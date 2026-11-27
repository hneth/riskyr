# Compute a population table (data) from frequencies (description).

`comp_popu` computes a table
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) (as an R
data frame) from the current frequency information (contained in
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md)).

## Usage

``` r
comp_popu(
  hi = freq$hi,
  mi = freq$mi,
  fa = freq$fa,
  cr = freq$cr,
  cond_lbl = txt$cond_lbl,
  cond_true_lbl = txt$cond_true_lbl,
  cond_false_lbl = txt$cond_false_lbl,
  dec_lbl = txt$dec_lbl,
  dec_pos_lbl = txt$dec_pos_lbl,
  dec_neg_lbl = txt$dec_neg_lbl,
  sdt_lbl = txt$sdt_lbl,
  hi_lbl = txt$hi_lbl,
  mi_lbl = txt$mi_lbl,
  fa_lbl = txt$fa_lbl,
  cr_lbl = txt$cr_lbl
)
```

## Format

An object of class `data.frame` with
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) rows and 3
columns (e.g., `"X/truth/cd", "Y/test/dc", "SDT/cell/class"`).

## Arguments

- hi:

  The number of hits
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md) (or true
  positives).

- mi:

  The number of misses
  [`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md) (or false
  negatives).

- fa:

  The number of false alarms
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) (or false
  positives).

- cr:

  The number of correct rejections
  [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) (or true
  negatives).

- cond_lbl:

  Text label for condition dimension ("by cd" perspective).

- cond_true_lbl:

  Text label for
  [`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)
  cases.

- cond_false_lbl:

  Text label for
  [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
  cases.

- dec_lbl:

  Text label for decision dimension ("by dc" perspective).

- dec_pos_lbl:

  Text label for
  [`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)
  cases.

- dec_neg_lbl:

  Text label for
  [`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)
  cases.

- sdt_lbl:

  Text label for 4 cases/combinations (SDT classifications).

- hi_lbl:

  Text label for
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md) cases.

- mi_lbl:

  Text label for
  [`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md) cases.

- fa_lbl:

  Text label for
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) cases.

- cr_lbl:

  Text label for
  [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) cases.

## Value

A data frame `popu` containing
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) rows
(individual cases) and 3 columns (e.g.,
`"X/truth/cd", "Y/test/dc", "SDT/cell/class"`). encoded as ordered
factors (with 2, 2, and 4 levels, respectively).

## Details

By default, `comp_popu` uses the text settings contained in
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md).

A visualization of the current population `popu` is provided by
[`plot_icons`](https://hneth.github.io/riskyr/dev/reference/plot_icons.md).

## See also

[`read_popu`](https://hneth.github.io/riskyr/dev/reference/read_popu.md)
creates a scenario (description) from data (as df);
[`write_popu`](https://hneth.github.io/riskyr/dev/reference/write_popu.md)
creates data (as df) from a riskyr scenario (description);
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) for data
format; [`num`](https://hneth.github.io/riskyr/dev/reference/num.md) for
basic numeric parameters;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) for
current frequency information;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for current
text settings;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) for current
color settings.

Other functions converting data/descriptions:
[`read_popu()`](https://hneth.github.io/riskyr/dev/reference/read_popu.md),
[`write_popu()`](https://hneth.github.io/riskyr/dev/reference/write_popu.md)

## Examples

``` r
popu <- comp_popu()  # => initializes popu (with current values of freq and txt)
dim(popu)            # => N x 3
#> [1] 1000    3
head(popu)
#>   True condition  Outcome Cases
#> 1        present positive    TP
#> 2        present positive    TP
#> 3        present positive    TP
#> 4        present positive    TP
#> 5        present positive    TP
#> 6        present positive    TP

# (A) Diagnostic/screening scenario (using default labels):
comp_popu(hi = 4, mi = 1, fa = 2, cr = 3)  # => computes a table of N = 10 cases.
#>    True condition  Outcome Cases
#> 1         present positive    TP
#> 2         present positive    TP
#> 3         present positive    TP
#> 4         present positive    TP
#> 5         present negative    FN
#> 6          absent positive    FP
#> 7          absent positive    FP
#> 8          absent negative    TN
#> 9          absent negative    TN
#> 10         absent negative    TN

# (B) Intervention/treatment scenario:
comp_popu(hi = 3, mi = 2, fa = 1, cr = 4,
          cond_lbl = "Treatment", cond_true_lbl = "pill", cond_false_lbl = "placebo",
          dec_lbl = "Health status", dec_pos_lbl = "healthy", dec_neg_lbl = "sick")
#>    Treatment Health status Cases
#> 1       pill       healthy    TP
#> 2       pill       healthy    TP
#> 3       pill       healthy    TP
#> 4       pill          sick    FN
#> 5       pill          sick    FN
#> 6    placebo       healthy    FP
#> 7    placebo          sick    TN
#> 8    placebo          sick    TN
#> 9    placebo          sick    TN
#> 10   placebo          sick    TN

# (C) Prevention scenario (e.g., vaccination):
comp_popu(hi = 3, mi = 2, fa = 1, cr = 4,
          cond_lbl = "Vaccination", cond_true_lbl = "yes", cond_false_lbl = "no",
          dec_lbl = "Disease", dec_pos_lbl = "no flu", dec_neg_lbl = "flu")
#>    Vaccination Disease Cases
#> 1          yes  no flu    TP
#> 2          yes  no flu    TP
#> 3          yes  no flu    TP
#> 4          yes     flu    FN
#> 5          yes     flu    FN
#> 6           no  no flu    FP
#> 7           no     flu    TN
#> 8           no     flu    TN
#> 9           no     flu    TN
#> 10          no     flu    TN
```

# Write a population table (data) from a riskyr scenario (description).

`write_popu` computes (or expands) a table
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) (as an R
data frame) from a
[`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)
scenario (description), using its 4 essential frequencies.

## Usage

``` r
write_popu(x = NULL, ...)
```

## Format

An object of class `data.frame` with
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) rows and 3
columns (e.g., `"X/truth/cd", "Y/test/dc", "SDT/cell/class"`).

## Arguments

- x:

  A [`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)
  scenario (description).

- ...:

  Additional parameters (text labels, passed to
  [`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md)).

## Value

A data frame `popu` containing
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) rows
(individual cases) and 3 columns (e.g.,
`"X/truth/cd", "Y/test/dc", "SDT/cell/class"`). encoded as ordered
factors (with 2, 2, and 4 levels, respectively).

## Details

`write_popu` expects a
[`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)
scenario as input and passes its 4 essential frequencies (rounded to
integers) to
[`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md).

By default, `write_popu` uses the text settings contained in
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md), but labels
can be changed by passing arguments to
[`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md)
(via `...`).

## See also

[`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md)
creates data (as df) from description (frequencies);
[`read_popu`](https://hneth.github.io/riskyr/dev/reference/read_popu.md)
creates a scenario (description) from data (as df);
[`popu`](https://hneth.github.io/riskyr/dev/reference/popu.md) for data
format; [`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for
current text settings;
[`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)
initializes a `riskyr` scenario.

Other functions converting data/descriptions:
[`comp_popu()`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md),
[`read_popu()`](https://hneth.github.io/riskyr/dev/reference/read_popu.md)

## Examples

``` r
# Define scenarios (by description):
s1 <- riskyr(prev = .5, sens = .5, spec = .5, N = 10)  # s1: define by 3 prob & N
s2 <- riskyr(hi = 2, mi = 3, fa = 2, cr = 3)           # s2: same scenario by 4 freq

# Create data (from descriptions):
write_popu(s1)  # data from (prob) description
#>    True condition  Outcome Cases
#> 1         present positive    TP
#> 2         present positive    TP
#> 3         present negative    FN
#> 4         present negative    FN
#> 5         present negative    FN
#> 6          absent positive    FP
#> 7          absent positive    FP
#> 8          absent positive    FP
#> 9          absent negative    TN
#> 10         absent negative    TN
write_popu(s2,  # data from (freq) description & change labels:
           cond_lbl = "Disease (X)",
           cond_true_lbl = "sick", cond_false_lbl = "healthy",
           dec_lbl = "Test (Y)")
#>    Disease (X) Test (Y) Cases
#> 1         sick positive    TP
#> 2         sick positive    TP
#> 3         sick negative    FN
#> 4         sick negative    FN
#> 5         sick negative    FN
#> 6      healthy positive    FP
#> 7      healthy positive    FP
#> 8      healthy negative    TN
#> 9      healthy negative    TN
#> 10     healthy negative    TN

# Rounding:
s3 <- riskyr(prev = 1/3, sens = 2/3, spec = 6/7, N = 10, round = FALSE)  # s3: w/o rounding
write_popu(s3, cond_lbl = "X", dec_lbl = "Y", sdt_lbl = "class")  # rounded to nearest integers
#>          X        Y class
#> 1  present positive    TP
#> 2  present positive    TP
#> 3  present negative    FN
#> 4   absent positive    FP
#> 5   absent negative    TN
#> 6   absent negative    TN
#> 7   absent negative    TN
#> 8   absent negative    TN
#> 9   absent negative    TN
#> 10  absent negative    TN

# Sampling:
s4 <- riskyr(prev = 1/3, sens = 2/3, spec = 6/7, N = 10, sample = TRUE)  # s4: with sampling
write_popu(s4, cond_lbl = "X", dec_lbl = "Y", sdt_lbl = "class")  # data from sampling
#>          X        Y class
#> 1  present positive    TP
#> 2  present positive    TP
#> 3  present positive    TP
#> 4  present positive    TP
#> 5  present positive    TP
#> 6   absent positive    FP
#> 7   absent negative    TN
#> 8   absent negative    TN
#> 9   absent negative    TN
#> 10  absent negative    TN
```

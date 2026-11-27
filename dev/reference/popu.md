# A table of cases (in the current population).

`popu` is an R data frame that is computed by
[`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md)
from the current frequency information (contained in
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md)). Each
individual is represented as a row; columns represent the individual's
condition (`TRUE` or `FALSE`), a corresponding decision (also encoded as
`TRUE` = positive or `FALSE` = negative), and its classification (i.e.,
its case or cell combination, in SDT terms), as true positive (hit
[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md)), false
negative (miss
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md)), false
positive (false alarm
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md)), or true
negative (correct rejection
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)).

## Usage

``` r
popu
```

## Value

A data frame `popu` containing
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) rows
(individual cases) and 3 columns (`"Truth", "Decision", "SDT"`) encoded
as ordered factors (with 2, 2, and 4 levels, respectively).

## Details

\#' `popu` is initialized to `NULL` and needs to be computed by calling
[`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md)
with current parameter settings.

By default,
[`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md)
uses the current information contained in
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) to define
text labels.

A visualization of the current population `popu` is provided by
[`plot_icons`](https://hneth.github.io/riskyr/dev/reference/plot_icons.md).

## See also

the corresponding generating function
[`comp_popu`](https://hneth.github.io/riskyr/dev/reference/comp_popu.md);
[`read_popu`](https://hneth.github.io/riskyr/dev/reference/read_popu.md)
interprets a data frame as a riskyr scenario;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) for basic
numeric parameters;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) for
current frequency information;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) for current
text settings.

## Examples

``` r
popu <- comp_popu()  # => initializes popu with current values of freq and txt
dim(popu)            # => N x 3
#> [1] 1000    3
head(popu)           # => shows head of data frame
#>   True condition  Outcome Cases
#> 1        present positive    TP
#> 2        present positive    TP
#> 3        present positive    TP
#> 4        present positive    TP
#> 5        present positive    TP
#> 6        present positive    TP
```

# Plot a frequency box object

`plot.box` is a utility method that allows to plot low level boxes for
`riskyr` plots.

## Usage

``` r
# S3 method for class 'box'
plot(x, cur_freq = freq, lbl_txt = txt, col_pal = pal, ...)
```

## Arguments

- x:

  The box (i.e., an object of class `box`) to be plotted.

- cur_freq:

  Current frequency information (see
  [`freq`](https://hneth.github.io/riskyr/reference/freq.md) for
  details).

- lbl_txt:

  Current text information (see
  [`txt`](https://hneth.github.io/riskyr/reference/txt.md) for details).

- col_pal:

  Current color palette (see
  [`pal`](https://hneth.github.io/riskyr/reference/pal.md) for details).

- ...:

  Additional (graphical) parameters to be passed to the underlying
  plotting functions.

## Details

`plot.riskyr` also uses the text settings specified in the "riskyr"
object.

## See also

Other utility functions:
[`as_pb()`](https://hneth.github.io/riskyr/reference/as_pb.md),
[`as_pc()`](https://hneth.github.io/riskyr/reference/as_pc.md),
[`print.box()`](https://hneth.github.io/riskyr/reference/print.box.md)

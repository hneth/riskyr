# Summarize a riskyr scenario.

`summary.riskyr` provides a `summary` method for objects of class
"riskyr".

## Usage

``` r
# S3 method for class 'riskyr'
summary(object = NULL, summarize = "all", ...)
```

## Format

An object of class `summary.riskyr` with up to 9 entries.

## Arguments

- object:

  A `riskyr` object, usually a result of a call to
  [`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md).
  Inbuilt `scenarios` are also of type `riskyr`.

- summarize:

  What is summarized as a vector consisting of
  `c("freq", "prob", "accu")` for frequencies, probabilities, and
  accuracy respectively. The default "all" is an alias to all three.

- ...:

  Additional parameters (to be passed to summary functions).

## Value

A summary list `obj.sum` with up to 9 entries, dependent on which
information is requested by `summarize`.

Scenario name, relevant condition , and `N` are summarized by default.

## See also

[`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)
initializes a `riskyr` scenario.

Other riskyr scenario functions:
[`plot.riskyr()`](https://hneth.github.io/riskyr/dev/reference/plot.riskyr.md),
[`riskyr()`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)

## Examples

``` r
summary(scenarios$n4)
#> Scenario:  Mammography 
#> 
#> Condition:  Breast cancer 
#> Decision:  Screening 
#> Population:  Women (general population) 
#> N =  1000 
#> Source:  Sedlmaier & Gigerenzer (2001), p.381ff. 
#> 
#> Probabilities:
#> 
#>  Essential probabilities:
#> prev sens mirt spec fart 
#> 0.01 0.80 0.20 0.90 0.10 
#> 
#>  Other probabilities:
#>  ppod   PPV   NPV   FDR   FOR   acc 
#> 0.107 0.075 0.998 0.925 0.002 0.899 
#> 
#> Frequencies:
#> 
#>  by conditions:
#>  cond_true cond_false 
#>         10        990 
#> 
#>  by decision:
#> dec_pos dec_neg 
#>     107     893 
#> 
#>  by correspondence (of decision to condition):
#> dec_cor dec_err 
#>     899     101 
#> 
#>  4 essential (SDT) frequencies:
#>  hi  mi  fa  cr 
#>   8   2  99 891 
#> 
#> Accuracy:
#> 
#>  acc:
#> 0.899
```

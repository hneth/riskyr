# Print summary information of a riskyr scenario.

`print.summary.riskyr` provides a `print` method for objects of class
"summary.riskyr".

## Usage

``` r
# S3 method for class 'summary.riskyr'
print(x = NULL, ...)
```

## Format

Printed output of a "summary.riskyr" object.

## Arguments

- x:

  An object of class "summary.riskyr", usually a result of a call to
  `summary.riskyr`.

- ...:

  Additional parameters (to be passed to generic print function).

## See also

[`riskyr`](https://hneth.github.io/riskyr/reference/riskyr.md)
initializes a `riskyr` scenario.

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

# List current values of basic numeric variables.

`num` is a list of named numeric variables containing 4 basic
probabilities
([`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md),
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md), and
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)) and 1
frequency parameter (the population size
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md)).

## Usage

``` r
num
```

## See also

[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric parameters;
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md) contains
current text information;
[`init_txt`](https://hneth.github.io/riskyr/dev/reference/init_txt.md)
initializes text information;
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/dev/reference/init_pal.md)
initializes color information;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information.

Other lists containing current scenario information:
[`accu`](https://hneth.github.io/riskyr/dev/reference/accu.md),
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md),
[`pal`](https://hneth.github.io/riskyr/dev/reference/pal.md),
[`pal_bw`](https://hneth.github.io/riskyr/dev/reference/pal_bw.md),
[`pal_bwp`](https://hneth.github.io/riskyr/dev/reference/pal_bwp.md),
[`pal_kn`](https://hneth.github.io/riskyr/dev/reference/pal_kn.md),
[`pal_mbw`](https://hneth.github.io/riskyr/dev/reference/pal_mbw.md),
[`pal_mod`](https://hneth.github.io/riskyr/dev/reference/pal_mod.md),
[`pal_org`](https://hneth.github.io/riskyr/dev/reference/pal_org.md),
[`pal_rgb`](https://hneth.github.io/riskyr/dev/reference/pal_rgb.md),
[`pal_unikn`](https://hneth.github.io/riskyr/dev/reference/pal_unikn.md),
[`pal_vir`](https://hneth.github.io/riskyr/dev/reference/pal_vir.md),
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md),
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md),
[`txt_TF`](https://hneth.github.io/riskyr/dev/reference/txt_TF.md),
[`txt_org`](https://hneth.github.io/riskyr/dev/reference/txt_org.md)

## Examples

``` r
num <- init_num()  # => initialize num to default parameters
num                # => show defaults
#> $prev
#> [1] 0.25
#> 
#> $sens
#> [1] 0.85
#> 
#> $spec
#> [1] 0.75
#> 
#> $fart
#> [1] 0.25
#> 
#> $N
#> [1] 1000
#> 
length(num)        # => 5
#> [1] 5
```

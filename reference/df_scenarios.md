# A collection of riskyr scenarios from various sources (as df).

`df_scenarios` is an R data frame that contains a collection of
scenarios from the scientific literature and other sources.

## Usage

``` r
df_scenarios
```

## Format

A data frame with currently 25 rows (i.e., scenarios) and 21 columns
(variables describing each scenario):

See [`scenarios`](https://hneth.github.io/riskyr/reference/scenarios.md)
for a list of scenarios and the variables currently contained in
`df_scenarios`.

Note that names of variables (columns) correspond to a subset of
[`init_txt`](https://hneth.github.io/riskyr/reference/init_txt.md) (to
initialize [`txt`](https://hneth.github.io/riskyr/reference/txt.md)) and
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md) (to
initialize [`num`](https://hneth.github.io/riskyr/reference/num.md)).

The variables `scen_src` and `scen_apa` provide a scenario's source
information.

## Details

When loading `riskyr`, all scenarios contained in `df_scenarios` are
converted into a list of `riskyr` objects
[`scenarios`](https://hneth.github.io/riskyr/reference/scenarios.md).

## See also

[`scenarios`](https://hneth.github.io/riskyr/reference/scenarios.md)
contains all scenarios as `riskyr` objects;
[`riskyr`](https://hneth.github.io/riskyr/reference/riskyr.md)
initializes a `riskyr` scenario;
[`txt`](https://hneth.github.io/riskyr/reference/txt.md) contains basic
text information;
[`init_txt`](https://hneth.github.io/riskyr/reference/init_txt.md)
initializes text information;
[`num`](https://hneth.github.io/riskyr/reference/num.md) contains basic
numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/reference/init_num.md)
initializes basic numeric parameters;
[`pal`](https://hneth.github.io/riskyr/reference/pal.md) contains
current color information;
[`init_pal`](https://hneth.github.io/riskyr/reference/init_pal.md)
initializes color information.

Other datasets:
[`BRCA1`](https://hneth.github.io/riskyr/reference/BRCA1.md),
[`BRCA1_mam`](https://hneth.github.io/riskyr/reference/BRCA1_mam.md),
[`BRCA1_ova`](https://hneth.github.io/riskyr/reference/BRCA1_ova.md),
[`BRCA2`](https://hneth.github.io/riskyr/reference/BRCA2.md),
[`BRCA2_mam`](https://hneth.github.io/riskyr/reference/BRCA2_mam.md),
[`BRCA2_ova`](https://hneth.github.io/riskyr/reference/BRCA2_ova.md),
[`t_A`](https://hneth.github.io/riskyr/reference/t_A.md),
[`t_B`](https://hneth.github.io/riskyr/reference/t_B.md),
[`t_I`](https://hneth.github.io/riskyr/reference/t_I.md)

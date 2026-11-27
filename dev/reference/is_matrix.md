# Verify a 2x2 matrix as a numeric contingency table.

`is_matrix` verifies that `mx` is a valid 2x2 matrix (i.e., a numeric
contingency table).

## Usage

``` r
is_matrix(mx)
```

## Arguments

- mx:

  An object to verify (required).

## Value

A Boolean value: `TRUE` if `mx` is a numeric matrix and 2x2 contingency
table; otherwise `FALSE`.

## Details

`is_matrix` is more restrictive than
[`is.matrix`](https://rdrr.io/r/base/matrix.html), as it also requires
that `mx` [`is.numeric`](https://rdrr.io/r/base/numeric.html),
[`is.table`](https://rdrr.io/r/base/table.html), `nrows(mx) == 2`, and
`ncols(mx) == 2`.

## References

Neth, H., Gradwohl, N., Streeb, D., Keim, D.A., & Gaissmaier, W. (2021).
Perspectives on the 2×2 matrix: Solving semantically distinct problems
based on a shared structure of binary contingencies. *Frontiers in
Psychology*, *11*, 567817. doi:
[doi:10.3389/fpsyg.2020.567817](https://doi.org/10.3389/fpsyg.2020.567817)

## See also

Other verification functions:
[`is_complement()`](https://hneth.github.io/riskyr/dev/reference/is_complement.md),
[`is_extreme_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_extreme_prob_set.md),
[`is_freq()`](https://hneth.github.io/riskyr/dev/reference/is_freq.md),
[`is_integer()`](https://hneth.github.io/riskyr/dev/reference/is_integer.md),
[`is_perc()`](https://hneth.github.io/riskyr/dev/reference/is_perc.md),
[`is_prob()`](https://hneth.github.io/riskyr/dev/reference/is_prob.md),
[`is_suff_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_suff_prob_set.md),
[`is_valid_freq_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_freq_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_triple.md)

## Examples

``` r
is_matrix(1:4)
#> is_matrix: mx is no matrix.
#> [1] FALSE
is_matrix(matrix("A"))
#> is_matrix: mx is no contingency table.
#> [1] FALSE
is_matrix(matrix(1:4))
#> is_matrix: mx is no contingency table.
#> [1] FALSE
is_matrix(as.table(matrix(1:4, nrow = 1, ncol = 4)))
#> is_matrix: mx does not have 2 rows.
#> [1] FALSE
is_matrix(as.table(matrix(1:4, nrow = 4, ncol = 1)))
#> is_matrix: mx does not have 2 rows.
#> [1] FALSE
is_matrix(as.table(matrix(1:4, nrow = 2, ncol = 2)))
#> [1] TRUE
```

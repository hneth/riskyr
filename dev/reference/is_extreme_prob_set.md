# Verify that a set of probabilities describes an extreme case.

`is_extreme_prob_set` verifies that a set of probabilities (i.e.,
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md), and
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) or
[`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) or
[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)) describe
an extreme case.

## Usage

``` r
is_extreme_prob_set(prev, sens = NA, mirt = NA, spec = NA, fart = NA)
```

## Arguments

- prev:

  The condition's prevalence
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (i.e.,
  the probability of condition being `TRUE`).

- sens:

  The decision's sensitivity
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `TRUE`). `sens` is optional when its complement `mirt` is
  provided.

- mirt:

  The decision's miss rate
  [`mirt`](https://hneth.github.io/riskyr/dev/reference/mirt.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `TRUE`). `mirt` is optional when its complement `sens` is
  provided.

- spec:

  The decision's specificity
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is `FALSE`). `spec` is optional when its complement `fart`
  is provided.

- fart:

  The decision's false alarm rate
  [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is `FALSE`). `fart` is optional when its complement `spec`
  is provided.

## Value

A Boolean value: `TRUE` if an extreme case is identified; otherwise
`FALSE`.

## Details

If `TRUE`, a warning message describing the nature of the extreme case
is printed to allow anticipating peculiar effects (e.g., that
[`PPV`](https://hneth.github.io/riskyr/dev/reference/PPV.md) or
[`NPV`](https://hneth.github.io/riskyr/dev/reference/NPV.md) values
cannot be computed or are `NaN`).

This function does not verify the type, range, sufficiency, or
consistency of its arguments. See
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md),
[`is_suff_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_suff_prob_set.md),
[`is_complement`](https://hneth.github.io/riskyr/dev/reference/is_complement.md),
[`is_valid_prob_pair`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md)
and
[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)
for these purposes.

## See also

[`is_valid_prob_pair`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md)
verifies that a pair of probabilities can be complements;
[`is_valid_prob_set`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md)
verifies the validity of a set of probability inputs;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric variables;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric variables;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information;
[`as_pc`](https://hneth.github.io/riskyr/dev/reference/as_pc.md)
displays a probability as a percentage;
[`as_pb`](https://hneth.github.io/riskyr/dev/reference/as_pb.md)
displays a percentage as probability

Other verification functions:
[`is_complement()`](https://hneth.github.io/riskyr/dev/reference/is_complement.md),
[`is_freq()`](https://hneth.github.io/riskyr/dev/reference/is_freq.md),
[`is_integer()`](https://hneth.github.io/riskyr/dev/reference/is_integer.md),
[`is_matrix()`](https://hneth.github.io/riskyr/dev/reference/is_matrix.md),
[`is_perc()`](https://hneth.github.io/riskyr/dev/reference/is_perc.md),
[`is_prob()`](https://hneth.github.io/riskyr/dev/reference/is_prob.md),
[`is_suff_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_suff_prob_set.md),
[`is_valid_freq_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_freq_set.md),
[`is_valid_prob_pair()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_pair.md),
[`is_valid_prob_set()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_set.md),
[`is_valid_prob_triple()`](https://hneth.github.io/riskyr/dev/reference/is_valid_prob_triple.md)

## Examples

``` r
# Identify 6 extreme cases (+ 4 variants):
is_extreme_prob_set(1, 1, NA, 1, NA)       # => TRUE + warning: N true positives
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> [1] TRUE
plot_tree(1, 1, NA, 1, NA, N = 100)        # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.


is_extreme_prob_set(1, 0, NA, 1, NA)       # => TRUE + warning: N false negatives
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> [1] TRUE
plot_tree(1, 0, NA, 1, NA, N = 200)        # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: Extreme case (prev = 1 & sens = 0):
#>   N mi (FN) cases; 0 cond_false or dec_true cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.


sens <- .50
is_extreme_prob_set(0, sens, NA, 0, NA)    # => TRUE + warning: N false positives
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> [1] TRUE
plot_tree(0, sens, NA, 0, N = 300)         # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.

# Variant:
is_extreme_prob_set(0, sens, NA, NA, 1)    # => TRUE + warning: N false positives
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> [1] TRUE
plot_tree(0, sens, NA, NA, 1, N = 350)     # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.


sens <- .50
is_extreme_prob_set(0, sens, NA, 1)        # => TRUE + warning: N true negatives
#> Warning: Extreme case (prev = 0 & spec = 1):
#>   N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.
#> [1] TRUE
plot_tree(0, sens, NA, NA, 1, N = 400)     # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.

# Variant:
is_extreme_prob_set(0, sens, NA, NA, 0)    # => TRUE + warning: N true negatives
#> Warning: Extreme case (prev = 0 & spec = 1):
#>   N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.
#> [1] TRUE
plot_tree(0, sens, NA, NA, 0, N = 450)     # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (prev = 0 & spec = 1):
#>   N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.
#> Warning: Extreme case (prev = 0 & spec = 1):
#>   N cr (TN) cases; 0 cond_true or dec_false cases; NPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.


prev <- .50
is_extreme_prob_set(prev, 0, NA, 1, NA)    # => TRUE + warning: 0 hi and 0 fa (0 dec_pos cases)
#> Warning: Extreme case (sens = 0 & spec = 1):
#>   0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.
#> [1] TRUE
plot_tree(prev, 0, NA, 1, NA, N = 500)     # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (sens = 0 & spec = 1):
#>   0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.
#> Warning: Extreme case (sens = 0 & spec = 1):
#>   0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.

# # Variant:
is_extreme_prob_set(prev, 0, 0, NA, 0)     # => TRUE + warning: 0 hi and 0 fa (0 dec_pos cases)
#> Warning: Extreme case (sens = 0 & spec = 1):
#>   0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.
#> [1] TRUE
plot_tree(prev, 0, NA, 1, NA, N = 550)     # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (sens = 0 & spec = 1):
#>   0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.
#> Warning: Extreme case (sens = 0 & spec = 1):
#>   0 hi (TP) and 0 fa (FP) cases; 0 dec_pos cases; PPV = NaN.
#> Warning: PPV is NaN.
#> Warning: FDR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.


prev <- .50
is_extreme_prob_set(prev, 1, NA, 0, NA)    # => TRUE + warning: 0 mi and 0 cr (0 dec_neg cases)
#> Warning: Extreme case (sens = 1 & spec = 0):
#>   0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.
#> [1] TRUE
plot_tree(prev, 1, NA, 0, NA, N = 600)     # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (sens = 1 & spec = 0):
#>   0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.
#> Warning: Extreme case (sens = 1 & spec = 0):
#>   0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.

# # Variant:
is_extreme_prob_set(prev, 1, NA, 0, NA)    # => TRUE + warning: 0 mi and 0 cr (0 dec_neg cases)
#> Warning: Extreme case (sens = 1 & spec = 0):
#>   0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.
#> [1] TRUE
plot_tree(prev, 1, NA, 0, NA, N = 650)     # => illustrates this case
#> Function 'plot_tree' is deprecated; using 'plot_prism' (with by = 'cd'/'dc'/'ac') instead.
#> Warning: Extreme case (sens = 1 & spec = 0):
#>   0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.
#> Warning: Extreme case (sens = 1 & spec = 0):
#>   0 mi (FN) and 0 cr (TN) cases; 0 dec_neg cases; NPV = NaN.
#> Warning: NPV is NaN.
#> Warning: FOR is NaN.
#> Warning: Some derived prob values are peculiar. Check for extreme probabilities!
#> Argument 'title_lbl' is deprecated. Please use 'main' instead.

```

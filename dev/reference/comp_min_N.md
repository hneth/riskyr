# Compute a suitable minimum population size value N.

`comp_min_N` computes a population size value
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) (an integer as
a power of 10) so that the frequencies of the 4 combinations of
conditions and decisions (i.e., the cells of the confusion table, or
center row of boxes in the frequency prism) reach or exceed a minimum
value `min_freq` given the basic parameters
[`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md),
[`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md), and
[`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md)
(`spec = 1 - `[`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md)).

## Usage

``` r
comp_min_N(prev, sens, spec, min_freq = 1)
```

## Arguments

- prev:

  The condition's prevalence value
  [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md) (i.e.,
  the probability of condition being TRUE).

- sens:

  The decision's sensitivity value
  [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md) (i.e.,
  the conditional probability of a positive decision provided that the
  condition is TRUE).

- spec:

  The specificity value
  [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md) (i.e.,
  the conditional probability of a negative decision provided that the
  condition is FALSE).

- min_freq:

  The minimum frequency of each combination of a condition and a
  decision (i.e., hits, misses, false alarms, and correct rejections).
  Default: `min_freq = 1`.

## Value

An integer value
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md) (as a power of
10).

## Details

Using this function helps avoiding excessively small decimal values in
categories – especially
[`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) – when
expressing combinations of conditions and decisions as natural
frequencies. As values of zero (0) are tolerable, the function only
increases [`N`](https://hneth.github.io/riskyr/dev/reference/N.md) (in
powers of 10) while the current value of any frequency (cell in
confusion table or leaf of a frequency tree) is positive but below
`min_freq`.

By default,
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
and
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
round frequencies to nearest integers to avoid decimal values in
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) (i.e.,
`round = TRUE` by default). Using the option `round = FALSE` turns off
rounding.

## See also

population size
[`N`](https://hneth.github.io/riskyr/dev/reference/N.md);
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric parameters;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes frequencies from probabilities;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes probabilities from probabilities;
[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
computes current frequency information from (4 essential) frequencies;
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
computes current frequency information from (3 essential) probabilities;
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md)
computes current probability information from (4 essential) frequencies;
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
computes current probability information from (3 essential)
probabilities.

Other functions computing frequencies:
[`comp_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md),
[`comp_freq_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md),
[`comp_freq_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_prob_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)

## Examples

``` r
comp_min_N(0, 0, 0)  # => 1
#> Warning: Extreme case (prev = 0 & spec = 0):
#>   N fa (FP) cases; 0 cond_true or dec_true cases; PPV = NaN.
#> [1] 1
comp_min_N(1, 1, 1)  # => 1
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> [1] 1

comp_min_N(1, 1, 1, min_freq = 10)  # =>  10
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> [1] 10
comp_min_N(1, 1, 1, min_freq = 99)  # => 100
#> Warning: Extreme case (prev = 1 & sens = 1):
#>   N hi (TP) cases; 0 cond_false or dec_false cases; NPV = NaN.
#> [1] 100

comp_min_N(.1, .1, .1)        # =>       100 = 10^2
#> [1] 100
comp_min_N(.001, .1, .1)      # =>    10 000 = 10^4
#> [1] 10000
comp_min_N(.001, .001, .1)    # => 1 000 000 = 10^6
#> [1] 1e+06
comp_min_N(.001, .001, .001)  # => 1 000 000 = 10^6
#> [1] 1e+06
```

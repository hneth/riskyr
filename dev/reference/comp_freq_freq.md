# Compute frequencies from (4 essential) frequencies.

`comp_freq_freq` computes current frequency information from 4 essential
frequencies ([`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)). It returns
a list of 11 frequencies
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) for a
population of [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
individuals as its output.

## Usage

``` r
comp_freq_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)
```

## Arguments

- hi:

  The number of hits
  [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md) (or true
  positives).

- mi:

  The number of misses
  [`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md) (or false
  negatives).

- fa:

  The number of false alarms
  [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md) (or false
  positives).

- cr:

  The number of correct rejections
  [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md) (or true
  negatives).

## Details

Key relationships between frequencies and probabilities (see
documentation of
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
or
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
for details):

- Three perspectives on a population:

  by condition / by decision / by accuracy.

- Defining probabilities in terms of frequencies:

  Probabilities can be computed as ratios between frequencies, but
  beware of rounding issues.

Functions translating between representational formats:
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md),
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
`comp_freq_freq` (see documentation of
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
for details).

## See also

[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
computes current frequency information from (3 essential) probabilities;
[`comp_prob_freq`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md)
computes current probability information from (4 essential) frequencies;
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
computes current probability information from (3 essential)
probabilities;
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md) contains
basic numeric parameters;
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
initializes basic numeric parameters;
[`prob`](https://hneth.github.io/riskyr/dev/reference/prob.md) contains
current probability information;
[`comp_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md)
computes current probability information;
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) contains
current frequency information;
[`comp_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md)
computes current frequency information;
[`is_prob`](https://hneth.github.io/riskyr/dev/reference/is_prob.md)
verifies probability inputs;
[`is_freq`](https://hneth.github.io/riskyr/dev/reference/is_freq.md)
verifies frequency inputs.

Other functions computing frequencies:
[`comp_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq.md),
[`comp_freq_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_min_N()`](https://hneth.github.io/riskyr/dev/reference/comp_min_N.md),
[`comp_prob_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)

Other format conversion functions:
[`comp_freq_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_prob_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_freq.md),
[`comp_prob_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)

## Examples

``` r
## Basics:
comp_freq_freq()
#> $N
#> [1] 1000
#> 
#> $cond_true
#> [1] 250
#> 
#> $cond_false
#> [1] 750
#> 
#> $dec_pos
#> [1] 400
#> 
#> $dec_neg
#> [1] 600
#> 
#> $dec_cor
#> [1] NA
#> 
#> $dec_err
#> [1] NA
#> 
#> $hi
#> [1] 212
#> 
#> $mi
#> [1] 38
#> 
#> $fa
#> [1] 188
#> 
#> $cr
#> [1] 562
#> 
all.equal(freq, comp_freq_freq())  # => should be TRUE
#> [1] "Component “dec_cor”: Modes: numeric, logical"              
#> [2] "Component “dec_cor”: target is numeric, current is logical"
#> [3] "Component “dec_err”: Modes: numeric, logical"              
#> [4] "Component “dec_err”: target is numeric, current is logical"

## Circular chain:
# 1. Current numeric parameters:
num
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

# 2. Compute all 10 probabilities in prob (from essential probabilities):
prob <- comp_prob()
prob
#> $prev
#> [1] 0.25
#> 
#> $sens
#> [1] 0.85
#> 
#> $mirt
#> [1] 0.15
#> 
#> $spec
#> [1] 0.75
#> 
#> $fart
#> [1] 0.25
#> 
#> $ppod
#> [1] 0.4
#> 
#> $PPV
#> [1] 0.53125
#> 
#> $FDR
#> [1] 0.46875
#> 
#> $NPV
#> [1] 0.9375
#> 
#> $FOR
#> [1] 0.0625
#> 
#> $acc
#> [1] 0.775
#> 
#> $p_acc_hi
#> [1] 0.2741935
#> 
#> $p_err_fa
#> [1] 0.8333333
#> 

# 3. Compute 9 frequencies in freq from probabilities:
freq <- comp_freq(round = FALSE)   # no rounding (to obtain same probabilities later)
freq
#> $N
#> [1] 1000
#> 
#> $cond_true
#> [1] 250
#> 
#> $cond_false
#> [1] 750
#> 
#> $dec_pos
#> [1] 400
#> 
#> $dec_neg
#> [1] 600
#> 
#> $dec_cor
#> [1] 775
#> 
#> $dec_err
#> [1] 225
#> 
#> $hi
#> [1] 212.5
#> 
#> $mi
#> [1] 37.5
#> 
#> $fa
#> [1] 187.5
#> 
#> $cr
#> [1] 562.5
#> 

# 4. Compute 9 frequencies AGAIN (but now from frequencies):
freq_freq <- comp_freq_freq()

# 5. Check equality of results (steps 2. and 4.):
all.equal(freq, freq_freq)  # => should be TRUE!
#> [1] "Component “dec_cor”: Modes: numeric, logical"              
#> [2] "Component “dec_cor”: target is numeric, current is logical"
#> [3] "Component “dec_err”: Modes: numeric, logical"              
#> [4] "Component “dec_err”: target is numeric, current is logical"
#> [5] "Component “hi”: Mean relative difference: 0.002352941"     
#> [6] "Component “mi”: Mean relative difference: 0.01333333"      
#> [7] "Component “fa”: Mean relative difference: 0.002666667"     
#> [8] "Component “cr”: Mean relative difference: 0.0008888889"    
```

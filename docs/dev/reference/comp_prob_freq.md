# Compute probabilities from (4 essential) frequencies.

`comp_prob_freq` computes current probability information from 4
essential frequencies
([`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md),
[`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md),
[`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md),
[`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md)). It returns
a list of 11 frequencies
[`freq`](https://hneth.github.io/riskyr/dev/reference/freq.md) for a
population of [`N`](https://hneth.github.io/riskyr/dev/reference/N.md)
individuals as its output.

## Usage

``` r
comp_prob_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)
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
`comp_prob_freq`,
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
(see documentation of
[`comp_prob_prob`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)
for details).

## See also

[`comp_freq_freq`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md)
computes current frequency information from (4 essential) frequencies;
[`comp_freq_prob`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md)
computes current frequency information from (3 essential) probabilities;
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

Other functions computing probabilities:
[`comp_FDR()`](https://hneth.github.io/riskyr/dev/reference/comp_FDR.md),
[`comp_FOR()`](https://hneth.github.io/riskyr/dev/reference/comp_FOR.md),
[`comp_NPV()`](https://hneth.github.io/riskyr/dev/reference/comp_NPV.md),
[`comp_PPV()`](https://hneth.github.io/riskyr/dev/reference/comp_PPV.md),
[`comp_acc()`](https://hneth.github.io/riskyr/dev/reference/comp_acc.md),
[`comp_accu_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_freq.md),
[`comp_accu_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_accu_prob.md),
[`comp_comp_pair()`](https://hneth.github.io/riskyr/dev/reference/comp_comp_pair.md),
[`comp_complement()`](https://hneth.github.io/riskyr/dev/reference/comp_complement.md),
[`comp_complete_prob_set()`](https://hneth.github.io/riskyr/dev/reference/comp_complete_prob_set.md),
[`comp_err()`](https://hneth.github.io/riskyr/dev/reference/comp_err.md),
[`comp_fart()`](https://hneth.github.io/riskyr/dev/reference/comp_fart.md),
[`comp_mirt()`](https://hneth.github.io/riskyr/dev/reference/comp_mirt.md),
[`comp_ppod()`](https://hneth.github.io/riskyr/dev/reference/comp_ppod.md),
[`comp_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob.md),
[`comp_sens()`](https://hneth.github.io/riskyr/dev/reference/comp_sens.md),
[`comp_spec()`](https://hneth.github.io/riskyr/dev/reference/comp_spec.md)

Other format conversion functions:
[`comp_freq_freq()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_freq.md),
[`comp_freq_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_freq_prob.md),
[`comp_prob_prob()`](https://hneth.github.io/riskyr/dev/reference/comp_prob_prob.md)

## Examples

``` r
## Basics:
comp_prob_freq()  # => computes prob from current freq
#> $prev
#> [1] 0.25
#> 
#> $sens
#> [1] 0.848
#> 
#> $mirt
#> [1] 0.152
#> 
#> $spec
#> [1] 0.7493333
#> 
#> $fart
#> [1] 0.2506667
#> 
#> $ppod
#> [1] 0.4
#> 
#> $PPV
#> [1] 0.53
#> 
#> $FDR
#> [1] 0.47
#> 
#> $NPV
#> [1] 0.9366667
#> 
#> $FOR
#> [1] 0.06333333
#> 
#> $acc
#> [1] 0.774
#> 
#> $p_acc_hi
#> [1] 0.2739018
#> 
#> $p_err_fa
#> [1] 0.8318584
#> 

## Beware of rounding:
all.equal(prob, comp_prob_freq())  # => would be TRUE (IF freq were NOT rounded)!
#>  [1] "Component “sens”: Mean relative difference: 0.002352941"    
#>  [2] "Component “mirt”: Mean relative difference: 0.01333333"     
#>  [3] "Component “spec”: Mean relative difference: 0.0008888889"   
#>  [4] "Component “fart”: Mean relative difference: 0.002666667"    
#>  [5] "Component “PPV”: Mean relative difference: 0.002352941"     
#>  [6] "Component “FDR”: Mean relative difference: 0.002666667"     
#>  [7] "Component “NPV”: Mean relative difference: 0.0008888889"    
#>  [8] "Component “FOR”: Mean relative difference: 0.01333333"      
#>  [9] "Component “acc”: Mean relative difference: 0.001290323"     
#> [10] "Component “p_acc_hi”: Mean relative difference: 0.001063991"
#> [11] "Component “p_err_fa”: Mean relative difference: 0.001769912"
fe <- comp_freq(round = FALSE)     # compute exact freq (not rounded)
all.equal(prob, comp_prob_freq(fe$hi, fe$mi, fe$fa, fe$cr))  # is TRUE (qed).
#> [1] TRUE

## Explain by circular chain (compute prob 1. from num and 2. from freq)
# 0. Inspect current numeric parameters:
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

# 1. Compute currently 11 probabilities in prob (from essential probabilities):
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

# 2. Compute currently 11 frequencies in freq (from essential probabilities):
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

# 3. Compute currently 11 probabilities again (but now from frequencies):
prob_freq <- comp_prob_freq()
prob_freq
#> $prev
#> [1] 0.25
#> 
#> $sens
#> [1] 0.848
#> 
#> $mirt
#> [1] 0.152
#> 
#> $spec
#> [1] 0.7493333
#> 
#> $fart
#> [1] 0.2506667
#> 
#> $ppod
#> [1] 0.4
#> 
#> $PPV
#> [1] 0.53
#> 
#> $FDR
#> [1] 0.47
#> 
#> $NPV
#> [1] 0.9366667
#> 
#> $FOR
#> [1] 0.06333333
#> 
#> $acc
#> [1] 0.774
#> 
#> $p_acc_hi
#> [1] 0.2739018
#> 
#> $p_err_fa
#> [1] 0.8318584
#> 

# 4. Check equality of probabilities (in steps 1. and 3.):
all.equal(prob, prob_freq)  # => should be TRUE!
#>  [1] "Component “sens”: Mean relative difference: 0.002352941"    
#>  [2] "Component “mirt”: Mean relative difference: 0.01333333"     
#>  [3] "Component “spec”: Mean relative difference: 0.0008888889"   
#>  [4] "Component “fart”: Mean relative difference: 0.002666667"    
#>  [5] "Component “PPV”: Mean relative difference: 0.002352941"     
#>  [6] "Component “FDR”: Mean relative difference: 0.002666667"     
#>  [7] "Component “NPV”: Mean relative difference: 0.0008888889"    
#>  [8] "Component “FOR”: Mean relative difference: 0.01333333"      
#>  [9] "Component “acc”: Mean relative difference: 0.001290323"     
#> [10] "Component “p_acc_hi”: Mean relative difference: 0.001063991"
#> [11] "Component “p_err_fa”: Mean relative difference: 0.001769912"
```

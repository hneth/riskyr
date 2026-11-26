# A collection of riskyr scenarios from various sources (as list).

`scenarios` is a list of scenarios of class `riskyr` collected from the
scientific literature and other sources and to be used by visualization
and summary functions.

## Usage

``` r
scenarios
```

## Format

A list with currently 25 scenarios of class `riskyr` which are each
described by 21 variables.

## Details

`scenarios` currently contains the following scenarios (n1 to n12 in
English language, n13 to n25 in German language):

1.  Bowel cancer screening

2.  Cab problem

3.  Hemoccult test

4.  Mammography screening

5.  Mammography (freq)

6.  Mammography (prob)

7.  Mushrooms

8.  Musical town

9.  PSA test (baseline)

10. PSA test (patients)

11. Psylicraptis screening

12. Sepsis

13. Amniozentese (in German language)

14. HIV-Test 1

15. HIV-Test 2

16. HIV-Test 3

17. HIV-Test 4

18. Mammografie 1

19. Mammografie 2

20. Mammografie 3

21. Mammografie 4

22. Nackenfaltentest (NFT) 1

23. Nackenfaltentest (NFT) 2

24. Sigmoidoskopie 1

25. Sigmoidoskopie 2

Variables describing a scenario:

1.  `scen_lbl`: Text label for current scenario.

2.  `scen_lng`: Language of current scenario (en/de).

3.  `scen_txt`: Description text of current scenario.

4.  `popu_lbl`: Text label for current population.

5.  `cond_lbl`: Text label for current condition.

6.  `cond_true_lbl`: Text label for
    [`cond_true`](https://hneth.github.io/riskyr/dev/reference/cond_true.md)
    cases.

7.  `cond_false_lbl`: Text label for
    [`cond_false`](https://hneth.github.io/riskyr/dev/reference/cond_false.md)
    cases.

8.  `dec_lbl`: Text label for current decision.

9.  `dec_pos_lbl`: Text label for
    [`dec_pos`](https://hneth.github.io/riskyr/dev/reference/dec_pos.md)
    cases.

10. `dec_neg_lbl`: Text label for
    [`dec_neg`](https://hneth.github.io/riskyr/dev/reference/dec_neg.md)
    cases.

11. `hi_lbl`: Text label for cases of hits
    [`hi`](https://hneth.github.io/riskyr/dev/reference/hi.md).

12. `mi_lbl`: Text label for cases of misses
    [`mi`](https://hneth.github.io/riskyr/dev/reference/mi.md).

13. `fa_lbl`: Text label for cases of false alarms
    [`fa`](https://hneth.github.io/riskyr/dev/reference/fa.md).

14. `cr_lbl`: Text label for cases of correct rejections
    [`cr`](https://hneth.github.io/riskyr/dev/reference/cr.md).

15. `prev`: Value of current prevalence
    [`prev`](https://hneth.github.io/riskyr/dev/reference/prev.md).

16. `sens`: Value of current sensitivity
    [`sens`](https://hneth.github.io/riskyr/dev/reference/sens.md).

17. `spec`: Value of current specificity
    [`spec`](https://hneth.github.io/riskyr/dev/reference/spec.md).

18. `fart`: Value of current false alarm rate
    [`fart`](https://hneth.github.io/riskyr/dev/reference/fart.md).

19. `N`: Current population size
    [`N`](https://hneth.github.io/riskyr/dev/reference/N.md).

20. `scen_src`: Source information for current scenario.

21. `scen_apa`: Source information in APA format.

Note that names of variables (columns) correspond to a subset of
[`init_txt`](https://hneth.github.io/riskyr/dev/reference/init_txt.md)
(to initialize
[`txt`](https://hneth.github.io/riskyr/dev/reference/txt.md)) and
[`init_num`](https://hneth.github.io/riskyr/dev/reference/init_num.md)
(to initialize
[`num`](https://hneth.github.io/riskyr/dev/reference/num.md)).

The variables `scen_src` and `scen_apa` provide a scenario's source
information.

The information of `scenarios` is also contained in an R data frame
[`df_scenarios`](https://hneth.github.io/riskyr/dev/reference/df_scenarios.md)
(and generated from the corresponding `.rda` file in `/data/`).

## See also

[`riskyr`](https://hneth.github.io/riskyr/dev/reference/riskyr.md)
initializes a `riskyr` scenario.

# riskyr

A toolbox for teaching risk literacy.

**Goal:** Develop and assemble a set of basic risk literacy tools in R

We start with some global variables:

- population size _N_
- prevalence _prev_ of some condition
- sensitivity _sens_ of some test (or probability of treatment success)
- specificity _spec_ of some test (or probability of side effects)

and provide a variety of perspectives on the consequences of and interplay between these variables:

1. tree of natural frequencies
2. icon array (with population vs. sample view)
3. contingency table (leaves of the frequency tree)
4. curves of PPV and NPV (as a function of prevalence or sensitivity/specificity)
5. fact boxes (with additional details on benefits and harms)

All visualizations will be interactive and use a common color scheme.

A set of examples illustrates cases with known data from the literature.
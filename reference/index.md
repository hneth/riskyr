# Package index

## Initialization

Creating riskyr scenarios and corresponding parameter collections.

- [`FFTrees_riskyr()`](https://hneth.github.io/riskyr/reference/FFTrees_riskyr.md)
  : Convert from FFTrees to riskyr objects.
- [`plot(`*`<riskyr>`*`)`](https://hneth.github.io/riskyr/reference/plot.riskyr.md)
  : Plot a riskyr scenario.
- [`print(`*`<summary.riskyr>`*`)`](https://hneth.github.io/riskyr/reference/print.summary.riskyr.md)
  : Print summary information of a riskyr scenario.
- [`riskyr()`](https://hneth.github.io/riskyr/reference/riskyr.md) :
  Create a riskyr scenario.
- [`riskyr.guide()`](https://hneth.github.io/riskyr/reference/riskyr.guide.md)
  : Opens the riskyr package guides
- [`summary(`*`<riskyr>`*`)`](https://hneth.github.io/riskyr/reference/summary.riskyr.md)
  : Summarize a riskyr scenario.
- [`df_scenarios`](https://hneth.github.io/riskyr/reference/df_scenarios.md)
  : A collection of riskyr scenarios from various sources (as df).
- [`scenarios`](https://hneth.github.io/riskyr/reference/scenarios.md) :
  A collection of riskyr scenarios from various sources (as list).
- [`init_num()`](https://hneth.github.io/riskyr/reference/init_num.md) :
  Initialize basic numeric variables.
- [`init_pal()`](https://hneth.github.io/riskyr/reference/init_pal.md) :
  Initialize basic color information.
- [`init_txt()`](https://hneth.github.io/riskyr/reference/init_txt.md) :
  Initialize basic text elements.
- [`num`](https://hneth.github.io/riskyr/reference/num.md) : List
  current values of basic numeric variables.
- [`popu`](https://hneth.github.io/riskyr/reference/popu.md) : A table
  of cases (in the current population).
- [`read_popu()`](https://hneth.github.io/riskyr/reference/read_popu.md)
  : Read population data (from df) into a riskyr scenario (description).
- [`pal`](https://hneth.github.io/riskyr/reference/pal.md) : List
  current values of scenario color palette.
- [`pal_bw`](https://hneth.github.io/riskyr/reference/pal_bw.md) :
  Alternative color palette for black-and-white (greyscale) graphs.
- [`pal_bwp`](https://hneth.github.io/riskyr/reference/pal_bwp.md) :
  Alternative color palette for black-and-white graphs (for printing
  purposes).
- [`pal_crisk`](https://hneth.github.io/riskyr/reference/pal_crisk.md) :
  Color palette for cumulative risk curve.
- [`pal_kn`](https://hneth.github.io/riskyr/reference/pal_kn.md) :
  Alternative color palette for kn.
- [`pal_mbw`](https://hneth.github.io/riskyr/reference/pal_mbw.md) :
  Modern and reduced color palette (in green/blue/bw).
- [`pal_mod`](https://hneth.github.io/riskyr/reference/pal_mod.md) :
  Modern color palette (in green/blue/orange).
- [`pal_org`](https://hneth.github.io/riskyr/reference/pal_org.md) :
  Original color palette.
- [`pal_rgb`](https://hneth.github.io/riskyr/reference/pal_rgb.md) :
  Alternative color palette for graphs (with RGB colors).
- [`pal_unikn`](https://hneth.github.io/riskyr/reference/pal_unikn.md) :
  Alternative color palette for unikn.
- [`pal_vir`](https://hneth.github.io/riskyr/reference/pal_vir.md) :
  Alternative color palette using viridis colors.
- [`txt`](https://hneth.github.io/riskyr/reference/txt.md) : Basic text
  elements.
- [`txt_TF`](https://hneth.github.io/riskyr/reference/txt_TF.md) :
  Alternative text labels (TP, FN, FP, TN).
- [`txt_org`](https://hneth.github.io/riskyr/reference/txt_org.md) :
  List of original values of text elements.

## Visualization

Functions for creating graphical representations of riskyr scenarios.

- [`plot(`*`<box>`*`)`](https://hneth.github.io/riskyr/reference/plot.box.md)
  : Plot a frequency box object
- [`plot(`*`<riskyr>`*`)`](https://hneth.github.io/riskyr/reference/plot.riskyr.md)
  : Plot a riskyr scenario.
- [`plot_area()`](https://hneth.github.io/riskyr/reference/plot_area.md)
  : Plot an area diagram of probabilities or frequencies.
- [`plot_bar()`](https://hneth.github.io/riskyr/reference/plot_bar.md) :
  Plot bar charts of population frequencies.
- [`plot_cbar()`](https://hneth.github.io/riskyr/reference/plot_cbar.md)
  : Plot cumulative risk dynamics (as bar chart)
- [`plot_crisk()`](https://hneth.github.io/riskyr/reference/plot_crisk.md)
  : Plot a cumulative risk curve.
- [`plot_curve()`](https://hneth.github.io/riskyr/reference/plot_curve.md)
  : Plot curves of selected values (e.g., PPV or NPV) as a function of
  prevalence.
- [`plot_fnet()`](https://hneth.github.io/riskyr/reference/plot_fnet.md)
  : Plot frequency net diagram of frequencies and probabilities.
- [`plot_icons()`](https://hneth.github.io/riskyr/reference/plot_icons.md)
  : Plot an icon array of a population.
- [`plot_mosaic()`](https://hneth.github.io/riskyr/reference/plot_mosaic.md)
  : Plot a mosaic plot of population frequencies.
- [`plot_plane()`](https://hneth.github.io/riskyr/reference/plot_plane.md)
  : Plot a plane of selected values (e.g., PPV or NPV) as a function of
  sensitivity and specificity.
- [`plot_prism()`](https://hneth.github.io/riskyr/reference/plot_prism.md)
  : Plot prism diagram of frequencies and probabilities.
- [`plot_tab()`](https://hneth.github.io/riskyr/reference/plot_tab.md) :
  Plot a 2 x 2 contingency table of population frequencies.
- [`plot_tree()`](https://hneth.github.io/riskyr/reference/plot_tree.md)
  : Plot a tree diagram of frequencies and probabilities.

## Probabilities

Lists and objects representing probability information.

- [`prob`](https://hneth.github.io/riskyr/reference/prob.md) : List
  current probability information.
- [`prev`](https://hneth.github.io/riskyr/reference/prev.md) : The
  prevalence (baseline probability or prior) of a condition.
- [`sens`](https://hneth.github.io/riskyr/reference/sens.md) : The
  sensitivity (or hit rate) of a decision process or diagnostic
  procedure.
- [`mirt`](https://hneth.github.io/riskyr/reference/mirt.md) : The miss
  rate of a decision process or diagnostic procedure.
- [`spec`](https://hneth.github.io/riskyr/reference/spec.md) : The
  specificity of a decision process or diagnostic procedure.
- [`fart`](https://hneth.github.io/riskyr/reference/fart.md) : The false
  alarm rate (or false positive rate) of a decision process or
  diagnostic procedure.
- [`ppod`](https://hneth.github.io/riskyr/reference/ppod.md) : The
  proportion (or baseline) of a positive decision (aka. bias).
- [`PPV`](https://hneth.github.io/riskyr/reference/PPV.md) : The
  positive predictive value of a decision process or diagnostic
  procedure.
- [`NPV`](https://hneth.github.io/riskyr/reference/NPV.md) : The
  negative predictive value of a decision process or diagnostic
  procedure.
- [`FDR`](https://hneth.github.io/riskyr/reference/FDR.md) : The false
  detection rate of a decision process or diagnostic procedure.
- [`FOR`](https://hneth.github.io/riskyr/reference/FOR.md) : The false
  omission rate (FOR) of a decision process or diagnostic procedure.
- [`acc`](https://hneth.github.io/riskyr/reference/acc.md) : Accuracy
  (acc) is the probability of a correct decision.
- [`accu`](https://hneth.github.io/riskyr/reference/accu.md) : A list
  containing current accuracy information.
- [`err`](https://hneth.github.io/riskyr/reference/err.md) : Error rate
  (err) as the probability of an incorrect decision.

## Frequencies

Lists and objects representing frequencies (i.e., various subsets of a
population).

- [`freq`](https://hneth.github.io/riskyr/reference/freq.md) : List
  current frequency information.
- [`N`](https://hneth.github.io/riskyr/reference/N.md) : Number of
  individuals in the population.
- [`cond_false`](https://hneth.github.io/riskyr/reference/cond_false.md)
  : Number of individuals for which the condition is false.
- [`cond_true`](https://hneth.github.io/riskyr/reference/cond_true.md) :
  Number of individuals for which the condition is true.
- [`dec_cor`](https://hneth.github.io/riskyr/reference/dec_cor.md) :
  Number of individuals for which the decision is correct.
- [`dec_err`](https://hneth.github.io/riskyr/reference/dec_err.md) :
  Number of individuals for which the decision is erroneous.
- [`dec_neg`](https://hneth.github.io/riskyr/reference/dec_neg.md) :
  Number of individuals for which the decision is negative.
- [`dec_pos`](https://hneth.github.io/riskyr/reference/dec_pos.md) :
  Number of individuals for which the decision is positive.
- [`hi`](https://hneth.github.io/riskyr/reference/hi.md) : Frequency of
  hits or true positives (TP).
- [`mi`](https://hneth.github.io/riskyr/reference/mi.md) : Frequency of
  misses or false negatives (FN).
- [`fa`](https://hneth.github.io/riskyr/reference/fa.md) : Frequency of
  false alarms or false positives (FP).
- [`cr`](https://hneth.github.io/riskyr/reference/cr.md) : Frequency of
  correct rejections or true negatives (TN).

## Computation and conversion

Computing parameters and translating between representational formats
and data/descriptions.

- [`comp_FDR()`](https://hneth.github.io/riskyr/reference/comp_FDR.md) :
  Compute a decision's false detection rate (FDR) from probabilities.
- [`comp_FOR()`](https://hneth.github.io/riskyr/reference/comp_FOR.md) :
  Compute a decision's false omission rate (FOR) from probabilities.
- [`comp_NPV()`](https://hneth.github.io/riskyr/reference/comp_NPV.md) :
  Compute a decision's negative predictive value (NPV) from
  probabilities.
- [`comp_PPV()`](https://hneth.github.io/riskyr/reference/comp_PPV.md) :
  Compute a decision's positive predictive value (PPV) from
  probabilities.
- [`comp_acc()`](https://hneth.github.io/riskyr/reference/comp_acc.md) :
  Compute overall accuracy (acc) from probabilities.
- [`comp_accu_freq()`](https://hneth.github.io/riskyr/reference/comp_accu_freq.md)
  : Compute accuracy metrics of current classification results.
- [`comp_accu_prob()`](https://hneth.github.io/riskyr/reference/comp_accu_prob.md)
  : Compute exact accuracy metrics based on probabilities.
- [`comp_comp_pair()`](https://hneth.github.io/riskyr/reference/comp_comp_pair.md)
  : Compute a probability's (missing) complement and return both.
- [`comp_complement()`](https://hneth.github.io/riskyr/reference/comp_complement.md)
  : Compute a probability's complement probability.
- [`comp_complete_prob_set()`](https://hneth.github.io/riskyr/reference/comp_complete_prob_set.md)
  : Compute a complete set of probabilities from valid probability
  inputs.
- [`comp_err()`](https://hneth.github.io/riskyr/reference/comp_err.md) :
  Compute overall error rate (err) from probabilities.
- [`comp_fart()`](https://hneth.github.io/riskyr/reference/comp_fart.md)
  : Compute a decision's false alarm rate from its specificity.
- [`comp_freq()`](https://hneth.github.io/riskyr/reference/comp_freq.md)
  : Compute frequencies from (3 essential) probabilities.
- [`comp_freq_freq()`](https://hneth.github.io/riskyr/reference/comp_freq_freq.md)
  : Compute frequencies from (4 essential) frequencies.
- [`comp_freq_prob()`](https://hneth.github.io/riskyr/reference/comp_freq_prob.md)
  : Compute frequencies from (3 essential) probabilities.
- [`comp_min_N()`](https://hneth.github.io/riskyr/reference/comp_min_N.md)
  : Compute a suitable minimum population size value N.
- [`comp_mirt()`](https://hneth.github.io/riskyr/reference/comp_mirt.md)
  : Compute a decision's miss rate from its sensitivity.
- [`comp_popu()`](https://hneth.github.io/riskyr/reference/comp_popu.md)
  : Compute a population table (data) from frequencies (description).
- [`comp_ppod()`](https://hneth.github.io/riskyr/reference/comp_ppod.md)
  : Compute the proportion of positive decisions (ppod) from
  probabilities.
- [`comp_prev()`](https://hneth.github.io/riskyr/reference/comp_prev.md)
  : Compute the condition's prevalence (baseline probability) from
  frequencies.
- [`comp_prob()`](https://hneth.github.io/riskyr/reference/comp_prob.md)
  : Compute probabilities from (3 essential) probabilities.
- [`comp_prob_freq()`](https://hneth.github.io/riskyr/reference/comp_prob_freq.md)
  : Compute probabilities from (4 essential) frequencies.
- [`comp_prob_prob()`](https://hneth.github.io/riskyr/reference/comp_prob_prob.md)
  : Compute probabilities from (3 essential) probabilities.
- [`comp_sens()`](https://hneth.github.io/riskyr/reference/comp_sens.md)
  : Compute a decision's sensitivity from its miss rate.
- [`comp_spec()`](https://hneth.github.io/riskyr/reference/comp_spec.md)
  : Compute a decision's specificity from its false alarm rate.
- [`read_popu()`](https://hneth.github.io/riskyr/reference/read_popu.md)
  : Read population data (from df) into a riskyr scenario (description).
- [`write_popu()`](https://hneth.github.io/riskyr/reference/write_popu.md)
  : Write a population table (data) from a riskyr scenario
  (description).

## Datasets

Data used in examples and illustrations.

- [`df_scenarios`](https://hneth.github.io/riskyr/reference/df_scenarios.md)
  : A collection of riskyr scenarios from various sources (as df).
- [`BRCA1`](https://hneth.github.io/riskyr/reference/BRCA1.md) :
  Cumulative risk of breast cancer in women with the BRCA1 mutation.
- [`BRCA1_mam`](https://hneth.github.io/riskyr/reference/BRCA1_mam.md) :
  Cumulative risk of breast cancer in women with the BRCA1 mutation.
- [`BRCA1_ova`](https://hneth.github.io/riskyr/reference/BRCA1_ova.md) :
  Cumulative risk of ovarian cancer in women with the BRCA1 mutation.
- [`BRCA2`](https://hneth.github.io/riskyr/reference/BRCA2.md) :
  Cumulative risk of breast cancer in women with the BRCA2 mutation.
- [`BRCA2_mam`](https://hneth.github.io/riskyr/reference/BRCA2_mam.md) :
  Cumulative risk of breast cancer in women with the BRCA2 mutation.
- [`BRCA2_ova`](https://hneth.github.io/riskyr/reference/BRCA2_ova.md) :
  Cumulative risk of ovarian cancer in women with the BRCA2 mutation.
- [`t_A`](https://hneth.github.io/riskyr/reference/t_A.md) : Cumulative
  risk curve A (main/transfer task A).
- [`t_B`](https://hneth.github.io/riskyr/reference/t_B.md) : Cumulative
  risk curve B (main/transfer task B).
- [`t_I`](https://hneth.github.io/riskyr/reference/t_I.md) : Cumulative
  risk curve I (introductory task).

## Validation and formatting

Verifying and displaying representational formats.

- [`is_complement()`](https://hneth.github.io/riskyr/reference/is_complement.md)
  : Verify that two numbers are complements.
- [`is_extreme_prob_set()`](https://hneth.github.io/riskyr/reference/is_extreme_prob_set.md)
  : Verify that a set of probabilities describes an extreme case.
- [`is_freq()`](https://hneth.github.io/riskyr/reference/is_freq.md) :
  Verify that input is a frequency (positive integer value).
- [`is_integer()`](https://hneth.github.io/riskyr/reference/is_integer.md)
  : Test for inters (i.e., whole numbers).
- [`is_matrix()`](https://hneth.github.io/riskyr/reference/is_matrix.md)
  : Verify a 2x2 matrix as a numeric contingency table.
- [`is_perc()`](https://hneth.github.io/riskyr/reference/is_perc.md) :
  Verify that input is a percentage (numeric value from 0 to 100).
- [`is_prob()`](https://hneth.github.io/riskyr/reference/is_prob.md) :
  Verify that input is a probability (numeric value from 0 to 1).
- [`is_suff_prob_set()`](https://hneth.github.io/riskyr/reference/is_suff_prob_set.md)
  : Verify a sufficient set of probability inputs.
- [`is_valid_freq_set()`](https://hneth.github.io/riskyr/reference/is_valid_freq_set.md)
  : Verify a set of 4 essential frequencies.
- [`is_valid_prob_pair()`](https://hneth.github.io/riskyr/reference/is_valid_prob_pair.md)
  : Verify that a pair of probability inputs can be a pair of
  complementary probabilities.
- [`is_valid_prob_set()`](https://hneth.github.io/riskyr/reference/is_valid_prob_set.md)
  : Verify that a set of probability inputs is valid.
- [`is_valid_prob_triple()`](https://hneth.github.io/riskyr/reference/is_valid_prob_triple.md)
  : Verify that a triple of essential probability inputs is valid.
- [`as_pb()`](https://hneth.github.io/riskyr/reference/as_pb.md) :
  Display a percentage as a (numeric and rounded) probability.
- [`as_pc()`](https://hneth.github.io/riskyr/reference/as_pc.md) :
  Display a probability as a (numeric and rounded) percentage.

## Extensions

Extensions and links to other packages.

- [`FFTrees_riskyr()`](https://hneth.github.io/riskyr/reference/FFTrees_riskyr.md)
  : Convert from FFTrees to riskyr objects.

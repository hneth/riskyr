## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# URLs:
url_riskyr_org <- "https://riskyr.org/" # WAS: "http://134.34.54.61:3838/spds/riskyr/"

## ----comp-PPV, messages = FALSE, warning = FALSE------------------------------
library("riskyr")  # loads the package

comp_PPV(prev = .01, sens = .80, spec = (1 - .096))

## ----comp_prob_prob_1---------------------------------------------------------
# Compute probabilities from 3 essential probabilities:                 # Input arguments:
p1 <- comp_prob_prob(prev = .01, sens = .80, spec =   NA, fart = .096)  # prev, sens, NA,   fart
p2 <- comp_prob_prob(prev = .01, sens = .80, spec = .904, fart =   NA)  # prev, sens, spec, NA 
p3 <- comp_prob_prob(prev = .01, sens = .80, spec = .904, fart = .096)  # prev, sens, spec, fart

# Check equality of outputs:
all.equal(p1, p2)
all.equal(p2, p3)

## ----print-p1-----------------------------------------------------------------
unlist(p1)

## ----comp-prob-prob-2---------------------------------------------------------
# Compute probabilities from 3 ratios of frequencies (probabilities):       # Input arguments:
p4 <- comp_prob_prob(prev = 10/1000, sens = 8/10, spec = NA, fart = 95/990) # prev, sens, NA, fart

p4$PPV

## ----comp-freq-prob-1---------------------------------------------------------
# Compute frequencies from probabilities:
f1 <- comp_freq_prob(prev =     .01, sens =  .80, spec = NA, fart =   .096, N = 1000)
f2 <- comp_freq_prob(prev = 10/1000, sens = 8/10, spec = NA, fart = 95/990, N = 1000)

# Check equality of outputs:
all.equal(f1, f2)

## ----comp-freq-prob-2, eval = FALSE-------------------------------------------
#  # Compute frequencies from probabilities (without rounding):
#  f3 <- comp_freq_prob(prev =     .01, sens =  .80, spec = NA, fart =   .096, N = 1000, round = FALSE)
#  f4 <- comp_freq_prob(prev = 10/1000, sens = 8/10, spec = NA, fart = 95/990, N = 1000, round = FALSE)
#  
#  ## Check equality of outputs:
#  all.equal(f3, f4)  # => shows slight differences in some frequencies:

## ----print-f1-----------------------------------------------------------------
unlist(f1)

## ----comp-prob-freq-----------------------------------------------------------
# Compute probabilities from frequencies:
p5 <- comp_prob_freq(hi = 8, mi = 2, fa = 95, cr = 895)  # => provide 4 essential frequencies

## ----equality-p4p5, eval = FALSE----------------------------------------------
#  # Check equality of outputs:
#  all.equal(p5, p4)

## ----full-circle, eval = FALSE------------------------------------------------
#  # Pick 3 random probability inputs:
#  rand.p <- runif(n = 3, min = 0, max = 1)
#  rand.p
#  
#  # Translation 1: Compute frequencies from probabilities (without rounding):
#  freq <- comp_freq_prob(prev = rand.p[1], sens = rand.p[2], spec = rand.p[3], round = FALSE)
#  
#  # Translation 2: Compute probabilities from frequencies:
#  prob <- comp_prob_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)
#  
#  ## Verify that results match original probabilities:
#  all.equal(prob$prev, rand.p[1])
#  all.equal(prob$sens, rand.p[2])
#  all.equal(prob$spec, rand.p[3])

## ----full_circle_2, eval = FALSE----------------------------------------------
#  # Pick 4 random frequencies:
#  rand.f <- round(runif(n = 4, min = 0, max = 10^3), 0)
#  rand.f
#  # sum(rand.f)
#  
#  # Translation 1: Compute probabilities from frequencies:
#  prob <- comp_prob_freq(hi = rand.f[1], mi = rand.f[2], fa = rand.f[3], cr = rand.f[4])
#  # prob
#  
#  # Translation 2: Compute frequencies from probabilities (for original N, without rounding):
#  freq <- comp_freq_prob(prev = prob$prev, sens = prob$sens, spec = prob$spec,
#                         N = sum(rand.f), round = FALSE)
#  # freq
#  
#  # Verify that results match original frequencies:
#  all.equal(freq$hi, rand.f[1])
#  all.equal(freq$mi, rand.f[2])
#  all.equal(freq$fa, rand.f[3])
#  all.equal(freq$cr, rand.f[4])

## ----riskyr_scenario_def------------------------------------------------------
s <- riskyr(scen_lbl = "Mammography screening", 
            cond_lbl = "breast cancer",
            cond_true_lbl = "cancer", cond_false_lbl = "no cancer",
            dec_lbl = "screening test",
            dec_pos_lbl = "predict cancer", dec_neg_lbl = "predict no cancer",
            prev = .01, 
            sens = .80, 
            spec = NA, 
            fart = .096)

## ----riskyr-scenario-summary--------------------------------------------------
summary(s)  # provides an overview over key scenario information:

## ----riskyr-scenario-plot, fig.width = 6, fig.height = 5----------------------
plot(s)  # a prism/network diagram of key frequencies and probabilities (by default):

## ----plot-icons-1, warning = FALSE, fig.width = 6, fig.height = 4, fig.show = 'hold', fig.cap = "An icon array showing the mammography scenario for a population of 1000 individuals."----
plot_icons(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000, 
           icon_types = c(21, 21, 22, 22),
           title_lbl = "Mammography screening")

## ----plot-tree-cd, fig.width = 6, fig.height = 4, fig.show = 'hold', fig.cap = "A tree diagram that applies the provided probabilities and frequencies to a population of 1000 individuals."----
plot_prism(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000, 
           by = "cd", title_lbl = "Mammography screening")

## ----plot-mosaic-cd, eval = TRUE, fig.align = "center", fig.width = 6, fig.height = 5, fig.show = 'hold', fig.cap = "An area plot in which area sizes represent the probabilities/relative frequencies of subgroups."----
plot_area(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000,
          title_lbl = "Mammography screening")

## ----plot-bar, eval = FALSE, fig.width = 6, fig.height = 5, fig.show = 'hold', fig.cap = "A bar plot."----
#  plot_bar(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000,
#           by = "all", dir = 2, title_lbl = "Mammography screening")

## ----plot-prism, fig.width = 6, fig.height = 5, fig.show = 'hold', fig.cap = "A prism plot that integrates 2 tree diagrams and represents relative frequency by area size."----
plot_prism(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000, 
           by = "cddc", area = "sq", title_lbl = "Mammography screening")

## ----plot-tree-dc, eval = FALSE, fig.width = 6, fig.height = 4.5, fig.show = 'hold', fig.cap = "Alternative tree diagram that splits the population by decision."----
#  plot_prism(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000,
#             by = "dc", title_lbl = "Mammography screening", col_pal = pal_mod)

## ----plot-tree-ac, eval = FALSE, fig.width = 6, fig.height = 4.5, fig.show = 'hold', fig.cap = "Alternative tree diagram that splits the population by accuracy"----
#  plot_prism(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000,
#             by = "ac", title_lbl = "Mammography screening")

## ----plot-mosaic-dc, eval = FALSE, fig.align = "center", fig.width = 6, fig.height = 4.5, fig.show = 'hold', fig.cap = "Alternative mosaic plot that first splits the population (horizontally) by decision."----
#  plot_area(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000,
#            by = "cddc",    # show by condition x decision
#            p_split = "h",  # horizontal perspective is primary
#            title_lbl = "Mammography screening")

## ----plot-tree-dc-vr, eval = FALSE, fig.width = 6, fig.height = 4.5, fig.show = 'asis', fig.cap = "A prism diagram that represents relative frequencies as the width of horizontal rectangles."----
#  plot_prism(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000,
#             by = "dc", area = "hr", title_lbl = "Mammography screening")

## ----plot-icons-mosaic, eval = FALSE, warning = FALSE, fig.width = 6, fig.height = 4.5, fig.show = 'asis', fig.cap = "An icon array showing the mammography scenario for 1000 mosaic puzzle parts."----
#  plot_icons(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000,
#             arr_type = "mosaic", icon_types = c(21, 21, 22, 22), icon_size = 2,
#             title_lbl = "Mammography screening")


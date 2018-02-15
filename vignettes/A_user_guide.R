## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----comp_PPV------------------------------------------------------------
library("riskyr")  # loads the package

comp_PPV(prev = .01, sens = .80, spec = (1 - .096))

## ----comp_prob_prob_1----------------------------------------------------
# Compute probabilities from 3 essential probabilities:                 # Input arguments:
p1 <- comp_prob_prob(prev = .01, sens = .80, spec =   NA, fart = .096)  # prev, sens, NA,   fart
p2 <- comp_prob_prob(prev = .01, sens = .80, spec = .904, fart =   NA)  # prev, sens, spec, NA 
p3 <- comp_prob_prob(prev = .01, sens = .80, spec = .904, fart = .096)  # prev, sens, spec, fart

# Check equality of outputs:
all.equal(p1, p2)
all.equal(p2, p3)

## ----print_p1------------------------------------------------------------
p1

## ----comp_prob_prob_2----------------------------------------------------
# Compute probabilities from 3 ratios of frequencies (probabilities):       # Input arguments:
p4 <- comp_prob_prob(prev = 10/1000, sens = 8/10, spec = NA, fart = 95/990) # prev, sens, NA, fart

p4$PPV

## ----comp_freq_prob_1----------------------------------------------------
# Compute frequencies from probabilities:
f1 <- comp_freq_prob(prev =     .01, sens =  .80, spec = NA, fart =   .096, N = 1000)
f2 <- comp_freq_prob(prev = 10/1000, sens = 8/10, spec = NA, fart = 95/990, N = 1000)

# Check equality of outputs:
all.equal(f1, f2)

## ----comp_freq_prob_2----------------------------------------------------
# Compute frequencies from probabilities (without rounding):
f3 <- comp_freq_prob(prev =     .01, sens =  .80, spec = NA, fart =   .096, N = 1000, round = FALSE)
f4 <- comp_freq_prob(prev = 10/1000, sens = 8/10, spec = NA, fart = 95/990, N = 1000, round = FALSE)

# Check equality of outputs:
all.equal(f3, f4)  # => shows slight differences in some frequencies:

## ----print_f1------------------------------------------------------------
f1

## ----comp_prob_freq------------------------------------------------------
# Compute probabilities from frequencies:
p5 <- comp_prob_freq(hi = 8, mi = 2, fa = 95, cr = 895)  # => provide 4 essential frequencies

## ------------------------------------------------------------------------
# Check equality of outputs:
all.equal(p5, p4)

## ----full_circle---------------------------------------------------------
# Pick 3 random probability inputs:
rand.p <- runif(n = 3, min = 0, max = 1)
rand.p

# Translation 1: Compute frequencies from probabilities:
freq <- comp_freq_prob(prev = rand.p[1], sens = rand.p[2], spec = rand.p[3], round = FALSE)  # without rounding!

# Translation 2: Compute probabilities from frequencies:
prob <- comp_prob_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)

# Verify that results match original probabilities: 
all.equal(prob$prev, rand.p[1])
all.equal(prob$sens, rand.p[2])
all.equal(prob$spec, rand.p[3])

## ----full_circle_2-------------------------------------------------------
# Pick 4 random frequencies:
rand.f <- round(runif(n = 4, min = 0, max = 10^3), 0)
rand.f  
# sum(rand.f)

# Translation 1: Compute probabilities from frequencies:
prob <- comp_prob_freq(hi = rand.f[1], mi = rand.f[2], fa = rand.f[3], cr = rand.f[4])
# prob

# Translation 2: Compute frequencies from probabilities (for the original population size N):
freq <- comp_freq_prob(prev = prob$prev, sens = prob$sens, spec = prob$spec, N = sum(rand.f), round = FALSE)  # without rounding!
# freq

# Verify that results match original frequencies: 
all.equal(freq$hi, rand.f[1])
all.equal(freq$mi, rand.f[2])
all.equal(freq$fa, rand.f[3])
all.equal(freq$cr, rand.f[4])

## ----plot_icons_1, warning = FALSE, fig.width = 7.2, fig.height = 4.2, fig.show = 'asis', fig.cap = "An icon array showing the mammography scenario for a population of 1000 individuals."----
plot_icons(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000, 
           icon.types = c(21, 21, 22, 22),
           title.lbl = "Mammography screening")

## ----plot_tree_cd, fig.width = 7.2, fig.height = 5.5, fig.show = 'asis', fig.cap = "A tree diagram that applies the provided probabilities and frequencies to a population of 1000 individuals."----
plot_tree(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000, 
          title.lbl = "Mammography screening")

## ----plot_mosaic_cd, fig.width = 5, fig.height = 5, fig.show = 'asis', fig.cap = "A mosaic plot in which area sizes represent the relative frequencies of subgroups."----
plot_mosaic(prev = .01, sens = .80, spec =   NA, fart = .096, N = 1000,
            title.lbl = "Mammography screening")

## ----plot_tree_dc, fig.width = 7.2, fig.height = 5.5, fig.show = 'asis', fig.cap = "Alternative tree diagram that first splits the population by decision."----
plot_tree(prev = .01, sens = .80, spec =   NA, fart = .096, N = 1000, 
          by = "dc", 
          title.lbl = "Mammography screening",
          dec.pos.lbl = "positive test",
          dec.neg.lbl = "negative test")

## ----plot_mosaic_dc, fig.width = 5, fig.height = 5, fig.show = 'asis', fig.cap = "Alternative mosaic plot that first splits the population (horizontally) by decision."----
plot_mosaic(prev = .01, sens = .80, spec =   NA, fart = .096, N = 1000,
            vsplit = FALSE, 
            title.lbl = "Mammography screening")

## ----plot_tree_dc_vr, fig.width = 7.2, fig.height = 5.5, fig.show = 'asis', fig.cap = "A tree diagram that represents relative frequencies by area sizes."----
plot_tree(prev = .01, sens = .80, spec =   NA, fart = .096, N = 1000, 
          by = "dc",
          area = "hr", 
          title.lbl = "Mammography screening",
          dec.pos.lbl = "positive test",
          dec.neg.lbl = "negative test")

## ----plot_icons_mosaic, warning = FALSE, fig.width = 7.2, fig.height = 5.5, fig.show = 'asis', fig.cap = "An icon array showing the mammography scenario for a population of 1000 individuals."----
plot_icons(prev = .01, sens = .80, spec = NA, fart = .096, N = 1000, block.d = 0.01,
           type = "mosaic",
           icon.types = c(21, 21, 22, 22),
           title.lbl = "Mammography screening")

## ----plot_fnet, fig.width = 7.2, fig.height = 7.2, fig.show = 'asis', fig.cap = "A network diagram that integrates two tree diagrams and represents relative frequencies by area sizes."----
plot_fnet(prev = .01, sens = .80, spec =   NA, fart = .096, N = 1000, 
          title.lbl = "Mammography screening")


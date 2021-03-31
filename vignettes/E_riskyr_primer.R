## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# URLs:
url_riskyr_org <- "https://riskyr.org/" # WAS: "http://134.34.54.61:3838/spds/riskyr/"

# Load pkg:
library("riskyr")

# init:
op <- par(no.readonly = TRUE)

## ----load-riskyr, message = FALSE---------------------------------------------
library("riskyr")  # loads the package

## ----create-scenario-minimal-prob---------------------------------------------
# Create a minimal scenario (from probabilities):
my_scenario <- riskyr(prev = .45, 
                      sens = .98,
                      spec = .46)

## ----create-scenario-min-freq-------------------------------------------------
# Create a minimal scenario (from frequencies):
my_scenario_2 <- riskyr(hi = my_scenario$hi, 
                        mi = my_scenario$mi,
                        fa = my_scenario$fa,
                        cr = my_scenario$cr)

## ----verify-equal-prob-freq---------------------------------------------------
all.equal(my_scenario, my_scenario_2)

## ----create-scenario-custom---------------------------------------------------
# Create a scenario with custom labels: 
my_scenario <- riskyr(scen_lbl = "Identifying reoffenders", 
                      popu_lbl = "prison inmates", 
                      cond_lbl = "reoffending",
                      cond_true_lbl = "reoffends", cond_false_lbl = "does not reoffend",
                      dec_lbl = "test result",
                      dec_pos_lbl = "predict to\nreoffend", dec_neg_lbl = "predict to\nnot reoffend",
                      hi_lbl = "reoffender found", mi_lbl = "reoffender missed",
                      fa_lbl = "false accusation", cr_lbl = "correct release",
                      prev = .45,  # prevalence of being a reoffender. 
                      sens = .98,  # p( will reoffend | offends again )
                      spec = .46,  # p( will not reoffend | does not offend again )
                      fart =  NA,  # p( will reoffend | does not offend gain )
                      N = 753,     # population size
                      scen_src = "(a ficticious example)")

## ----plot-default, include = TRUE, fig.align = "center", fig.width = 6, fig.height = 5----
plot(my_scenario)

## ----plot-area-sq, eval = FALSE, fig.align = "center", fig.width = 6, fig.height = 5----
#  plot(my_scenario, area = "sq", f_lbl = "nam", p_lbl = "mix")  # show frequency names
#  plot(my_scenario, area = "hr", f_lbl = "num", p_lbl = "num")  # only numeric labels

## ----prism-practice, echo = FALSE, eval = FALSE, fig.align = "center"---------
#  plot(my_scenario, area = "hr")
#  plot(my_scenario, area = "no", by = "cdac")
#  plot(my_scenario, area = "hr", by = "acdc", f_lbl = "nam", p_lbl = "num", f_lwd = .5, col_pal = pal_bw)

## ----icons, fig.align = "center", fig.width = 5.5, fig.height = 3.5-----------
plot(my_scenario, type = "icons")

## ----full-summary-------------------------------------------------------------
summary(my_scenario)

## ----summary-prob, include = FALSE--------------------------------------------
summary(my_scenario, summarize = "prob")

## ----tree-plot, fig.align = "center", fig.width = 6, fig.height = 3.5---------
plot(my_scenario, type = "tree", by = "dc")  # plot tree diagram (splitting N by decision)

## ----plotting-curve, fig.align = "center", fig.width = 5.5, fig.height = 4.5----
plot(my_scenario, type = "curve", uc = .05)

## ----scenario-table, echo = FALSE, results = "asis"---------------------------
library(knitr)
scen_table <- df_scenarios[, c("scen_lbl", "cond_lbl", "N", "prev",
                               "sens", "spec", "fart")]
scen_table[, -c(1:2)] <- round(scen_table[, -c(1:2)], 3)
names(scen_table) <- c("Scenario", "Condition", "N", "prev", "sens", "spec", "fart")
knitr::kable(scen_table)

## ----s10-select---------------------------------------------------------------
s10 <- scenarios$n10  # assign pre-defined Scenario 10 to s10.

## ----s10-info-----------------------------------------------------------------
# Show basic scenario information: 
s10$scen_lbl  # shows descriptive label:
s10$cond_lbl  # shows current condition:
s10$dec_lbl   # shows current decision:
s10$popu_lbl  # shows current population:
s10$scen_apa  # shows current source: 

## ----s10-summary--------------------------------------------------------------
summary(s10) # summarizes key scenario information:

## ----s10-icons, eval = FALSE, fig.width = 7.2, fig.height = 4.5---------------
#  plot(s10, type = "tab")                   # plot 2x2 table
#  plot(s10, type = "icons", cex_lbl = .75)  # plot an icon array
#  plot(s10, type = "prism", area = "sq")    # plot a network/prism diagram
#  plot(s10, type = "area")                  # plot an area plot
#  plot(s10, type = "bar", dir = 2)          # plot a bar chart

## ----s10-prism-1, fig.align = "center", fig.width = 6, fig.height = 4.5-------
plot(s10, 
     by = "cddc",      # perspective: upper half by condition, lower half by decision 
     area = "hr",      # frequency boxes as horizontal rectangles (scaled to N)
     p_lbl = "num")    # probability labels: numeric only

## ----s10-prism-2, eval = FALSE------------------------------------------------
#  plot(s10, by = "cdac", area = "sq")
#  plot(s10, by = "ac", area = "hr")

## ----s10-curve, eval = FALSE, fig.align = "center", fig.width = 6, fig.height = 5----
#  plot(s10, type = "curve",
#       what = "all",  # plot "all" available curves
#       uc = .05)      # with a 5%-uncertainty range

## ----opar-set, echo = FALSE---------------------------------------------------
# opar <- par(no.readonly = TRUE)  # save plot settings.
# par(mfrow = c(1, 2))           # 1 row with 2 plots:

## ----s10-planes, fig.align = "center", fig.show = "hold", fig.width = 3.5, fig.height = 3----
## Plot plane of PPV and NPV as functions of sens and spec (for given prev): 
plot(s10, type = "plane", what = "PPV", cex_lbl = .7)  # PPV by sens x spec (fixed prev)
plot(s10, type = "plane", what = "NPV", cex_lbl = .7)  # NPV by sens x spec (fixed prev)

## ----opar-reset, echo = FALSE-------------------------------------------------
# par(op)  # reset plot settings.

## ----s9-summary---------------------------------------------------------------
# Select Scenario 9: 
s9 <- scenarios$n9  # assign pre-defined Scenario 9 to s9. 

# Basic scenario information: 
s9$scen_lbl  # shows descriptive label:
s9$popu_lbl  # shows current population:


## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## Initialize some stuff:
library("riskyr")  # loads the package
op <- par(no.readonly = TRUE)

## ----load_riskyr, message = FALSE----------------------------------------
library("riskyr")  # loads the package

## ----create_scenario_minimal_prob----------------------------------------
# Create a minimal scenario (from probabilities):
my_scenario <- riskyr(prev = .45, 
                      sens = .98,
                      spec = .46)

## ----create_scenario_min_freq--------------------------------------------
# Create a minimal scenario (from frequencies):
my_scenario_2 <- riskyr(hi = my_scenario$hi, 
                        mi = my_scenario$mi,
                        fa = my_scenario$fa,
                        cr = my_scenario$cr)

## ----verify_equal_prob_freq----------------------------------------------
all.equal(my_scenario, my_scenario_2)

## ----create_scenario_custom----------------------------------------------
# Create a customized scenario: 
my_scenario <- riskyr(scen_lbl = "Identifying reoffenders", 
                      popu_lbl = "prison inmates", 
                      cond_lbl = "reoffending",
                      cond.true_lbl = "offends again", cond.false_lbl = "does not offend again",
                      dec_lbl = "test result",
                      dec.pos_lbl = "predict to\nreoffend", dec.neg_lbl = "predict to\nnot reoffend",
                      hi_lbl = "reoffender found", mi_lbl = "reoffender missed",
                      fa_lbl = "false accusation", cr_lbl = "correct release",
                      prev = .45,  # prevalence of being a reoffender. 
                      sens = .98,  # p( will reoffend | offends again )
                      spec = .46,  # p( will not reoffend | does not offend again )
                      fart =  NA,  # p( will reoffend | does not offend gain )
                      N = 753,     # population size
                      scen_src = "(a ficticious example)")

## ----fnet, include = FALSE, fig.width = 7.2, fig.height = 6--------------
plot(my_scenario)

## ----icons, fig.width = 7.2, fig.height = 4.8----------------------------
plot(my_scenario, plot_type = "icons")

## ----full_summary--------------------------------------------------------
summary(my_scenario)

## ----summary_prob, include = FALSE---------------------------------------
summary(my_scenario, summarize = "prob")

## ----tree, fig.width = 7.2, fig.height = 5-------------------------------
plot(my_scenario, plot_type = "tree", by = "dc")  # plot tree diagram (splitting N by decision)

## ----plotting_curve, fig.width = 7.2, fig.height = 5---------------------
plot(my_scenario, plot_type = "curve", uc = .05)

## ----scenario_table, echo = FALSE, results = "asis"----------------------
library(knitr)
scen_table <- df_scenarios[-1,
                           c("scen_lbl", "cond_lbl", "N", "prev",
                             "sens", "spec", "fart")]
scen_table[, -c(1:2)] <- round(scen_table[, -c(1:2)], 3)
names(scen_table) <- c("Scenario", "Condition", "N", "prev", "sens", "spec", "fart")
kable(scen_table)

## ----s21_select----------------------------------------------------------
s21 <- scenarios$n21  # assign pre-defined Scenario 21 to s21.

## ----s21_info------------------------------------------------------------
# Show basic scenario information: 
s21$scen_lbl  # shows descriptive label:
s21$cond_lbl  # shows current condition:
s21$dec_lbl   # shows current decision:
s21$popu_lbl  # shows current population:
s21$scen_apa  # shows current source: 

## ----s21_summary---------------------------------------------------------
summary(s21) # summarizes key scenario information:

## ----s21_icons, fig.width = 7.2, fig.height = 4.5------------------------
plot(s21, plot_type = "icons", cex_lbl = 0.75)  # plot default icon array: 

## ----s21_prism_1, fig.width = 7.2, fig.height = 5.5----------------------
plot(s21, 
     by = "cddc",      # perspective: upper half by condition, lower half by decision 
     area = "hr",      # frequency boxes as horizontal rectangles (scaled to N)
     p_lbl = "num")    # probability labels: numeric only

## ----s21_prism_2, eval = FALSE-------------------------------------------
#  plot(s21, by = "cdac", area = "sq")
#  plot(s21, by = "ac", area = "hr")

## ----s21_curve, fig.width = 7.2, fig.height = 6.2------------------------
plot(s21, plot_type = "curve", 
     what = "all",  # plot "all" available curves 
     uc = .05)      # with a 5%-uncertainty range 

## ----s21_planes, results = "hold", fig.width = 7.9, fig.height = 4.2-----
op <- par(no.readonly = TRUE)  # save plot settings.
par(mfrow = c(1, 2))           # 1 row with 2 plots:

## Plot plane of PPV and NPV as functions of sens and spec (for given prev): 
plot(s21, plot_type = "plane", what = "PPV", cex_lbl = 0.7)  # PPV by sens x spec (fixed prev)
plot(s21, plot_type = "plane", what = "NPV", cex_lbl = 0.7)  # NPV by sens x spec (fixed prev)
par(op)  # reset plot settings.

## ----s22_summary---------------------------------------------------------
s22 <- scenarios$n22  # assign pre-defined Scenario 22 to s22. 

# Show basic scenario information: 
s22$scen_lbl  # shows descriptive label:
s22$popu_lbl  # shows current population:


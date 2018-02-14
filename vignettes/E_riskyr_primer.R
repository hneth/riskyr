## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----create_scenario-----------------------------------------------------
# Create a custom scenario: 
my.scenario <- riskyr(scen.lbl = "Identifying reoffenders", 
                      popu.lbl = "inmates", 
                      cond.lbl = "reoffending",
                      cond.true.lbl = "offends again", cond.false.lbl = "does not offend again",
                      dec.lbl = "test result",
                      dec.pos.lbl = "predict to reoffend", dec.neg.lbl = "predict to not reoffend",
                      hi.lbl = "reoffender found", mi.lbl = "reoffender missed",
                      fa.lbl = "false accusation", cr.lbl = "correct release",
                      prev = .45,  # prevalence of being a reoffender. 
                      sens = .98,  # p( will reoffend | offends again )
                      spec = .46,  # p( will not reoffend | does not offend again )
                      fart =  NA,  # p( will reoffend | does not offend gain )
                      N = 753,     # population size
                      scen.src = "ficticious example scenario")

## ----icons, fig.width=7, fig.height=5------------------------------------
plot(my.scenario, plot.type = "icons", cex.lbl = 0.85)

## ----full_summary--------------------------------------------------------
summary(my.scenario)

## ----summary_prob--------------------------------------------------------
summary(my.scenario, summarize = "prob")

## ----tree, fig.width=7, fig.height=6-------------------------------------
plot(my.scenario, plot.type = "tree", by = "dc")

## ----plotting_curve, fig.width=7, fig.height=5.5-------------------------
plot(my.scenario, plot.type = "curve")

## ----scenario_table, echo = FALSE, results = 'asis'----------------------
library(knitr)
scen.table <- df.scenarios[-1,
                           c("scen.lbl", "cond.lbl", "dec.lbl", "N", "prev",
                             "sens", "spec", "fart", "scen.src")]
scen.table[, -c(1:3, 9)] <- round(scen.table[, -c(1:3, 9)], 3)
names(scen.table) <- c("Scenario", "Condition", "Decision",
                       "N", "Prevalence", "Sensitivity", "Specificity", "False alarm rate",
                       "Source")
kable(scen.table)

## ----example_use_a, fig.width=7, fig.height=5.5--------------------------
## (a) PSA screening with high prevalence: ----------
s21 <- scenarios$n21  # assign pre-defined Scenario_21 to s

## ----summary_s21---------------------------------------------------------
summary(s21) # shows all scenario information.

## ----summary_s21_freq----------------------------------------------------
summary(s21, summarize = "freq") # shows frequencies. 

## ----s21_net, fig.width=7, fig.height=6----------------------------------
plot(s21) # plots a network diagram (by default)

## ----s21_icons, fig.width = 7, fig.height = 5----------------------------
plot(s21, plot.type = "icons", cex.lbl = 0.75)

## ----s21_curve, fig.width = 7, fig.height = 5----------------------------
plot(s21, plot.type = "curve", what = "all")

## ----s22_summary---------------------------------------------------------
s22 <- scenarios$n22  # assign pre-defined Scenario_22 to s22. 

summary(s22)

## ----example_use_b, fig.width=7.5, fig.height=5.5------------------------
op <- par(no.readonly = TRUE)
par(mfrow = c(1,2))  # set plotting space for direct comparison. 

## Contrast two versions: 
plot(s21, plot.type = "plane", what = "PPV", cex.lbl = 0.75)
plot(s22, plot.type = "plane", what = "PPV", cex.lbl = 0.75)


## ----examples_NPV, fig.width=7.5, fig.height=5.5-------------------------
op <- par(no.readonly = TRUE)
par(mfrow = c(1,2))  # set plotting space for direct comparison. 

## Contrast two versions: 
plot(s21, plot.type = "plane", what = "NPV", cex.lbl = 0.75)
plot(s22, plot.type = "plane", what = "NPV", cex.lbl = 0.75)

## ----reset_par, echo = FALSE---------------------------------------------
par(op)


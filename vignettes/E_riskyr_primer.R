## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----create_scenario-----------------------------------------------------
# Create your custom scenario: 
custom.scenario <- riskyr(scen.lbl = "Identify reoffenders", 
                          cond.lbl = "Being a reoffender",
                          popu.lbl = "Prisoners", 
                          cond.true.lbl = "Has reoffended", cond.false.lbl = "Has not reoffended",
                          dec.lbl = "Test result",
                          dec.pos.lbl = "will reoffend", dec.neg.lbl = "will not reoffend",
                          hi.lbl = "Reoffender found", mi.lbl = "Reoffender missed",
                          fa.lbl = "False accusation", cr.lbl = "Correct release",
                          prev = .45,  # prevalence of being a reoffender. 
                          sens = .98,
                          spec = .46, fart = NA, N = 753,
                          scen.src = "Ficticious example scenario")

## ----icons, fig.width=7, fig.height=5------------------------------------
plot(custom.scenario, plot.type = "icons")

## ----full_summary--------------------------------------------------------
summary(custom.scenario)

## ----summary_prob--------------------------------------------------------
summary(custom.scenario, summarize = "prob")

## ----tree, fig.width=7, fig.height=6-------------------------------------
plot(custom.scenario, plot.type = "tree", by = "dc")

## ----plotting_curve, fig.width=7, fig.height=5.5-------------------------
plot(custom.scenario, plot.type = "curve")

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

## ---- summary_s21_freq---------------------------------------------------
summary(s21, summarize = "freq") # shows frequencies. 

## ---- s21_net, fig.width=7, fig.height=6---------------------------------
plot(s21) # plots a network diagram (by default)

## ----s21_icons, fig.width=7, fig.height=5--------------------------------
plot(s21, plot.type = "icons")

## ----s21_curve, fig.width=7, fig.height=5--------------------------------
plot(s21, plot.type = "curve", what = "all")

## ------------------------------------------------------------------------
plot(s21, plot.type = "plane", what = "PPV")

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


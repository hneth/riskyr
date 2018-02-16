## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# initialize some stuff:
library("riskyr")  # loads the package
op <- par(no.readonly = TRUE)   

## ----load_riskyr---------------------------------------------------------
library("riskyr")  # loads the package

## ----create_scenario_minimal---------------------------------------------
# Create a minimal scenario:
my.scenario <- riskyr(prev = .45, 
                      sens = .98,
                      spec = .46)

## ----create_scenario_custom----------------------------------------------
# Create a customized scenario: 
my.scenario <- riskyr(scen.lbl = "Identifying reoffenders", 
                      popu.lbl = "prison inmates", 
                      cond.lbl = "reoffending",
                      cond.true.lbl = "offends again", cond.false.lbl = "does not offend again",
                      dec.lbl = "test result",
                      dec.pos.lbl = "predict to\nreoffend", dec.neg.lbl = "predict to\nnot reoffend",
                      hi.lbl = "reoffender found", mi.lbl = "reoffender missed",
                      fa.lbl = "false accusation", cr.lbl = "correct release",
                      prev = .45,  # prevalence of being a reoffender. 
                      sens = .98,  # p( will reoffend | offends again )
                      spec = .46,  # p( will not reoffend | does not offend again )
                      fart =  NA,  # p( will reoffend | does not offend gain )
                      N = 753,     # population size
                      scen.src = "(a ficticious example)")

## ----fnet, include = FALSE, fig.width = 7.2, fig.height = 6--------------
plot(my.scenario, plot.type = "fnet", round = FALSE)

## ----icons, fig.width = 7.2, fig.height = 4.8----------------------------
plot(my.scenario, plot.type = "icons")

## ----full_summary--------------------------------------------------------
summary(my.scenario)

## ----summary_prob, include = FALSE---------------------------------------
summary(my.scenario, summarize = "prob")

## ----tree, fig.width = 7.2, fig.height = 5.5-----------------------------
plot(my.scenario, plot.type = "tree", by = "dc")  # plot tree diagram (splitting N by decision)

## ----plotting_curve, fig.width = 7, fig.height = 5.5---------------------
plot(my.scenario, plot.type = "curve")

## ----scenario_table, echo = FALSE, results = "asis"----------------------
library(knitr)
scen.table <- df.scenarios[-1,
                           c("scen.lbl", "cond.lbl", "N", "prev",
                             "sens", "spec", "fart")]
scen.table[, -c(1:2)] <- round(scen.table[, -c(1:2)], 3)
names(scen.table) <- c("Scenario", "Condition", "N", "prev", "sens", "spec", "fart")
kable(scen.table)

## ----s21_select----------------------------------------------------------
s21 <- scenarios$n21  # assign pre-defined Scenario 21 to s21.

## ----s21_info------------------------------------------------------------
# Show basic scenario information: 
s21$scen.lbl  # shows descriptive label:
s21$cond.lbl  # shows current condition:
s21$dec.lbl   # shows current decision:
s21$popu.lbl  # shows current population:
s21$scen.apa  # shows current source: 

## ----s21_summary---------------------------------------------------------
summary(s21) # summarizes key scenario information:

## ----s21_icons, fig.width = 7.2, fig.height = 4.5------------------------
plot(s21, plot.type = "icons", cex.lbl = 0.75)

## ----s21_fnet, fig.width = 7.2, fig.height = 6.5-------------------------
plot(s21, plot.type = "fnet", area = "sq")

## ----s21_curve, fig.width = 7.2, fig.height = 5--------------------------
plot(s21, plot.type = "curve", what = "all")

## ----s21_planes, results = "hold", fig.width = 7.8, fig.height = 4.2-----
op <- par(no.readonly = TRUE)  # save plot settings.
par(mfrow = c(1, 2))           # 1 row with 2 plots:

## Plot plane of PPV and NPV as functions of sens and spec (for given prev): 
plot(s21, plot.type = "plane", what = "PPV", cex.lbl = 0.75)
plot(s21, plot.type = "plane", what = "NPV", cex.lbl = 0.75)
par(op)  # reset plot settings.

## ----s22_summary---------------------------------------------------------
s22 <- scenarios$n22  # assign pre-defined Scenario 22 to s22. 

# Show basic scenario information: 
s22$scen.lbl  # shows descriptive label:
s22$popu.lbl  # shows current population:


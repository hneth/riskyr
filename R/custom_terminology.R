## riskyr
## 2018 01 06
## -----------------------------------------------
## Define and initialize current terminology
## (i.e., labels and text elements):

terminology <- list(
  scenario.lbl = "Default scenario",
  scenario.txt = "Describe the current scenario in a short paragraph here.\nThis description may include several sentences.",
  target.population.lbl = "Describe target population here",
  ## (a) Condition:
  condition.lbl  = "Current condition",
  cond.true.lbl  = "Condition true",   # "has condition", "is affected"
  cond.false.lbl = "Condition false", # "does not have condition", "is unaffected"
  ## (b) Decisions:
  decision.lbl  = "Diagnostic decision",
  dec.true.lbl  = "Decision positive",  # "has condition", "is affected"
  dec.false.lbl = "Decision negative", # "does not have condition", "is unaffected"
  ## (c) sdt cases (combinations):
  sdt.hi.lbl = "hit",   # "true positive", "has condition and is detected as such", p(dec TRUE | cond TRUE)
  sdt.mi.lbl = "miss",  # "false negative", "omission", "has condition and is NOT detected as such", p(dec FALSE | cond TRUE)
  sdt.fa.lbl = "false alarm",      # "false positive", p(dec TRUE | cond FALSE)
  sdt.cr.lbl = "correct rejection" # "true negative",  p(dec FALSE | cond FALSE)
  )

## -----------------------------------------------
## eof.

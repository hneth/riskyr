## riskyR
## 2018 01 07
## -----------------------------------------------
## Define and initialize current terminology (txt)
## (i.e., text labels and longer text elements):

txt <- list(
  scenario.lbl = "Scenario name",
  scenario.txt = "Describe the current scenario in a short paragraph here.\nThis description may include several sentences.",
  target.population.lbl = "Describe the target population in a few words here",

  ## (a) Condition:
  condition.lbl  = "Condition",       # "disease", "Breast cancer", "HIV", "Sepsis" etc.
  cond.true.lbl  = "Condition true",  # "has condition", "is affected"
  cond.false.lbl = "Condition false", # "does not have condition", "is unaffected"

  ## (b) Decision:
  decision.lbl  = "Decision",          # "diagnostic decision", "Mammography", "HIV test"
  dec.true.lbl  = "Decision positive", # "is called", "is judged to have condition", "is classified as affected"
  dec.false.lbl = "Decision negative", # "is not called", "is judged to not have condition", "is classified as unaffected"

  ## (c) Labels for the 4 SDT cases (combinations):
  sdt.hi.lbl = "true positive",  # "hit", "true positive", "has condition and is detected as such", "set(dec POS & cond TRUE)"
  sdt.mi.lbl = "false negative", # "miss", "false negative", "omission", "has condition and is NOT detected as such", "set(dec NEG & cond TRUE)"
  sdt.fa.lbl = "false positive", # "false alarm",       "false positive", "set(dec POS & cond FALSE)"
  sdt.cr.lbl = "true negative"   # "correct rejection", "true negative",  "set(dec NEG & cond FALSE)"
  )

## ToDo:
## - add txt to cus object
## - make txt user-customizable

## -----------------------------------------------
## eof.

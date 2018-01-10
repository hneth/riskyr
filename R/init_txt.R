## init_txt.R | riskyR
## 2018 01 09
## -----------------------------------------------
## Define and initialize current set of
## custom text elements (txt):

## Note that txt contains defaults for user inputs.

## -----------------------------------------------
## Set defaults for all text inputs (txt):

txt <- list(
  scen.lbl = "Scenario Title", # in Title Caps (to print in plot titles)
  scen.txt = "Describe the current scenario in a short paragraph of text here.\nThis description may include several sentences.",
  scen.src = "Describe the source information for this scenario here",

  popu.lbl = "Describe the target population in a few words here",

  ## (a) Condition:
  cond.lbl = "Condition",             # "Condition X", "disease", "Breast cancer", "HIV", "Sepsis" etc.
  cond.true.lbl  = "Condition true",  # "has condition", "is affected"
  cond.false.lbl = "Condition false", # "does not have condition", "is unaffected"

  ## (b) Decision/prediction/predicted condition:
  dec.lbl = "Decision",                # "Prediction", "Diagnostic decision", "Mammography", "HIV test"
  dec.true.lbl  = "Decision positive", # "Prediction positive", "called", "is judged to have condition", "is classified as affected"
  dec.false.lbl = "Decision negative", # "Prediction negative", "not called", "is judged to not have condition", "is classified as unaffected"

  ## (c) Labels for the 4 SDT cases (combinations):
  sdt.hi.lbl = "True positive",  # "hit", "true positive", "has condition and is detected/predicted as such", "set(dec POS & cond TRUE)"
  sdt.mi.lbl = "False negative", # "miss", "false negative", "omission", "has condition and is NOT detected/predicted as such", "set(dec NEG & cond TRUE)"
  sdt.fa.lbl = "False positive", # "false alarm",       "false positive", "set(dec POS & cond FALSE)"
  sdt.cr.lbl = "True negative"   # "correct rejection", "true negative",  "set(dec NEG & cond FALSE)"
  )

# length(txt)

## -----------------------------------------------
## (+) ToDo:

## - add txt to a cus object?
## - make txt user-customizable!

## -----------------------------------------------
## eof.

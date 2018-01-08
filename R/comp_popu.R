## comp_popu.R | riskyR
## 2018 01 08
## -----------------------------------------------
## Compute a population (popu) as 3 x N data frame
## based on only the necessary parameters of:
##  [a. the current N from prm (not needed)]
##   b. the current frequencies of freq
##   c. the current text labels of txt

## Notes:
## - Called "popu" rather than "pop" as it is an output,
##   rather than an input!
## - Always use prm (essential) rather than env (NON-essential)!

## -----------------------------------------------
## Compute current population (popu):

comp_popu <- function(## N = prm$N, ## (not needed)
  ## frequencies from freq:
  n.true = freq$cond.true, n.false = freq$cond.false,
  n.hi = freq$hi, n.mi = freq$mi, n.fa = freq$fa, n.cr = freq$cr,
  ## text labels from txt:
  cond.true.lbl = txt$cond.true.lbl, cond.false.lbl = txt$cond.false.lbl,
  dec.true.lbl = txt$dec.true.lbl, dec.false.lbl = txt$dec.false.lbl,
  sdt.hi.lbl = txt$sdt.hi.lbl, sdt.mi.lbl = txt$sdt.mi.lbl,
  sdt.fa.lbl = txt$sdt.fa.lbl, sdt.cr.lbl = txt$sdt.cr.lbl) {

  ## (1) Define and initialize 3 vectors of length N:
  ## (a) Truth:
  truth <- c(rep(TRUE, n.true), rep(FALSE, n.false))

  ## (b) Decisions (ordered by ACTUAL truth values of condition):
  decision <- c(rep(TRUE, n.hi), rep(FALSE, n.mi),
                rep(TRUE, n.fa), rep(FALSE, n.cr))

  ## (c) SDT (status decision/truth):
  sdt <- c(rep("hi", n.hi), rep("mi", n.mi),
           rep("fa", n.fa), rep("cr", n.cr))


  ## (2) Coerce 3 vectors into ordered factors:
  ## (a) Truth:
  truth <- factor(truth,
                  levels = c(TRUE, FALSE),
                  labels = c(cond.true.lbl, cond.false.lbl), # explicit labels
                  ordered = TRUE)

  ## (b) Decisions (ordered by ACTUAL truth values of condition):
  decision <- factor(decision,
                     levels = c(TRUE, FALSE),
                     labels = c(dec.true.lbl, dec.false.lbl), # explicit labels
                     ordered = TRUE)

  ## (c) SDT (status decision/truth):
  sdt <- factor(sdt,
                levels = c("hi", "mi", "fa", "cr"),
                labels = c(sdt.hi.lbl, sdt.mi.lbl, sdt.fa.lbl, sdt.cr.lbl), # explicit labels
                # labels = c("hi", "mi", "fa", "cr"), # implicit labels
                ordered = TRUE)

  ## (5) Combine 3 vectors in a population data frame pop:
  popu <- data.frame(tru = truth,
                     dec = decision,
                     sdt = sdt)
  names(popu) <- c("Truth", "Decision", "STD")

  ## (6) Return the entire data frame pop:
  return(popu)

}

## Apply:
popu <- comp_popu()
# popu

## Checks:
# dim(popu)
# head(popu)
# tail(popu)

## -----------------------------------------------
## (+) ToDo:

## - ...

## -----------------------------------------------
## eof.

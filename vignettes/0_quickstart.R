## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
custom.scenario <- riskyr(scen.lbl = txt$scen.lbl, scen.lng = txt$scen.lng,
                  scen.txt = txt$scen.txt, popu.lbl = txt$popu.lbl,
                  cond.lbl = txt$cond.lbl,
                  cond.true.lbl = txt$cond.true.lbl, cond.false.lbl = txt$cond.false.lbl,
                  dec.lbl = txt$dec.lbl,
                  dec.pos.lbl = txt$dec.pos.lbl, dec.neg.lbl = txt$dec.neg.lbl,
                  hi.lbl = txt$hi.lbl, mi.lbl = txt$mi.lbl,
                  fa.lbl = txt$fa.lbl, cr.lbl = txt$cr.lbl,
                  prev = num$prev,
                  sens = num$sens,
                  spec = num$spec, fart = NA, N = freq$N,
                  scen.src = txt$scen.src, scen.apa = txt$scen.apa)


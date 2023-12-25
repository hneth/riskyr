## cum_risk.R | riskyr
## 2023 12 25
## Compute cumulative risks

# Recursive application of repeated risk r:

apply_risk <- function(pc = 100, ev = 0, r, i){

  # cat(i, ": ", pc, "\n")


  if (i == 0){

    print(paste0(ev, ": ", pc))

  } else {

      j <- i - 1
      pc <- c(apply_risk(pc * r, ev + 1, r, j), apply_risk(pc * (1 - r), ev, r, j))

  }

  # Output:
  return(pc)

}

# Check:
apply_risk(pc = 100, ev = 0, r = .25, i = 4)




## (*) Done: ----------

## - etc.

## (+) ToDo: ----------

## - etc.

## eof. ------------------------------------------

## comp_metrics.R | riskyr
## 2019 12 26
## Compute key metrics from 4 basic frequencies:
## -----------------------------------------------

# (0) 2x2 matrix: ------

mx <- c(4, 2, 1, 3)



# (1) Frequencies: ------

# get_a: -----

get_a <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a)

  return(out)

}


# get_b: -----

get_b <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)

  return(out)

}


# get_c: -----

get_c <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)

  return(out)

}


# get_d: -----

get_d <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)

  return(out)

}


# get_N: -----

get_N <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + b + c + d)

  return(out)

}



# (2) Probabilities: ------

# get_prev: -----

get_prev <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + c)/(a + b + c + d)

  return(out)

}


# get_bias: -----

get_bias <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + b)/(a + b + c + d)

  return(out)

}


# get_ACC: -----

get_ACC <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a + d)/(a + b + c + d)

  return(out)

}



# (3) Conditional Probabilities: ------

# (3a) normalized by column:

# get_sens: -----

get_sens <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a)/(a + c)

  return(out)

}


# get_TPR: -----

get_TPR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- get_sens(a = a, b = b, c = c, d = d)  # TPR = sens

  return(out)

}


# get_FPR: -----

get_FPR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)/(b + d)

  return(out)

}


# get_FNR: -----

get_FNR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)/(a + c)

  return(out)

}


# get_spec: -----

get_spec <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)/(b + d)

  return(out)

}


# get_TNR: -----

get_TNR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- get_spec(a = a, b = b, c = c, d = d)  # TNR = spec

  return(out)

}

# (3b) normalized by row:

# get_PPV: -----

get_PPV <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (a)/(a + b)

  return(out)

}


# get_FDR: -----

get_FDR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (b)/(a + b)

  return(out)

}


# get_FOR: -----

get_FOR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (c)/(c + d)

  return(out)

}


# get_NPV: -----

get_NPV <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- (d)/(c + d)

  return(out)

}



# (4) Likelihood/odds ratios: ------

# get_LRp: -----

get_LRp <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  sens <- NA
  FPR <- NA

  sens <- get_sens(a, b, c, d)
  FPR  <- get_FPR(a, b, c, d)

  out <- sens/FPR

  return(out)

}


# get_LRn: -----

get_LRn <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  spec <- NA
  FNR <- NA

  FNR  <- get_FNR(a, b, c, d)
  spec <- get_spec(a, b, c, d)

  out <- FNR/spec

  return(out)

}


# get_DOR: -----

get_DOR <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  LRp <- NA
  LRn <- NA
  sens <- NA
  spec <- NA
  PPV <- NA
  NPV <- NA

  out <- (a * d)/(b * c)

  # Checks:
  LRp <- get_LRp(a, b, c, d)
  LRn <- get_LRn(a, b, c, d)
  alt <- LRp/LRn

  sens <- get_sens(a, b, c, d)
  spec <- get_spec(a, b, c, d)
  alt2 <- (sens * spec)/((1 - sens) * (1 - spec))

  PPV <- get_PPV(a, b, c, d)
  NPV <- get_NPV(a, b, c, d)
  alt3 <- (PPV * NPV)/((1 - PPV) * (1 - NPV))

  if ((round(out, 9) == round(alt, 9)) &
      (round(out, 9) == round(alt2, 9)) &
      (round(out, 9) == round(alt3, 9))){
    return(out)
  } else {
    message(paste0("get_DOR: out = ", out, " differs from some alt = ", alt))
  }

}



# (5) Integrated measures: ------


# (a) Contingencies: ------

# get_dPr: -----

get_dPr <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- ((a)/(a + b)) - ((c)/(c + d))  # PPV - FOR

  return(out)

}


# get_dPc: -----

get_dPc <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA

  out <- ((a)/(a + c)) - ((b)/(b + d))  # sens - FPR

  return(out)

}


# get_chi2: -----

get_chi2 <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  out_num <- NA
  out_den <- NA

  # Compute chi2:
  out_num <- (a + b + c + d) * ((a * d) - (b * c))^2
  out_den <- (a + b) * (c + d) * (a + c) * (b + d)
  out <- out_num/out_den

  # Check: chi2 == N * dPc * dPr
  alt <- get_N(a, b, c, d) *
    get_dPc(a, b, c, d) *
    get_dPr(a, b, c, d)

  if (round(out, 9) == round(alt, 9)){
    return(out)
  } else {
    message(paste0("get_chi2: out = ", out, " differs from alt = ", alt))
  }

}



# (b) Other integrated measures:
# get_MCC: -----

get_MCC <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4]){

  out <- NA
  out_num <- NA
  out_den <- NA

  # Compute MCC:
  out_num <- ((a * d) - (b * c))
  out_den <- sqrt((a + b) * (c + d) * (a + c) * (b + d))
  out <- out_num/out_den

  # Check: chi2 == N * MCC^2
  alt <- sqrt(get_chi2(a, b, c, d)/get_N(a, b, c, d))

  if (round(out, 9) == round(alt, 9)){
    return(out)
  } else {
    message(paste0("get_MCC: out = ", out, " differs from alt = ", alt))
  }

}


# get_F1s: -----

get_F1s <- function(a = mx[1], b = mx[2], c = mx[3], d = mx[4], beta = 1){

  out <- NA
  precision <- NA
  recall <- NA

  # Compute F1s:
  precision <- get_PPV(a, b, c, d)
  recall    <- get_sens(a, b, c, d)
  out <- (1 + beta^2) * (precision * recall)/((beta^2 * precision) + recall)

  return(out)

}



# (6) Risk measures: ------

# AR: Absolute risk (i.e., 2 conditional probabilities)
#     - correspondence of AR+ to sens/PPV and
#     -                of AR- to 1-spec/1-NPV depends on direction.

# RR: Relative risk
# RRR: Relative risk reduction
# ARR: Absolute risk reduction



## (+) ToDo: ----------

## - ...

## eof. ------------------------------------------

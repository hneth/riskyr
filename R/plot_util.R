## plot_util.R | riskyr
## 2018 10 07
## Helper functions for plotting objects (freq/prob, boxes/lines).
## -----------------------------------------------

## (0) Generic plotting functions: ----------

## ex: Restore old par settings (see ?par) ------

ex <- function() {

  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))

  ## ...
  ## ... lots of par() settings and plots
  ## ...

  invisible()  # restores par(opar)
}

## Check:
# ex()  # Note: dev.off() also restores default par settings.



## (1) Define and create box objects: ----------

## Define objects: Create an object of type "box" as a list: ----------
box0 <- list(name = "box0_name", x = .5, y = .5, lx = 1, ly = 1)  # object as list
class(box0) <- "box"  # name class

## Check:
# box0 # shows object (list)

## box class: Create constructor function for the "box" class: ----------
make_box <- function(name, x, y, lx, ly) {

  # Note: It is good practice to give the constructor function
  #       the same name as the class (here: box).  However, as
  #       the function box exists (in graphics), we use make_box here.

  # Check integrity of arguments:
  if (!is.character(name)) stop("name must be a character.")
  if (!is.numeric(x)) stop("x must be numeric.")
  if (!is.numeric(y)) stop("y must be numeric.")
  if (!is.numeric(lx)) stop("lx must be numeric.")
  if (!is.numeric(ly)) stop("ly must be numeric.")
  # if (x < x_min || x > x_max)  stop("x must be in valid range.")
  # if (y < y_min || y > y_max)  stop("y must be in valid range.")

  # Create object as a list:
  obj <- list(name = name, x = x, y = y, lx = lx, ly = ly)

  # Set class by using class() or attr() function:
  class(obj) <- "box"          # name class
  attr(obj, "class") <- "box"  # set attr

  # Return object:
  obj
}

## Check:
# box1 <- make_box(1, 0, 0, 1, 1)  # => Error due to stop; no object created.
# box1 <- make_box("box1_name", .1, .1, 1, 1) # use constructor function to create new objects.
# box1

## box methods: Create generic print and plot methods for box objects: ---------

print.box <- function(obj) {
  cat("box name:", obj$name, "\n")
  cat("position: x =", obj$x, "; y =", obj$y, "\n")
  cat("width:   lx =", obj$lx, "\n")
  cat("height:  ly =", obj$ly, "\n")
}

plot.box <- function(obj, ...) {

  ## Call plot_fbox helper function:
  plot_fbox(fname = obj$name,
            x  = obj$x,   y = obj$y,
            lx = obj$lx, ly = obj$ly,
            ...)
}

# ## Check:
# # Create some box objects:
# box_b1 <- make_box("1st_box", 3, 9, 2, 2)  # 1st box with an arbitrary label
# box_b2 <- make_box("2nd_box", 3, 6, 2, 2)  # 2nd box with an arbitrary label
# box_hi <- make_box("hi", 3, 3, 2, 2)       # box with known freq label
# box_mi <- make_box("mi", 6, 3, 2, 2)       # box with known freq label
# print(box_b1)
# print(box_hi)
# # Plot boxes:
# plot(c(0, 10), c(0, 10), type = "n") # 2 points, empty canvas
# plot(box_b1)  # plot box with arbitrary label (and default color)
# plot(box_b2, col = "skyblue", cex = 2/3, font = 2)  # plot box with arbitrary label (and specific color)
# plot(box_hi)  # plot box with known freq label (and type, color, etc.)
# plot(box_mi, lbl_type = "nam",
#      cex = 2/3, lwd = 4, col = "gold", font = 2) # overwrite default parameters








## (2) Constructing and plotting labels, boxes, and links: ----------




## (A) Labels: ------


## label_freq: Label a known frequency (in freq) by fname ----------
label_freq <- function(fname,
                       lbl_type = "default",    # label type: "default", "nam"/"num"/"namnum", "abb", or NULL/NA/"no" (to hide label).
                       lbl_sep = " = ",         # separator: " = " (default), ":\n"
                       cur_freq = freq,         # current freq
                       cur_txt = txt            # current txt
) {

  ## Initialize:
  f_lbl <- fname # initialize (in case of unknown freq)
  f_val  <- NA
  f_type <- NA

  ## (0) If lbl_type is NULL or NA:
  if ( is.null(fname) || is.na(fname) ||
       is.null(lbl_type) || is.na(lbl_type) || tolower(lbl_type) == "no" || tolower(lbl_type) == "none" || tolower(lbl_type) == "nil" ) {

    f_lbl <- NA
    return(f_lbl)  # return NA
  }

  ## Robustness (after handling NA/NULL cases):
  lbl_type <- tolower(lbl_type)
  if (lbl_type == "val") (lbl_type <- "num")
  if (lbl_type == "namval" || lbl_type == "full" || lbl_type == "all") (lbl_type <- "namnum")
  if (lbl_type == "abbnum") (lbl_type <- "default")


  ## (1) Abbreviated name (i.e., variable name of fname): ----
  if (lbl_type == "abb") {

    f_lbl <- as.character(fname)  # use fname as f_lbl

    # If f_lbl contains a dot (.):  Use only the name part after the dot:
    if (any(grep(pattern = "\\.", x = f_lbl))) {  # if f_lbl contains a dot (.):

      nameparts <- unlist(strsplit(f_lbl, split = "\\."))
      part_1 <- nameparts[1]  # 1st part of f_lbl
      part_2 <- nameparts[2]  # 2nd part of f_lbl
      f_lbl <- part_2  # 2nd part (after dot)

    }

    return(f_lbl)
  }

  ## (2) Determine the frequency value f_val of cur_freq corresponding to fname: ----

  if (lbl_type != "nam") {

    if (tolower(fname) %in% tolower(names(cur_freq))) { # if fname corresponds to named frequency in cur_freq:

      # f_lbl <- fname  # initialize to fname

      # Derive current value corresponding to fname in cur_freq:
      ix <- which(tolower(names(cur_freq)) == tolower(fname))  # index of fname in cur_freq
      f_val <- cur_freq[ix]  # current cur_freq value

      # Round f_val to n_digits:
      n_digits <- 2  # currently fixed parameter
      f_val <- round(as.numeric(f_val), digits = n_digits)  # round f_val (i.e., value displayed, NOT the actual freq value computed!)

      # Type of frequency:
      # f_type <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

    } # if (fname %in% names(cur_freq)...
  }

  ## (3) Compose f_lbl based on lbl_type: ---

  if (lbl_type == "num"){

    # (a) Value:
    f_lbl <- as.character(f_val)

  } else if (lbl_type == "namnum"){

    ## (b) Name AND value of cur_freq:

    # if (tolower(fname) == "n") { f_lbl <- "N" }         # use "N" as f_lbl
    if (tolower(fname) == "n") { f_lbl <- cur_txt$popu.lbl }  # use population label as f_lbl

    if (tolower(fname) == "hi") { f_lbl <- cur_txt$hi.lbl }
    if (tolower(fname) == "mi") { f_lbl <- cur_txt$mi.lbl }
    if (tolower(fname) == "fa") { f_lbl <- cur_txt$fa.lbl }
    if (tolower(fname) == "cr") { f_lbl <- cur_txt$cr.lbl }

    if (tolower(fname) == "cond.true")  { f_lbl <- cur_txt$cond.true.lbl }
    if (tolower(fname) == "cond.false") { f_lbl <- cur_txt$cond.false.lbl }

    if (tolower(fname) == "dec.pos") { f_lbl <- cur_txt$dec.pos.lbl }
    if (tolower(fname) == "dec.neg") { f_lbl <- cur_txt$dec.neg.lbl }

    if (tolower(fname) == "dec.cor") { f_lbl <- cur_txt$dec.cor.lbl }
    if (tolower(fname) == "dec.err") { f_lbl <- cur_txt$dec.err.lbl }

    # Combine f_lbl with f_val (from above):
    f_lbl <- paste0(f_lbl, lbl_sep, as.character(f_val))

  } else if (lbl_type == "nam") {

    ## (c) ONLY the name of cur_freq:

    # if (tolower(fname) == "n") { f_lbl <- "N" }         # use "N" as f_lbl
    if (tolower(fname) == "n") { f_lbl <- cur_txt$popu.lbl }  # use population label as f_lbl

    if (tolower(fname) == "hi") { f_lbl <- cur_txt$hi.lbl }
    if (tolower(fname) == "mi") { f_lbl <- cur_txt$mi.lbl }
    if (tolower(fname) == "fa") { f_lbl <- cur_txt$fa.lbl }
    if (tolower(fname) == "cr") { f_lbl <- cur_txt$cr.lbl }

    if (tolower(fname) == "cond.true")  { f_lbl <- cur_txt$cond.true.lbl }
    if (tolower(fname) == "cond.false") { f_lbl <- cur_txt$cond.false.lbl }

    if (tolower(fname) == "dec.pos") { f_lbl <- cur_txt$dec.pos.lbl }
    if (tolower(fname) == "dec.neg") { f_lbl <- cur_txt$dec.neg.lbl }

    if (tolower(fname) == "dec.cor") { f_lbl <- cur_txt$dec.cor.lbl }
    if (tolower(fname) == "dec.err") { f_lbl <- cur_txt$dec.err.lbl }

  } else {  ## "any"/"default":

    ## (d) Any other lbl_type: Use basic fname + f_val as default:
    f_lbl <- paste0(fname, lbl_sep, as.character(f_val))

  }

  ## (4) Split/re-format long f_lbl into 2 lines of text: ----
  nchar_max <- 99  # criterium for f_lbl being too long (currently fixed)

  # if f_lbl is too long and it contains a lbl_sep (e.g., " = "):
  if ((nchar(f_lbl) > nchar_max) && any(grep(pattern = lbl_sep, x = f_lbl))) {

    # Split into 2 parts:
    lbl_parts <- unlist(strsplit(f_lbl, split = lbl_sep))
    lbl_part1 <- lbl_parts[1]  # 1st part of f_lbl
    lbl_part2 <- lbl_parts[2]  # 2nd part of f_lbl

    f_lbl <- paste0(lbl_part1, ":\n", lbl_part2)  # Put into 2 lines.
  }

  ## (5) Return f_lbl: ----
  return(f_lbl)

}

## Check:

## Basics:
# label_freq("hi")
# label_freq("prev")  # => "prev = NA" (as prev is no freq)
#
## Missing values:
# label_freq(NULL)  # => NA
# label_freq(NA)    # => NA
# label_freq("hi", lbl_type = NULL)  # => NA
# label_freq("hi", lbl_type = NA)    # => NA
#
## Abbreviated names:
# label_freq("prev", lbl_type = "abb")
# label_freq("err-fa", lbl_type = "abb")
# label_freq("stuff", lbl_type = "abb")
#
## Standard cases:
# label_freq("hi")
# label_freq("HI")
# label_freq("HI", lbl_type = "aBB")
# label_freq("hi", lbl_sep = ":\n")
# label_freq("cr", lbl_type = "num")
# label_freq("cr", lbl_type = "nam")
# label_freq("cr", lbl_type = "namnum")
# label_freq("cr", lbl_type = "nix")  # default lbl_type
# label_freq("dec.err", lbl_type = "all")
#
## Special cases:
# label_freq("dec.err")              # no lbl_type specified: use default
# label_freq("hi", lbl_type = NULL)  # => NA
# label_freq("unknown fname")        # unknown freq: return fname


## label_prob: Label a known probability (in prob) by pname ----------

label_prob <- function(pname,
                       lbl_type = "default",  # label type: "default", "nam"/"num"/"namnum", "abb"/"min"/"mix" or NULL/NA/"no" (to hide label).
                       lbl_sep = " = ",       # separator: " = " (default), ":\n"
                       cur_prob = prob        # current freq
                       # cur_txt = txt        # current txt (does currently NOT include any probabilities)
                       # accu = accu          # use current accuracy (now included in prob)  +++ here now +++
) {

  ## Initialize:
  p_lbl  <- pname  # initialize (in case of unknown prob)
  p_val  <- NA
  p_type <- NA

  ## Additional parameters (currently fixed):
  n_digits <- 2  # number of decimal digits to round percentage to.

  ## (0) If pname is NA or lbl_type is NA/NULL/"no: ----
  if (is.na(pname) ||
      is.null(lbl_type) || is.na(lbl_type) || tolower(lbl_type) == "no" || tolower(lbl_type) == "none" || tolower(lbl_type) == "nil" )  {

    p_lbl <- NA
    return(p_lbl)  # return NA

  }

  ## Robustness (after handling NA/NULL cases):
  pname <- tolower(pname)

  lbl_type <- tolower(lbl_type)
  if (lbl_type == "val") (lbl_type <- "num")
  if (lbl_type == "namval" || lbl_type == "full" || lbl_type == "all") (lbl_type <- "namnum")
  if (lbl_type == "abbnum") (lbl_type <- "default")

  ## (1) Switch labels: Base label type on type of prob: ----

  ## (a) If lbl_type == "min": Label only key probs:
  if (lbl_type == "min") {

    # Define lists of key probability names:
    key_prob_1 <- c("prev", "ppod", "acc")  # key unconditional prob (3 perspectives)
    key_prob_2 <- c("sens", "spec",  "PPV", "NPV",  "acc-hi", "err-fa")  # key conditional prob (2 per perspective)

    if (pname %in% tolower(c(key_prob_1, key_prob_2))) { # pname is a key probability:

      lbl_type <- "nam"  # use some explicit label: "nam" or "default"

    } else { # not a key probability:

      p_lbl <- NA
      return(p_lbl)  # return NA

    }

  } # if (lbl_type == "min")...

  ## (b) If lbl_type == "mix": Label key probs by name and numeric value, others only by numeric value (num):
  if (lbl_type == "mix") {

    # Define lists of key probability names:
    key_prob_1 <- c("prev", "ppod", "acc")  # key unconditional prob (1 for each of 3 perspectives)
    key_prob_2 <- c("sens", "spec",  "PPV", "NPV",  "acc-hi", "err-fa")  # key conditional prob (2 per perspective)

    if (pname %in% tolower(c(key_prob_1, key_prob_2))) { # pname is a key probability:

      lbl_type <- "default"  # use some label with name and value: "namnum" or "default"

    } else { # not a key probability:

      lbl_type <- "num"  # only numeric value

    }

  } # if (lbl_type == "mix")...


  ## (2) Abbreviated name (i.e., variable name of pname): ----
  if (lbl_type == "abb") {
    p_lbl <- as.character(pname)
    return(p_lbl)
  }


  ## (3) Values: Determine the probability value p_val of prob corresponding to pname: ----
  if (lbl_type != "nam") {

    p_val <- comp_prob_pname(pname, cur_prob = cur_prob)  # use new fn (defined in comp_prob_prob.R)

  }

  ## (4) Compose p_lbl based on lbl_type: ----
  if (lbl_type == "num"){

    # (a) Value only:
    if (is_prob(p_val)) {
      # Label p_val (ONLY) as a percentage:
      p_lbl <- paste0(as.character(as_pc(p_val, n_digits = n_digits)), "%")
    } else {
      p_lbl <- paste0(as.character(p_val))  # Label p_val as is (as number)
    }

  } else if (lbl_type == "namnum"){

    ## (b) Name AND value of prob:
    if (pname == "prev") { p_lbl <- "Prevalence" }
    if (pname == "cprev") { p_lbl <- "1 - prevalence" }

    if (pname == "sens") { p_lbl <- "Sensitivity" }
    if (pname == "spec") { p_lbl <- "Specificity" }
    if (pname == "mirt") { p_lbl <- "Miss rate" }
    if (pname == "fart") { p_lbl <- "False alarm rate" }

    if (pname == "ppod") { p_lbl <- "Proportion positive" }
    if (pname == "cppod"){ p_lbl <- "Proportion negative" }
    if (pname == "pned") { p_lbl <- "Proportion negative" }

    if (pname == "ppv") { p_lbl <- "Positive predictive value" }
    if (pname == "npv") { p_lbl <- "Negative predictive value" }
    if (pname == "fdr") { p_lbl <- "False detection rate" }
    if (pname == "for") { p_lbl <- "False omission rate" }

    # Accuracy (as probability):
    if (pname == "acc") { p_lbl <- "Rate correct" }
    if (pname == "cor") { p_lbl <- "Rate correct" }
    if (pname == "err") { p_lbl <- "Rate incorrect" }

    if (pname == "acc-hi") { p_lbl <- "p(hi | dec.cor)" }  # "Proportion positive correct" (ppcor)
    if (pname == "acc-cr") { p_lbl <- "p(cr | dec.cor)" }  # "Proportion negative correct" (pncor)
    if (pname == "err-mi") { p_lbl <- "p(mi | dec.err)" }
    if (pname == "err-fa") { p_lbl <- "p(fa | dec.err)" }

    # Combine p_lbl (NOT pname) with p_val (from above):
    if (is_prob(p_val)) {
      # Label p_val as a percentage:
      p_lbl <- paste0(p_lbl, lbl_sep, as.character(as_pc(p_val, n_digits = n_digits)), "%")
    } else {
      p_lbl <- paste0(p_lbl, lbl_sep, as.character(p_val))  # Label p_val as is (as number)
    }

  } else if (lbl_type == "nam") {

    ## (c) ONLY the name of prob:

    if (pname == "prev") { p_lbl <- "Prevalence" }
    if (pname == "cprev") { p_lbl <- "1 - prevalence" }

    if (pname == "sens") { p_lbl <- "Sensitivity" }
    if (pname == "spec") { p_lbl <- "Specificity" }
    if (pname == "mirt") { p_lbl <- "Miss rate" }
    if (pname == "fart") { p_lbl <- "False alarm rate" }

    if (pname == "ppod") { p_lbl <- "Proportion positive" }
    if (pname == "cppod"){ p_lbl <- "Proportion negative" }
    if (pname == "pned") { p_lbl <- "Proportion negative" }

    if (pname == "ppv") { p_lbl <- "Positive predictive value" }
    if (pname == "npv") { p_lbl <- "Negative predictive value" }
    if (pname == "fdr") { p_lbl <- "False detection rate" }
    if (pname == "for") { p_lbl <- "False omission rate" }

    # Accuracy (as probability):
    if (pname == "acc") { p_lbl <- "Rate correct" }
    if (pname == "cor") { p_lbl <- "Rate correct" }
    if (pname == "err") { p_lbl <- "Rate incorrect" }

    if (pname == "acc-hi") { p_lbl <- "p(hi | dec.cor)" }  # "Proportion positive correct" (ppcor) / key prob
    if (pname == "acc-cr") { p_lbl <- "p(cr | dec.cor)" }  # "Proportion negative correct" (pncor)
    if (pname == "err-mi") { p_lbl <- "p(mi | dec.err)" }
    if (pname == "err-fa") { p_lbl <- "p(fa | dec.err)" }  # key prob

  } else {  ## "any"/"default":

    ## (d) Any other lbl_type: Use basic pname + p_val as default:

    # Special cases: CHANGE pname to some other default value:

    if (pname == "cprev") {  # if complement of prevalence:
      pname <- "(1 - prev)"           # custom basic name
    }
    if (pname == "cppod" || pname == "pned") {  # if complement of ppod:
      pname <- "(1 - ppod)"           # custom basic name
    }

    # Accuracy (as probability):
    if (pname == "acc") { pname <- "acc" }
    if (pname == "cor") { pname <- "acc" }
    if (pname == "err") { pname <- "err" }

    if (pname == "acc-hi") { pname <- "p(hi|acc)" }  # ppcor / key prob
    if (pname == "acc-cr") { pname <- "p(cr|acc)" }  # pncor
    if (pname == "err-mi") { pname <- "p(mi|err)" }
    if (pname == "err-fa") { pname <- "p(fa|err)" }  # key prob

    # print(p_val)
    # is.numeric(p_val)

    # Combine pname (NOT p_lbl) with p_val (from above):
    if (is_prob(p_val)) {
      # Label p_val as a percentage:
      p_lbl <- paste0(pname, lbl_sep, as.character(as_pc(p_val, n_digits = n_digits)), "%")
    } else {
      p_lbl <- paste0(pname, lbl_sep, as.character(p_val))  # Label p_val as is (as number)
    }

  }

  ## (5) Split/re-format long p_lbl into 2 lines of text: ----
  nchar_max <- 99  # criterium for p_lbl being too long (currently fixed)

  # if p_lbl is too long and it contains a lbl_sep (e.g., " = "):
  if ((nchar(p_lbl) > nchar_max) && any(grep(pattern = lbl_sep, x = p_lbl))) {

    # Split into 2 parts:
    lbl_parts <- unlist(strsplit(p_lbl, split = lbl_sep))
    lbl_part1 <- lbl_parts[1]  # 1st part of p_lbl
    lbl_part2 <- lbl_parts[2]  # 2nd part of p_lbl

    p_lbl <- paste0(lbl_part1, ":\n", lbl_part2)  # Put into 2 lines.
  }

  ## (4) Return p_lbl: ----
  return(p_lbl)

}

## Check:
# label_prob("prev", lbl_type = "default")
# label_prob("prev", lbl_sep = ":\n")
# label_prob("sens", lbl_type = "nam")
# label_prob("spec", lbl_type = "num")
# label_prob("fart", lbl_type = "namnum")
# label_prob("PPV", lbl_type = "namnum")
# label_prob("NPV", lbl_type = "namnum")
## Missing values:
# label_prob("unknown pname")  # unknown prob: return pname
# label_prob("hi", lbl_type = NA)
# label_prob(NA, lbl_type = "stuff") # => NA
## Special cases:
# label_prob("prev", lbl_type = NULL)        # => NA
# label_prob("prev", lbl_type = "no")        # => NA
# label_prob("cprev", lbl_type = "default")  # complement to prev
# label_prob("cprev", lbl_type = "nam")      # complement to prev
# label_prob("cprev", lbl_type = "namnum")   # complement to prev
# label_prob("cppod", lbl_type = "default")
# label_prob("pned", lbl_type = "namnum")
# label_prob("acc", lbl_type = "default")
# label_prob("cor", lbl_type = "nam")
# label_prob("err", lbl_type = "namnum")
## Labels based on prob type:
# label_prob("spec", lbl_type = "min")
# label_prob("fart", lbl_type = "min")
# label_prob("spec", lbl_type = "mix")
# label_prob("fart", lbl_type = "mix")
## Abbreviated names:
# label_prob("spec", lbl_type = "abb")
# label_prob("err-fa", lbl_type = "abb")


## name_prob: Determine the (name of the) prob that links 2 freq ---------

name_prob <- function(freq1, freq2) {

  ## ToDo: This function currently uses default names of freq.
  ##       To generalize it, consider adding user-defined names of txt.

  # (0) Prepare:
  pname <- NA  # initialize

  freq1 <- tolower(freq1)  # all lowercase
  freq2 <- tolower(freq2)

  # (1) by condition (bc):

  # 2 unconditional probabilities:
  if ( (freq1 == "n" & freq2 == "cond.true") ||
       (freq2 == "n" & freq1 == "cond.true") ) { pname <- "prev" }
  if ( (freq1 == "n" & freq2 == "cond.false") ||
       (freq2 == "n" & freq1 == "cond.false") ) { pname <- "cprev" }

  # 4 conditional probabilities:
  if ( (freq1 == "hi" & freq2 == "cond.true") ||
       (freq2 == "hi" & freq1 == "cond.true") ) { pname <- "sens" }
  if ( (freq1 == "mi" & freq2 == "cond.true") ||
       (freq2 == "mi" & freq1 == "cond.true") ) { pname <- "mirt" }

  if ( (freq1 == "cr" & freq2 == "cond.false") ||
       (freq2 == "cr" & freq1 == "cond.false") ) { pname <- "spec" }
  if ( (freq1 == "fa" & freq2 == "cond.false") ||
       (freq2 == "fa" & freq1 == "cond.false") ) { pname <- "fart" }

  # (2) by decision (dc):

  # 2 unconditional probabilities:
  if ( (freq1 == "n" & freq2 == "dec.pos") ||
       (freq2 == "n" & freq1 == "dec.pos") ) { pname <- "ppod" }
  if ( (freq1 == "n" & freq2 == "dec.neg") ||
       (freq2 == "n" & freq1 == "dec.neg") ) { pname <- "cppod" } # aka. "pned"

  # 4 conditional probabilities:
  if ( (freq1 == "hi" & freq2 == "dec.pos") ||
       (freq2 == "hi" & freq1 == "dec.pos") ) { pname <- "PPV" }
  if ( (freq1 == "fa" & freq2 == "dec.pos") ||
       (freq2 == "fa" & freq1 == "dec.pos") ) { pname <- "FDR" }

  if ( (freq1 == "cr" & freq2 == "dec.neg") ||
       (freq2 == "cr" & freq1 == "dec.neg") ) { pname <- "NPV" }
  if ( (freq1 == "mi" & freq2 == "dec.neg") ||
       (freq2 == "mi" & freq1 == "dec.neg") ) { pname <- "FOR" }

  # (3) by accuracy/correspondence (ac):

  # 2 unconditional probabilities:
  if ( (freq1 == "n" & freq2 == "dec.cor") ||
       (freq2 == "n" & freq1 == "dec.cor") ) { pname <- "acc" } # aka. "cor"
  if ( (freq1 == "n" & freq2 == "dec.err") ||
       (freq2 == "n" & freq1 == "dec.err") ) { pname <- "err" } # error rate

  # 4 conditional probabilities:
  if ( (freq1 == "dec.cor" & freq2 == "hi") ||
       (freq2 == "dec.cor" & freq1 == "hi") ) { pname <- "acc-hi" } # in lack of a better name
  if ( (freq1 == "dec.cor" & freq2 == "cr") ||
       (freq2 == "dec.cor" & freq1 == "cr") ) { pname <- "acc-cr" } # in lack of a better name

  if ( (freq1 == "dec.err" & freq2 == "mi") ||
       (freq2 == "dec.err" & freq1 == "mi") ) { pname <- "err-mi" } # in lack of a better name
  if ( (freq1 == "dec.err" & freq2 == "fa") ||
       (freq2 == "dec.err" & freq1 == "fa") ) { pname <- "err-fa" } # in lack of a better name

  # Note: No prob for links between dec.cor OR dec.err and
  #       4 SDT cases (hi, mi, fa, cr).

  # (4) Return:
  return(pname)

}

## Check:
# name_prob("no", "nix")       # => NA
# name_prob("N", "cond.true")  # => "prev"
# name_prob("cond.false", "N") # => "cprev"
#
# name_prob("N", "dec.neg")
# name_prob("dec.pos", "hi")
# name_prob("dec.neg", "cr")
# name_prob("dec.neg", "mi")
#
# name_prob("dec.cor", "N")
# name_prob("dec.err", "N")
#
# label_prob(pname = name_prob("fa", "cond.false"), lbl_type = "default")
# label_prob(pname = name_prob("hi", "dec.pos"), lbl_type = "namnum")
# label_prob(pname = name_prob("N", "dec.err"), lbl_type = "namnum")



## plot_ftype_label: Label the freq type corresponding to fname at (x, y): ----------
plot_ftype_label <- function(fname,               # name of a known freq
                             x, y,                # coordinates
                             ## Optional arguments:
                             suffix = "",         # suffix
                             # pos = NULL,        # pos (NULL = default; 1 = bottom, 2 = left, 3 = top)
                             # offset = 0.5,      # offset, etc.
                             # col = pal["txt"],  # default color
                             ...                  # other (graphical) parameters
){

  ## Initialize ftype_lbl:
  # ftype_lbl <- ""

  ## Determine ftype_lbl:
  ftype_lbl <- paste0(comp_freq_type(fname), suffix)  # determine freq_type corresponding to fname

  ## Plot ftype_lbl:
  text(x, y,
       labels = ftype_lbl,
       xpd = TRUE,    # NA...plotting clipped to device region; T...figure region; F...plot region
       # col = col,   # pass on parameter
       ...)  # other parameters: pos, offset, ...

  ## Return ftype_lbl (as name):
  # return(ftype_lbl)

}

## Check:
# plot(0:1, 0:1, type = "n")  # empty canvas
# plot_ftype_label("N", .1, .9)
# plot_ftype_label("cond.false", .2, .8, suffix = ":", cex = .8)
# plot_ftype_label("dec.pos", .3, .7, col = "red3")
# plot_ftype_label("dec.cor", .7, .3, col = "gold")
# plot_ftype_label("hi", .5, .5, col = "green3")
# plot_ftype_label("hi", .5, .5, col = "steelblue1", pos = 1)
# plot_ftype_label("mi", .5, .5, col = "steelblue2", pos = 2)
# plot_ftype_label("fa", .5, .5, col = "steelblue3", pos = 3)
# plot_ftype_label("cr", .5, .5, col = "steelblue4", pos = 4)

## plot_freq_label: Label the freq corresponding to fname at (x, y): ----------
plot_freq_label <- function(fname,                # name of a known freq
                            x, y,                 # coordinates
                            ## Optional arguments:
                            lbl_type = "nam",     # lbl_type (of label_freq)
                            lbl_sep = " = ",      # lbl_sep  (of label_freq)
                            suffix = "",          # suffix
                            cur_freq = freq,      # current freq
                            cur_txt = txt,        # current txt
                            # pos = NULL,         # pos (NULL = default; 1 = bottom, 2 = left, 3 = top)
                            # offset = 0.5,       # offset, etc.
                            # col = pal["txt"],   # default color
                            ...                   # other (graphical) parameters
){

  ## Initialize f_lbl:
  # f_lbl <- ""

  ## Determine f_lbl:
  f_lbl <- label_freq(fname, lbl_type = lbl_type, lbl_sep = lbl_sep,
                      cur_freq = cur_freq, cur_txt = cur_txt)  # create label corresponding to fname
  f_lbl <- paste0(f_lbl, suffix)  # add suffix

  ## Plot text label:
  text(x, y,
       labels = f_lbl,
       xpd = TRUE,    # NA...plotting clipped to device region; T...figure region; F...plot region
       # col = col,   # pass on parameter
       ...)  # other parameters: pos, offset, ...

  ## Return f_lbl (as character):
  # return(f_lbl)

}

## ToDo: Allow for vectors or lists of lists?

## Check:
# plot(0:1, 0:1, type = "n")  # empty canvas
# plot_freq_label("N", .1, .9)
# plot_freq_label("cond.false", suffix = ": ...", .2, .8, cex = .8)
# plot_freq_label("dec.cor", .3, .7, lbl_type = "namnum", col = pal["cor"])
# plot_freq_label("dec.pos", .4, .6, lbl_type = "nam", col = pal["pos"])
#
# # Local freq object:
# f2 <- comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 100)
# plot_freq_label("N", .5, .5, lbl_type = "namnum", cur_freq = f2)
# plot_freq_label("cond.true", .5, .4, lbl_type = "namnum", cur_freq = f2)
# plot_freq_label("hi", .5, .3, lbl_type = "namnum", cur_freq = f2)
# plot_freq_label("dec.cor", .5, .2, lbl_type = "namnum", cur_freq = f2)

## (B) Boxes: ------
## (a) Plotting boxes: ------

## plot_vbox: Plot a vertical box (x = center, y = bottom) with text label ----------

## Note: plot_vbox only plots provided arguments.
##       It is NOT "smart" by NOT automatically deriving
##       freq and prob labels from global objects!

plot_vbox <- function(box.x,  box.y,    # coordinates x (center) and y (bottom)
                      box.lx, box.ly,   # lengths of box (width and height)
                      ## Text labels:
                      ftype = NA,        # type of freq/box (to be shown as title below box)
                      show.freq = TRUE,  # option to show/hide frequency labels
                      lbl_type = "default", # label type of label_freq: "default" (fname = fnum) or "nam"/"abb"/"num"/"namnum"
                      fname = NA,        # frequency name (corresponding to a color in pal, as character)
                      fnum,              # frequency (as number).  ToDo: Derive fnum from ftype and/OR name!
                      ## Color options:
                      col.fill = grey(.95, .75),  # default color (but derived from name below)
                      col.brd = pal["brd"],
                      col.txt = pal["txt"],
                      ...  # other (graphical) parameters: lwd, cex, ...
) {

  ## (0) Additional parameters (currently fixed):

  n_digits <- 2  # n_digits to which fnum is to be rounded (in lbl_type "num" or "namnum")

  ## Box parameters:

  ## Fill color:
  col.fill <- comp_freq_col(fname)
  # if (fname %in% names(pal)) { # if fname corresponds to a color name in pal
  #  col.fill <- pal[fname]     # use this color to fill box
  #}

  ## Text parameters:
  # col.lbl <- pal["txt"]  # defined in pal
  # cex.lbl <- .90   # scaling factor for text labels
  # cex.lbl.sm <- if (cex.lbl > .50) {cex.lbl - .10} else {cex.lbl}  # slightly smaller than cex.lbl
  # h.shift <- .05   # horizontal shifting of labels
  # v.shift <- .05   # vertical shifting of labels

  ## (1) Plot rect:
  rect(xleft  = (box.x - box.lx/2), ybottom = box.y,
       xright = (box.x + box.lx/2), ytop    = (box.y + box.ly),
       col = col.fill,
       border = col.brd,
       # lwd = box.lwd,
       ...)

  ## (2) Print ftype as box title (below box, optional):
  if (!is.na(ftype)) {  # ftype is specified:

    text(x = box.x, y = box.y,
         labels = ftype,
         pos = 1,       # NULL...center (default), 1...below, 3...above
         # xpd = TRUE,  # NA...plotting clipped to device region; T...figure region; F...plot region
         col = col.txt,
         # cex = cex.lbl.sm,
         ...)

  }

  ## (3) Plot box label (centered in box, optional):
  if (show.freq) {

    # y-coordinate of label:
    mid.y <- box.y + box.ly/2  # y-value of mid point

    # Round fnum to n_digits:
    fnum <- round(as.numeric(fnum), digits = n_digits)  # round fnum (i.e., value displayed, NOT the actual freq value computed!)

    ## Distinguish 2 cases:

    # (A) Use label_freq function (which also reports global freq values):

    ## A1. General case (using all lbl_type options with global freq values):
    # box_lbl <- label_freq(fname, lbl_type = lbl_type, lbl_sep = " = ")
    # ToDo...

    ## A2. Use label_freq only for types without values (to not require/report global freq values):
    if ( is.null(lbl_type) || is.na(lbl_type) || (lbl_type == "no") ) {

      box_lbl <- ""  # no label

    } else if (lbl_type == "nam") {

      box_lbl <- label_freq(fname, lbl_type = "nam")  # long name, no numeric value

    } else if (lbl_type == "abb") {

      # box_lbl <- as.character(fname)  # use fname as box_lbl
      #
      # # If box_lbl contains a dot (.):  Use only the name part after the dot:
      # if (any(grep(pattern = "\\.", x = box_lbl))) {  # if box_lbl contains a dot (.):
      #
      #   nameparts <- unlist(strsplit(box_lbl, split = "\\."))
      #   part_1 <- nameparts[1]  # 1st part of f_lbl
      #   part_2 <- nameparts[2]  # 2nd part of f_lbl
      #   box_lbl <- part_2  # 2nd part (after dot)
      #
      # }

      box_lbl <- label_freq(fname, lbl_type = "abb")  # abbreviated name, no numeric value

    } else if (lbl_type == "num") {

      box_lbl <- paste0(fnum)  # use current fnum value only

    } else { # default (for all other lbl_type values, including "namnum"):

      ## (B) Construct a simple label (based on current values of fname and fnum):

      if (!is.na(fname)) {  # fname is specified:

        box_lbl <- paste0(fname, " = ", fnum)  # use current values: fname = fnum (local values):

      } else { # no fname specified:

        box_lbl <- paste0(fnum)  # use current fnum value only

      }

    } # if (lbl_type == ...)

    # Plot freq label:
    text(x = box.x, y = mid.y,
         labels = box_lbl,
         # pos = NULL,  # NULL...center (default), 1...below, 3...above
         # xpd = TRUE,  # NA...plotting clipped to device region; T...figure region; F...plot region
         col = col.txt,
         # cex = cex.lbl.sm,
         ...)

  } # if (show.freq)...

}

## Check:

## Preparation:
# plot(c(0, 100), c(0, 100), type = "n")  # empty canvas
## Basics:
# plot_vbox(10, 80, 20, 20, fnum = 111)  # no name => default fill color
# plot_vbox(50, 80, 20, 20, fname = "N", fnum = 222)  # no ftype label
# plot_vbox(50, 60, 20, 20, fname = "N", fnum = 222, lbl_type = "nam")  # name only
# plot_vbox(50, 40, 20, 20, fname = "N", fnum = 222, lbl_type = "abb")  # abbreviated name only
# plot_vbox(50, 20, 20, 20, fname = "N", fnum = 222, lbl_type = NA)  # hide label (NA/NULL/"no")
# plot_vbox(80, 80, 20, 20, fname = "cond.true", fnum = 333, ftype = comp_freq_type("cond.true"))
# plot_vbox(10, 50, 30, 20, ftype = "type as box title", fname = "hi", fnum = 444)
# plot_vbox(40, 50, 20, 20, fname = "mi", fnum = 555, lwd = 3, cex = .7, ftype = comp_freq_type("mi"))
## Other cases:
# plot_vbox(70, 50, 20, 20, fname = "asdf", fnum = 667, ftype = comp_freq_type("asdf"))


## Distinguish between 2 separate box plotting functions:
#   1. generic plot_cbox (that plots a box given its CENTER coordinates and format) vs.
#   2. plot_fbox (that determines current freq label/value/color for known freq).

## plot_cbox: Plot a CENTERED box (x = center, y = center) with text label ----------

plot_cbox <- function(x,  y,    # coordinates of box CENTER (x and y)
                      lx, ly,   # lengths of box (width and height)
                      ## Text labels:
                      lbl     = NA,       # main label (in middle)
                      lbl.top = NA,       # title (at top)
                      lbl.bot = NA,       # caption (at bottom)
                      ## Color options:
                      col.fill = grey(.95, .50),  # default fill color
                      col.brd = pal["brd"],       # default border color
                      col.txt = pal["txt"],       # default label color
                      ## Shading options:
                      density = NULL,  # density of shading lines (per in)
                      angle = 45,      # angle of shading lines
                      ...  # other graphical parameters: lwd, cex, adj, ...
) {

  ## (0) Parameters (currently fixed):

  # Compute box coordinates:
  x_left = (x - lx/2)
  x_right = x_left + lx
  y_bottom = (y - ly/2)
  y_top = y_bottom + ly

  ## (1) Plot rectangle:

  rect(xleft = x_left, ybottom = y_bottom, xright = x_right, ytop = y_top,
       col = col.fill,
       border = col.brd,
       density = density,
       angle = angle,
       ...)

  ## (2) Print optional text labels:

  if (!is.na(lbl)) {

    text(x = x, y = y,
         labels = paste0(lbl),
         pos = NULL,    # NULL...center (default), 1...below, 3...above
         # xpd = TRUE,  # NA...plotting clipped to device region; T...figure region; F...plot region
         col = col.txt,
         ...)

  }

  if (!is.na(lbl.top)) {

    text(x = x, y = y_top,
         labels = paste0(lbl.top),
         pos = 3,       # NULL...center (default), 1...below, 3...above
         # xpd = TRUE,  # NA...plotting clipped to device region; T...figure region; F...plot region
         col = col.txt,
         ...)

  }

  if (!is.na(lbl.bot)) {

    text(x = x, y = y_bottom,
         labels = paste0(lbl.bot),
         pos = 1,       # NULL...center (default), 1...below, 3...above
         # xpd = TRUE,  # NA...plotting clipped to device region; T...figure region; F...plot region
         col = col.txt,
         ...)

  }

}

## Check:
#
# plot(0:1, 0:1, type = "n", xlab = "x-axis", ylab = "y-axis",
#     xlim = c(0, 10), ylim = c(0, 6))
#
# plot_cbox(1, 5, 1, 1)  # default color, no text labels
#
# plot_cbox(3, 5, 1, 1, density = 15)  # with diagonal lines
#
# plot_cbox(5, 5, 1, 1, lbl = "Label", lbl.top = "Title:", lbl.bot = "Caption.")  # add text labels
#
# plot_cbox(7, 5, 1, 1, lbl = "Label", lbl.top = "Title:", lbl.bot = "Caption.",
#           cex = .75, font = 2,                                # add text options
#           col.fill = "gold", col.brd = "steelblue", lwd = 3)  # add color options
#
# plot_cbox(9, 5, 1, 1, lbl = "Label", lbl.top = "Title:", lbl.bot = "Caption.",
#           cex = .75, font = 1,                                # add text options
#           col.fill = "firebrick", col.brd = "firebrick", lwd = 1,
#           density = 15)  # add color options


## plot_fbox: Plot a known frequency (freq) as a box ----------

plot_fbox <- function(fname,   # name of a known frequency (freq)
                      x,  y,   # coordinates of box CENTER (x and y)
                      lx, ly,  # lengths of box (width and height)
                      ## Optional arguments:
                      scale_lx = 1,  # scaling factor for x-widths
                      # scale_ly = 1,  # scaling factor for y-widths
                      ## Text labels:
                      # lbl     = NA,        # label (in middle): freq
                      # lbl.top = NA,        # title (at top)
                      # lbl.bot = NA,        # caption (at bottom)
                      lbl_type = "default",  # type of freq label
                      lbl_sep = " = ",       # label separator (" = ", ":\n")
                      show_type = FALSE,     # option to show/hide f_type label (at bottom)
                      cur_freq = freq,       # current freq
                      cur_txt = txt,         # current txt
                      ## Color options:
                      col.fill = col,  # if missing, color of fname freq is derived below
                      # col.brd = pal["brd"],
                      # col.txt = pal["txt"],
                      ...  # other graphical parameters: lwd, cex, pos, ...
) {

  # Initialize:
  f_val <- NA
  f_type <- NA
  f_col <- NA
  f_lbl <- NA
  bot_lbl <- NA

  if (fname %in% names(cur_freq)) { # if fname corresponds to named frequency in cur_freq:

    # Derive current values corresponding to cur_freq:
    ix <- which(names(cur_freq) == fname)  # index in cur_freq

    # (a) Value of frequency in cur_freq:
    f_val <- cur_freq[ix]

    # (b) Type of frequency:
    if (show_type) {
      f_type <- comp_freq_type(fname)  # use helper function (defined in init_freq_num.R)
    }

    # (c) Color of frequency box:
    if (missing(col.fill)) {  # no col.fill has been specified:
      # if (is.na(col)) {  # no col has been specified:
      f_col <- comp_freq_col(fname)  # determine default f_col corresponding to fname in cur_freq and pal
    } else {
      f_col <- col.fill  # use the color specified in function call
      # f_col <- col  # use the color specified in function call
    }
    # print(f_col)

    # (d) Label of frequency name and/or value:
    f_lbl <- label_freq(fname = fname, lbl_type = lbl_type, lbl_sep = lbl_sep,
                        cur_freq = cur_freq, cur_txt = cur_txt)
    # print(f_lbl)

    # (e) Label of frequency type:
    if (show_type) { bot_lbl <- paste0(f_type) }

    # (e) Plot corresponding cbox with values of fname freq:
    plot_cbox(x = x,
              y = y,
              lx = (lx * scale_lx),
              ly = ly,
              # lbl = paste0(fname, " = ", f_val),
              lbl = f_lbl,
              lbl.bot = bot_lbl,
              col.fill = f_col,
              ...)

  } else {  # fname is NOT a known freq:

    # (a) Fill color of frequency box:
    if (missing(col.fill)) {  # no col.fill has been specified:
      f_col <- grey(.95, .50)  # default fill color
    } else {
      f_col <- col.fill  # use the color specified in function call
    }
    # print(f_col)

    # (b) Plot cbox with default settings:
    plot_cbox(x = x,
              y = y,
              lx = (lx * scale_lx),
              ly = ly,
              lbl = paste0(fname),
              lbl.bot = bot_lbl,
              col.fill = f_col,
              ...)

  } # if (fname %in% names(freq))...

}

# ## Check:
# plot(0:1, 0:1, type = "n", xlab = "x-axis", ylab = "y-axis",
#        xlim = c(0, 10), ylim = c(0, 10))  # empty canvas
# # Aspect ratio of current plot:
# plot_xy <- par("pin")                # use par("pin") OR dev.size("in")
# plot_ratio <- plot_xy[1]/plot_xy[2]  # current aspect ratio
# scale_x <- 1/plot_ratio              # multiplicative correction factor (for x-widths)
#
# # Basics:
# plot_fbox(fname = "N1", 3, 9, 2, 2)  # square (appears as rectangle in rectangular plot area)
# plot_fbox(fname = "N2", 7, 9, 2, 2, scale_lx = scale_x)  # square (appears as square in any plot area)
#
# # Plot freq boxes:
# plot_fbox(fname = "N", 5, 5, 1, 2/3)
# plot_fbox(fname = "N", 5, 5, 3/2, 2/3, lbl_type = "namnum", lbl_sep = ":\nN = ")
# plot_fbox(fname = "cond.true", 3, 4, 2, 2/3, show_type = TRUE)
# plot_fbox(fname = "cond.false", 7, 4, 2, 2/3, lbl_type = "nam", show_type = FALSE)
# plot_fbox(fname = "hi", 2, 3, 1, 2/3, lbl_type = "nam", show_type = TRUE)
# plot_fbox(fname = "mi", 4, 3, 1, 2/3, lbl_type = "num")
# plot_fbox(fname = "fa", 6, 3, 1, 2/3, lbl_type = "namnum", show_type = FALSE)
# plot_fbox(fname = "cr", 8, 3, 1, 2/3)
# plot_fbox(fname = "dec.pos", 3, 2, 2, 2/3, lbl_type = "namnum")
# plot_fbox(fname = "dec.neg", 7, 2, 2, 2/3)
# plot_fbox(fname = "dec.cor", 3, 1, 2, 2/3)
# plot_fbox(fname = "dec.err", 7, 1, 2, 2/3, lbl_type = "namnum")
# plot_fbox(fname = "N", 5, 1, 1, 2/3, col = "green2", col.brd = "red3", cex = .6, lwd = 3)
#
# # Local freq object:
# f2 <- comp_freq_prob(prev = .5, sens = .5, spec = .5, N = 100) # create f2
# plot_fbox(fname = "N", 5, 8, 1, 1, lbl_type = "def", cur_freq = f2)
# plot_fbox(fname = "cond.true", 3, 7, 1, 1, lbl_type = "def", cur_freq = f2)
# plot_fbox(fname = "hi", 2, 6, 1, 1, lbl_type = "def", cur_freq = f2)
# plot_fbox(fname = "mi", 4, 6, 1, 1, lbl_type = "def", cur_freq = f2)
# plot_fbox(fname = "dec.cor", 3, 5, 1, 1, lbl_type = "def", cur_freq = f2)
#
# # Arbitrary boxes (with unknown freq): ###
# plot_fbox(fname = "unknown_freq", 9, 2, 1, 2/3)  # unknown fname (freq) with defaults
# plot_fbox(fname = "other_freq", 9, 1, 1, 2/3, col = "gold", cex = .7, font = 2)




# +++ here now +++ ----


## (b) Computing box dimensions (width lx): -------


## comp_freq_fbox: Compute freq value of fbox (based on its name) --------

comp_freq_fbox <- function(fbox) {

  f_val  <- NA
  fname <- NA

  if (is.list(fbox) && isTRUE(exists(fbox$name)) ) {

    fname <- fbox$name

    if (tolower(fname) %in% tolower(names(freq))) { # if fname corresponds to named frequency in freq:

      # Derive current value corresponding to fname in freq:
      ix <- which(tolower(names(freq)) == tolower(fname))  # index of fname in freq
      f_val <- freq[ix]  # current freq value

      # Type of frequency:
      # f_type <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

    } # if (fname %in% names(freq)...

  }

  return(as.numeric(f_val))

}

## Check:
# box_N  <- make_box("N",  5, 5, 4, 1)  # define box for N
# box_hi <- make_box("hi", 2, 3, 1, 1)  # ...            hi
# box_mi <- make_box("mi", 4, 3, 1, 1)  # ...            mi
# box_fa <- make_box("fa", 6, 3, 1, 1)  # ...            fa
# box_cr <- make_box("cr", 8, 3, 1, 1)  # ...            cr
#
# comp_freq_fbox(box_N)  == freq$N  # should be TRUE
# comp_freq_fbox(box_hi) == freq$hi # should be TRUE
# comp_freq_fbox("no_box")  # NA
# comp_freq_fbox(1)         # NA
#
# # str(box_N)
#
# # Box as list:
# is.list(box_N)     # TRUE
# length(box_N)      # 5
# length(box_N[1])   # 1
# is.list(box_N[1])  # TRUE
# length(box_N[[1]]) # 1!
# # box_N[[1]] == box_N[1]
# is.list(box_N[[1]]) # FALSE
#
# # List of boxes:
# boxes <- list(box_hi, box_mi, box_fa, box_cr)
# is.list(boxes)     # TRUE
# length(boxes)      # 4
# boxes[[2]]         # 2nd box of boxes
# length(boxes[-1])  # 3
# is.list(boxes[1])  # TRUE
# length(boxes[[1]]) # 5!
#
# box_freqs <- unlist(lapply(X = boxes, FUN = comp_freq_fbox))
# box_freqs
#
# ix_box_max_freq <- which(box_freqs == max(box_freqs))
# ix_box_max_freq
#
# dec <- order(box_freqs, decreasing = TRUE)
#
# plot(0:10, type = "n")
# lapply(X = boxes[dec], FUN = plot)


## plot_fbox_list: Plot a list of fboxes in some order --------

plot_fbox_list <- function(fboxes, ...) {
  # Plot a list of fboxes in some order:

  if ( is.list(fboxes) && (length(fboxes) > 0) && is.list(fboxes[[1]]) ) { # length(fboxes[[1]] == 5) ) { # fboxes is a list of 1+ fboxes:

    ## (A) Plot fboxes in given order (from 1, 2, ...):

    ## (a) with recursive while:
    # plot(fboxes[[1]], ...)  # plot 1st box of list
    # fboxes <- fboxes[-1]  # remove 1st box from list

    ## (b) with lapply:
    # lapply(X = fboxes, FUN = plot, ...)    # plot all in dec_order


    ## (B) Plot fboxes by decreasing frequency (from largest to smallest):

    ## Determine order:
    box_freqs <- unlist(lapply(X = fboxes, FUN = comp_freq_fbox))  # get freq of all fboxes (with utility function)
    decr_order <- order(box_freqs, decreasing = TRUE)  # order to plot boxes

    ## (a) with recursive while:
    # ix <- which(box_freqs == max(box_freqs))  # get ix of the box with maximum freq
    # if (length(ix) > 1) { ix <- ix[1] }  # if ix is no scalar (i.e., fboxes has multiple freq max), use 1st element of ix.
    # plot(fboxes[[ix]], ...) # plot box ix
    # fboxes <- fboxes[-ix]   # remove box ix from list

    ## (b) with lapply:
    lapply(X = fboxes[decr_order], FUN = plot, ...)    # plot all in dec_order

  } # if/while ...

  if (is.list(fboxes) && is.list(fboxes[[1]]) == FALSE) { # 1st element is NO list/box (i.e., fboxes is only 1 fbox):

    plot(fboxes, ...)

  }

}


## Check:
# box_N  <- make_box("N",  5, 5, 4, 1)  # define box for N
# box_hi <- make_box("hi", 2, 3, 1, 1)  # ...            hi
# box_mi <- make_box("mi", 4, 3, 1, 1)  # ...            mi
# box_fa <- make_box("fa", 6, 3, 1, 1)  # ...            fa
# box_cr <- make_box("cr", 8, 3, 1, 1)  # ...            cr
#
# comp_freq_fbox(box_N)  == freq$N  # should be TRUE
# comp_freq_fbox(box_hi) == freq$hi # should be TRUE
# comp_freq_fbox("no_box")  # NA
# comp_freq_fbox(1)         # NA
#
# # Box as list:
# is.list(box_N)  # TRUE
# length(box_N)   # 5
# length(box_N[[1]]) # 1!
#
# # List of boxes:
# boxes <- list(box_hi, box_mi, box_fa, box_cr)
# is.list(boxes)  # TRUE
# length(boxes)   # 4
# boxes[[2]] # 1st box of boxes
# length(boxes[-1]) # 3
# is.list(boxes[1]) # TRUE
# length(boxes[[1]]) # 5!
#
# box_freqs <- unlist(lapply(X = boxes, FUN = comp_freq_fbox))
# box_freqs
# ix_box_max_freq <- which(box_freqs == max(box_freqs))
# ix_box_max_freq
# length(ix_box_max_freq)
#
# unlist(lapply(X = list(box_N, box_hi), FUN = comp_freq_fbox)) #
# sort(c(1, 3, 2))


## comp_lx: Compute scaled lx given ly, mfactor mf and correction factor corf ------

comp_lx <- function(ly, mf = 1, corf = 1) {
  # Scale fbox width lx given its height ly,
  # a multiplicative factor mf (default: mf = 1),
  # and an optional correction factor corf:

  lx <- NA

  lx <- (ly * mf * corf)

  return(lx)

}

## Check:
# comp_lx(ly = c(0, 1, NA, 3, NULL, 5), 2, 10) # =>  0  20  NA  60 100 (i.e., NULL dropped)


## comp_lx_fbox: Compute scaled length lx of fbox (fname) given f/p scale: ------

comp_lx_fbox <- function(fname, len_N,
                         cur_freq = freq,
                         cur_prob = prob,
                         scale = "f"
) {
  # Compute a scaled length lx of an fbox based on its name (fname),
  # some current population length len_N, current population size N,
  # and current type of scale ("f" or "p") so that:
  # lx == scaled len_N (i.e., all lx on a level sum to len_N).

  lx  <- NA
  fval <- NA
  pval <- NA

  # (1) get the current value (p/f) corresponding to fname:

  if (scale == "p") {

    # (1) scale by exact probability:

    # (a) Compute current probability val of frequency (named by fname) from current prob values:
    pval <- comp_prob_fname(fname, cur_prob = cur_prob)

    # (b) Scale len_N by probability val:
    lx <- pval * len_N

  } else {  # scale = "f" or any other scale:

    # (2) scale by current freq cur_freq (rounded or non-rounded):
    if (tolower(fname) %in% tolower(names(cur_freq))) { # if fname corresponds to named frequency in freq:

      # (a) Get current freq value corresponding to fname in freq:
      ix <- which(tolower(names(cur_freq)) == tolower(fname))  # index of fname in cur_freq
      fval <- cur_freq[ix]  # current freq value
      fval <- as.numeric(fval)  # ensure that fval is numeric

      # Type of frequency:
      # f_type <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

    } # if (fname %in% names(cur_freq)...

    # (b) Scale len_N by the ratio of frequencies fval/N:
    lx <- (fval/cur_freq$N) * len_N

  } # if (scale...)

  return(lx)

}

## Check:
# comp_lx_fbox("N",  len_N = 100)  # => 100 = lN
# comp_lx_fbox("cond.true", len_N = 100)
# comp_lx_fbox("hi", len_N = 100)  # => hi/lN
#
# comp_lx_fbox("hi", len_N = 100, scale = "f")  # => freq of hi/lN
# comp_lx_fbox("hi", len_N = 100, scale = "p")  # => p(hi) x lN
# comp_lx_fbox("hi", len_N = 100, scale = "x")  # => as in "f" (default)
#
# comp_lx_fbox("xx", len_N = 100)  # => NA

## ToDo: Vectorize comp_lx_fbox (to allow computing many lx values at once).


## comp_ly_fsqr: Compute scaled height ly of fsqr (fname) given f/p scale: ------

comp_ly_fsqr <- function(fname, area_N, N = freq$N, scale = "f") {
  # Compute a scaled height ly of an frequency square (fsqr) based on its name (fname),
  # the current population area_N, current population size N,
  # and current type of scale ("f" or "p") so that:
  # ly ^ 2 == scaled area_N (i.e., all areas ly^2 on a level sum to area_N).

  ly  <- NA
  val <- NA

  # (1) get the current value (p/f) corresponding to fname:

  if (scale == "p") {

    # (1) scale by exact probability:

    # (a) Compute current probability val of freq (named by fname) from current prob values:
    val <- comp_prob_fname(fname)

    # (b) Scale area_N by probability val so that ly^2 == (val * area_N):
    ly <- sqrt(val * area_N)

  } else {  # scale == "f" or any other scale:

    # (2) scale by current freq (rounded or non-rounded):
    if (tolower(fname) %in% tolower(names(freq))) { # if fname corresponds to named frequency in freq:

      # (a) Get current freq value corresponding to fname in freq:
      ix <- which(tolower(names(freq)) == tolower(fname))  # index of fname in freq
      val <- freq[ix]  # current freq value
      val <- as.numeric(val)  # ensure that val is numeric

      # Type of frequency:
      # f_type <- comp_freq_type(fname)  # see helper function (defined in init_freq_num.R)

    } # if (fname %in% names(freq)...

    # (b) Scale area_N by ratio of frequencies val/N so that ly^2 == (val/N * area_N):
    ly <- sqrt(val/N * area_N)

  } # if (scale...)

  return(ly)

}

## Check:
# comp_ly_fsqr("N", area_N = 100)  # => 10
# comp_ly_fsqr("cond.true", area_N = 100)
#
# a_N <- rnorm(1, 100, 10)  # random area a_N
#
# hi_ly <- comp_ly_fsqr("hi", area_N = a_N, scale = "f")
# mi_ly <- comp_ly_fsqr("mi", area_N = a_N, scale = "f")
# fa_ly <- comp_ly_fsqr("fa", area_N = a_N, scale = "f")
# cr_ly <- comp_ly_fsqr("cr", area_N = a_N, scale = "f")
# all.equal((hi_ly^2 + mi_ly^2 + fa_ly^2 + cr_ly^2), a_N)  # should be TRUE.
#
# hi_ly <- comp_ly_fsqr("hi", area_N = a_N, scale = "p")
# mi_ly <- comp_ly_fsqr("mi", area_N = a_N, scale = "p")
# fa_ly <- comp_ly_fsqr("fa", area_N = a_N, scale = "p")
# cr_ly <- comp_ly_fsqr("cr", area_N = a_N, scale = "p")
# all.equal((hi_ly^2 + mi_ly^2 + fa_ly^2 + cr_ly^2), a_N)  # should be TRUE.
#
# comp_ly_fsqr("xx", area_N = 100) # => NA

## ToDo: Vectorize comp_ly_fsqr (to allow computing many ly values at once).




## (3) Links: ------
## plot_line: Plot an (arrow) line between 2 points (with optional text label): ------
plot_line <- function(x0, y0, x1, y1,      # coordinates of p1 and p2
                      # lty = 1, lwd = 1,                   # line options
                      pt_pch = 21, pt_cex = 1, pt_lwd = 1,  # point options
                      arr_code = 0,         # 0: none, 1-3: single/double V, 4-6: single/double T, -1 to -3: single/double points.
                      ## Optional text label:
                      lbl = NA,             # string for text label
                      lbl.x = (x0 + x1)/2,  # x-coord of label (default in middle)
                      lbl.y = (y0 + y1)/2,  # y-coord of label (default in middle)
                      lbl.pos = NULL,       # pos (NULL = default, 1 = left, 2 = top, etc.)
                      lbl.off = .75,        # offset of text label
                      ## Colors:
                      col.fill = pal["brd"], # if missing, color of fname freq is derived below
                      col.brd =  pal["brd"],
                      col.txt =  pal["txt"],
                      # lbl.cex = 1,        # size of text label
                      ...                   # other graphical parameters: lwd, cex, pos, ...
) {

  ## (0) Preparations:

  # ## Determine col.fill:
  # if (missing(col.fill)) {  # no color was specified:
  #   col.fill <- pal["brd"] # default fill color
  # } else {
  #   col.fill <- col  # use the color specified in function call
  # }
  #
  # print(col.fill)

  arrow <- TRUE # FALSE # initialize

  ## (1) Draw an arrow between both points:

  if (arr_code > 0) {  # V- or T-shaped arrows:

    if (arr_code <= 3) {
      # Draw V-shape arrow between both points:
      arrows(x0, y0, x1, y1,
             length = .10, angle = 45/2, code = arr_code,    # V shape (small)
             # length = .08, angle = 90, code = arr_code,  # T shape
             col = col.fill,
             ...)  # lty, lwd, ...
    }
    else  if (arr_code > 3) {
      # Draw T-shape arrow between both points:
      arrows(x0, y0, x1, y1,
             # length = .10, angle = 45/2, code = arr_code,    # V shape (small)
             length = .08, angle = 90, code = (arr_code - 3),  # T shape
             col = col.fill,
             ...)  # lty, lwd, ...
    }


  } else { # no arrow heads, but point symbols on line end:

    ## Draw a line with 2 points at line ends:
    arrows(x0, y0, x1, y1,
           length = 0, angle = 0, code = 0,       # no arrows
           col = col.fill,
           ...)  # lty, lwd, ...

    if (arr_code < 0) {  # draw points:

      if (arr_code == -1 || arr_code == -3) {
        points(x0, y0, pch = pt_pch, cex = pt_cex,    # 1st point
               lwd = pt_lwd, col = col.brd, bg = col.fill)
      }

      if (arr_code == -2 || arr_code == -3) {
        points(x1, y1, pch = pt_pch, cex = pt_cex,    # 2nd point
               lwd = pt_lwd, col = col.brd, bg = col.fill)
      }

    }

  } # if (arr_code ...

  ## (2) Optional text label: ------

  if (!is.na(lbl.x)) { # if lbl.x exists:

    ## Text label:
    text(lbl.x, lbl.y,
         labels = lbl,
         col = col.txt,  # text color
         pos = lbl.pos,
         offset = lbl.off,
         ...)  # cex, ...
  }

}

### Check:
# plot(0:10, 0:10, type = "n")  # empty canvas
# # (1) line without labels:
# plot_line(0, 10, 9, 10)  # basic line (without label)
# plot_line(0, 9.5, 9, 9.5, arr_code = 0)  # no arrow (without label)
# plot_line(0, 9, 9, 9, arr_code = 1)  # left arrow (without label)
# plot_line(0, 7, 9, 7, arr_code = 2)  # right arrow (without label)
# plot_line(0, 5, 9, 5, arr_code = 3)  # double arrow (without label)
# plot_line(0, 8, 9, 8, arr_code = -1) # arrow with points (without label)
# plot_line(0, 6, 9, 6, arr_code = -2) # arrow with points (without label)
# plot_line(0, 4, 9, 4, arr_code = -3) # arrow with points (without label)
# plot_line(0, 8.5, 9, 8.5, arr_code = 4) # arrow with points (without label)
# plot_line(0, 6.5, 9, 6.5, arr_code = 5) # arrow with points (without label)
# plot_line(0, 4.5, 9, 4.5, arr_code = 6) # arrow with points (without label)
# #
# # (2) line with labels:
# plot_line(0, 3, 9, 3, arr_code = 3,
#           lbl = "Label 1", cex = .8, lty = 1, lwd = 1)  # text label (on line) and options
# plot_line(0, 2, 9, 2, lbl = "Label 2", arr_code = -3,
#           lbl.pos = 4, lbl.off = 1,
#           col.fill = "firebrick", col.txt = "forestgreen",
#           font = 2, cex = .8)  # basic with raised text label
# plot_line(0, 1, 9, 9,  arr_code = -3,
#           pt_pch = 22, pt_cex = 2, pt_lwd = 2,  # point paramters
#           # Text label (with options):
#           lbl.x = 10, lbl.y = 9,
#           lbl = "Some label\nthat takes\nmultiple (3) lines",
#           lbl.pos = NULL, lbl.off = 0,
#           # Colors:
#           col = "gold", col.brd = "steelblue", col.txt = "steelblue",
#           cex = .7, lty = 2, lwd = 2) # grapical parameters
# # (3) probability lines:
# plot_line(1, 9, 9, 9, arr_code = -3, lbl = label_prob("prev"),
#           col.fill = comp_freq_col("cond.true"), col.txt = NA,
#           lbl.pos = 3, cex = .8)  # horizontal
# # vertical labels:
# plot_line(2, 0, 2, 10, arr_code = -3, lbl = label_prob("PPV"),
#           col.fill = pal["ppv"], col.txt = pal["ppv"],
#           srt = 90, lbl.pos = 2, lbl.off = .5, adj = .5, cex = .8)  # vertical (long line)
# plot_line(3, 4, 3, 5, arr_code = -3, lbl = label_prob("PPV"),
#           col.fill = pal["ppv"], col.txt = pal["ppv"],
#           srt = 90, lbl.pos = 2, lbl.off = .5, adj = 0, cex = .8)  # short (label too low!)
# plot_line(4, 4, 4, 5, arr_code = -3, lbl = label_prob("PPV"),
#           col.fill = pal["ppv"], col.txt = pal["ppv"],
#           srt = 90, lbl.pos = 3, lbl.off = .5, adj = 0, cex = .8)  # short (better, but not perfect)
# # diagonal:
# plot_line(4, 1, 8, 10, arr_code = -3, lbl = label_prob("NPV", lbl_type = "namnum"),
#           col.fill = pal["npv"], col.txt = pal["npv"],
#           srt = 0, lbl.pos = 2, lbl.off = .5, adj = 0, cex = .8)  # diagonal

## plot_arrs: Plot multiple (n_arr) arrows along a line: --------
##      Note: Obsolete, as plot_line (defined above) is more flexible.

plot_arrs <- function(x0, y0, x1, y1,       # coordinates
                      n_arr = 2,            # number of arrows to draw
                      l_arr = .10,          # length of arrows to draw
                      a_arr = 45/2,         # angle of arrows to draw
                      ## Optional label:
                      lbl = NA,         # string for text label
                      lbl.x = (x0 + x1)/2,  # x-coord of label (default in middle)
                      lbl.y = (y0 + y1)/2,  # y-coord of label (default in middle)
                      pos = NULL,           # pos (NULL = default; 1 = bottom, 2 = left, 3 = top)
                      offset = 1,           # offset, etc.
                      ...                   # other graphical parameters
)
{
  ## (0) Draw line from p1 to p2: ----

  # lines(c(x0, x1), c(y0, y1), ...)


  ## (1) Draw n_arr arrows: ----

  # Split line into n_arr + 1 segments:
  Ax = seq(x0, x1, length = n_arr + 1)
  Ay = seq(y0, y1, length = n_arr + 1)

  # Loop to draw all arrows:
  for (i in 1:n_arr)
  {
    arrows(Ax[i], Ay[i], Ax[i + 1], Ay[i + 1],
           length = l_arr, angle = a_arr, code = 2, # arrow type: V or T?
           ...)
  }

  ## (3) Optional text label: ------

  if (!is.na(lbl.x)) { # if lbl.x exists:

    # Parameters:
    # lbl.cex = 1          # size of text label

    ## Text label:
    text(lbl.x, lbl.y,
         labels = lbl,
         # col = col,
         # cex = lbl.cex,
         pos = pos, offset = offset,
         ...)
  }

}

## Check:
# plot(0:1, 0:1, type = "n") # 2 points
# plot_arrs(0, 0, 1, 0, col = "red3")  # 2 arrows, no text
# plot_arrs(0, .1, 1, .1, col = "grey", lbl = "Label 0")
# plot_arrs(0, .2, 1, .2, col = "green3", lbl = "Label 1", pos = 3)
# plot_arrs(0, .3, 1, .5, col = "blue3",
#             n_arr = 3, l_arr = .25, a_arr = 20,
#             lbl = "3 arrows", pos = 3, lwd = 2)
# plot_arrs(0, .4, 1, .9, col = "black", lbl = "Label 3\nis a longer\nand wider label\nin smaller font", pos = 3, offset = 2, cex = .8)


## plot_link: Plot link between 2 boxes (given 2 boxes and pos values, using plot_line) ----------

## Note: If boxes are 2 known freq and name_prob returns a known prob,
##       then label_prob is used to automatically generate a p_lbl as lbl.

plot_link <- function(box1, box2,                # 2 boxes
                      pos1 = NULL, pos2 = NULL,  # 2 positions: NULL = center, bltr
                      lbl = NA,                  # lbl (derived automatically, if NA)
                      lbl_type = "default",      # lbl_type ("default", "nam", "num", "namnum")
                      lbl_sep = " = ",           # label separator (" = ", ":\n")
                      ...) {

  # (1) Determine link coordinates:

  # 1st point:
  if (is.null(pos1) || pos1 == 0) {
    x1 <- box1$x  # x in center of box1
    y1 <- box1$y  # y in center of box1
  } else if (pos1 == 1) {
    x1 <- box1$x              # x in center of box1
    y1 <- box1$y - box1$ly/2  # y at bottom of box1
  } else if (pos1 == 2) {
    x1 <- box1$x - box1$lx/2  # x at left of box1
    y1 <- box1$y              # y in center of box1
  } else if (pos1 == 3) {
    x1 <- box1$x              # x in center of box1
    y1 <- box1$y + box1$ly/2  # y at top of box1
  } else if (pos1 == 4) {
    x1 <- box1$x + box1$lx/2  # x at right of box1
    y1 <- box1$y              # y in center of box1
  } else { # default:
    x1 <- box1$x  # x in center of box1
    y1 <- box1$y  # y in center of box1
  }

  # 2nd point:
  if (is.null(pos2) || pos2 == 0) {
    x2 <- box2$x  # x in center of box2
    y2 <- box2$y  # y in center of box2
  } else if (pos2 == 1) {
    x2 <- box2$x              # x in center of box2
    y2 <- box2$y - box2$ly/2  # y at bottom of box2
  } else if (pos2 == 2) {
    x2 <- box2$x - box2$lx/2  # x at left of box2
    y2 <- box2$y              # y in center of box2
  } else if (pos2 == 3) {
    x2 <- box2$x              # x in center of box2
    y2 <- box2$y + box2$ly/2  # y at top of box2
  } else if (pos2 == 4) {
    x2 <- box2$x + box2$lx/2  # x at right of box2
    y2 <- box2$y              # y in center of box2
  } else { # default:
    x2 <- box2$x  # x in center of box1
    y2 <- box2$y  # y in center of box1
  }

  # (2) Check if no lbl exists and link is a known prob.
  #     If so, label it accordingly:
  if (!is.na(lbl)) {  # lbl is specified:

    # (a) plot line with the current lbl:
    plot_line(x1, y1, x2, y2, lbl = lbl, ...)

  } else {  # lbl is NA: Check whether link is between 2 freq boxes with a known prob:

    pname <- name_prob(box1$name, box2$name)  # try name_prob on box names (freq)!

    if (!is.na(pname)) {  # A pname is found (not NA)/prob is known:

      p_lbl <- label_prob(pname, lbl_type, lbl_sep)  # make p_lbl from label_prob

      # (b) plot line with this p_lbl:
      plot_line(x1, y1, x2, y2, lbl = p_lbl, ...)

    } else {  # NO pname was found by name_prob:

      # (c) plot line as is:
      plot_line(x1, y1, x2, y2, ...)

    }

  }

}

## Check:
# ## Define some boxes:
# box_b1 <- make_box("1st_box", 1, 9, 2, 2)   # 1st box with an arbitrary label
# box_b2 <- make_box("2nd_box", 2, 3, 2, 3)   # 2nd box ...
# box_N  <- make_box("N", 4, 9, 2, 2)         # box with known freq label and type
# box_ct <- make_box("cond.true", 8, 7, 3, 2) # ...
# box_hi <- make_box("hi", 7, 2, 2, 2)        # ...
#
# ## Prepare canvas:
# plot(c(0, 10), c(0, 10), type = "n")  # empty canvas
# ## Plot boxes:
# plot(box_b1)  # plot box with arbitrary label (and default color)
# plot(box_b2, col = "skyblue", cex = 2/3, font = 2)  # plot box with arbitrary label (and specific color)
# plot(box_N)
# plot(box_ct)
# plot(box_hi)  # plot box with known freq label (and type, color, etc.)
#
# ## Link positions:
# # plot_link(box_b1, box_b2, 0, 0)  # 0-0: link from center to center
# # plot_link(box_b1, box_b2, 2, 2)  # 2-2: link from left to left
# # plot_link(box_b1, box_b2, 1, 3)  # 1-3: link from bottom to top
# # plot_link(box_b1, box_b2, 3, 1)  # 3-1: link from top to bottom
# # plot_link(box_b1, box_b2, 4, 4)  # 1-3: link from right to right
#
# ## Link options:
# ## (a) Link 2 freq boxes with a known prob:
# plot_link(box_N, box_ct, 4, 3, lbl.pos = 3, cex = .8, arr_code = -2)
# plot_link(box_N, box_ct, 4, 2, lbl = "given label", lbl.pos = 1, cex = .8)
# plot_link(box_ct, box_hi, 1, 3, arr_code = -3, col.fill = pal["hi"],
#           lbl_type = "namnum", lbl.pos = NULL, col.txt = pal["hi"], cex = .8)
#
# ## (b) Link 2 boxes with NO known prob:
# plot_link(box_b2, box_ct, 4, 2)  # no label
# plot_link(box_N, box_hi, 1, 2, arr_code = -3,
#           lbl = "given label in color",
#           lbl.pos = 2, cex = .8,
#           col.txt = "steelblue", col.fill = "sienna2", lwd = 3)

## Note:
## Functionality included in plot_link: plot_plink/plot_prob:
## plot_plink/plot_prob: Link 2 boxes of 2 known frequencies
##                       and label link by using name_prob, label_prob, plot_link...
## 4 steps:
## 1. Get names of freq1 and freq2 from 2 boxes
## 2. Call pname <- name_prob(freq1, freq1) to get pname
## 3. Call p_lbl <- label_prob(pname) to get label
## 4. Call plot_link with p_lbl as lbl



## (4) Define and plot margin labels: ----------


## (a) make_freq_lbl: Label current frequency values ------

make_freq_lbl <- function(hi, mi, fa, cr) {

  lbl <- ""  # initialize

  N <- (hi + mi + fa + cr)

  lbl <- paste0(#"Frequency ",      # Dimension
    "Freq ",            # Abbreviation
    #"Population of ",  # Description
    "(N = ", N, "):  ", # "(N = x):  "
    "hi = ", hi, ", ",
    "mi = ", mi, ", ",
    "fa = ", fa, ", ",
    "cr = ", cr, " "
  )

  return(lbl)

}

## Check:
# make_freq_lbl(11, 22, 33, 44)

## (b) make_cond_lbl: Label current key parameters/probabilities by condition ------

make_cond_lbl <- function(prev, sens, spec) {

  lbl <- ""  # initialize

  lbl <- paste0(#"Conditions:  ",  # Dimension:
    #"p(cond):  ",                 # values are probabilities
    "Cond:  ",                     # Abbreviation:
    "prev = ", as_pc(prev, n_digits = 1), "%, ",
    "sens = ", as_pc(sens, n_digits = 1), "%, ",
    "spec = ", as_pc(spec, n_digits = 1), "%"
  )

  return(lbl)

}

## Check:
# make_cond_lbl(.001, 6/7, 2/3)

## (c) make_dec_lbl:  Label current key parameters/probabilities by decision ------

make_dec_lbl <- function(ppod, PPV, NPV) {

  lbl <- ""  # initialize

  lbl <- paste0(#"Decisions:  ",  # Dimension:
    #"p(dec):  ",                 # values are probabilities
    "Dec:  ",                     # Abbreviation:
    "ppod = ", as_pc(ppod, n_digits = 1), "%, ",
    "PPV = ", as_pc(PPV, n_digits = 1), "%, ",
    "NPV = ", as_pc(NPV, n_digits = 1), "%"
  )

  return(lbl)

}

## Check:
# make_dec_lbl(ppod = 1/3, PPV = 2/3, NPV = 1/7)


## (d) make_accu_lbl: Label current accuracy values ------

make_accu_lbl <- function(acc, w = NA, wacc = NA, mcc = NA) {

  lbl <- ""  # initialize


  if ( !is.na(w) && !is.na(wacc) && !is.na(mcc) ) {

    # (a) Complete accu label:

    wacc.lbl <- ""  # initialize

    # Sub-label for wacc.lbl:
    if (w == .50) {  # wacc is bacc:
      wacc.lbl <- paste0("bacc = ", as_pc(wacc, n_digits = 1), "%, ")
    } else {  # show wacc with w:
      wacc.lbl <- paste0("wacc = ", as_pc(wacc, n_digits = 1), "% ",
                         "(w = ", round(w, 2), "), ")
    }

    # Complete accu label (INcluding wacc and mcc):
    lbl <- paste0(#"Accuracy:  ",  # Dimension:
      # "p(dec.cor): ",            # acc value is a probability
      "Accu:  ",                   # Abbreviation:
      "acc = ", as_pc(acc, n_digits = 1), "%, ",
      wacc.lbl,
      "mcc = ", round(mcc, 2),
      " "  # add space at end
    )

  } else {

    # (b) Reduced accu label (EXcluding wacc and mcc):
    lbl <- paste0(# "Accuracy:  ",  # Dimension:
      # "p(dec.cor):  ",            # acc value is a probability
      "Accu:  ",                    # Abbreviation:
      "acc = ", as_pc(acc, n_digits = 1), "%",
      # wacc.lbl,
      # "mcc = ", round(mcc, 2),
      ""  # add NO space at end
    )

  } # if (!is.na(w) && ...)

  return(lbl)

}

## Check:
# make_accu_lbl(acc = 1/3)
# make_accu_lbl(acc = 1/3, w = 2/3, wacc = 3/7, mcc = 1/7)

## (e) label_note: Create a standard note when area is scaled: --------

label_note <- function(area = NULL, scale = "f") {

  note_lbl <- ""  # initialize

  if (!is.null(area)) {  # create an area label:

    area_lbl <- "Areas"
    scale_lbl <- ""

    if (area == "hr") {

      area_lbl <- "Horizontal widths"  # "Rectangles" / "Areas" ...

    } else if (area == "sq") {

      area_lbl <- "Areas"  # "Squares" in fnet/prism, but "Rectangles" (inside square) in mosaic plot!

    }

    if (scale == "p") {

      scale_lbl <- " by probabilities"

    } else if (scale == "f") {

      scale_lbl <- " by frequencies"

    }

    note_lbl <- paste0(area_lbl, " are scaled", scale_lbl, ".")

  }

  return(note_lbl)

}

## Check:
# label_note("hr", "f")
# label_note("sq", "p")
# label_note("xx", "x") # => "Areas are scaled."
# label_note(NULL)      # => ""

## plot_mar: Plot margin labels on an existing plot ------

plot_mar <- function(show_freq = TRUE,
                     show_cond = TRUE,
                     show_dec = TRUE,
                     show_accu = TRUE,
                     accu_from_freq = FALSE,  # Compute accuracy based on current (rounded or non-rounded) freq (default: accu_round_freq = FALSE).
                     note = "",
                     ...) {

  ## (0) Preparations: ------

  ## Record graphical parameters (par):
  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))

  ## Plot on existing plot:
  par(new = TRUE)  # TRUE ... adds to an existing plot; FALSE ... starts a new plot.

  ## Define margin areas:
  n_lines_mar <- 3
  n_lines_oma <- 0
  par(mar = c(n_lines_mar, 1, 2, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(n_lines_oma, 0, 0, 0) + 0.1)  # outer margins; default: par("oma") = 0 0 0 0.

  ## Plot empty canvas:
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

  ## Plotting commands:
  # box("plot", col = "firebrick")
  # box("figure", col = "forestgreen")
  # box("outer", col = "steelblue")


  ## (1) Margin text: ------

  ## Text parameters:
  m_col <- grey(.33, .99)  # color
  m_cex <- .85             # size

  ##   (A) on left side (adj = 0): ----

  ## A1. freq label:
  if (show_freq) {
    freq_lbl <- make_freq_lbl(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)  # use current freq values
    mtext(freq_lbl, side = 1, line = 0, adj = 0, col = m_col, cex = m_cex)  # print freq label
  }

  ## A2. Condition / p(cond) label:
  if (show_cond) {
    cond_lbl <- make_cond_lbl(prob$prev, prob$sens, prob$spec)  # use current prob values
    mtext(cond_lbl, side = 1, line = 1, adj = 0, col = m_col, cex = m_cex)  # print cond label
  }

  ## A3. Note:
  if ( !is.null(note) && !is.na(note) && (nchar(note) > 0) ) {
    note_lbl <- paste0("Note:  ", note, "")  # use current note
    mtext(paste0("", note_lbl, ""), side = 1, line = 2, adj = 0, col = m_col, cex = m_cex)  # print note
  }

  ##   (B) on right side (adj = 1): ----

  ## B1. Accuracy label:
  if (show_accu) {

    # if w.acc is undefined: use default (w.acc = .50):
    if ( !exists("w.acc") || is.null(w.acc) || is.na(w.acc) ) { w.acc <- .50 }

    if (accu_from_freq) {

      # (a) Compute accuracy from current (rounded or non-rounded) freq:
      cur.accu <- comp_accu_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr, w = w.acc)  # use current freq values
      accu_lbl <- make_accu_lbl(acc = cur.accu$acc, w = w.acc, wacc = cur.accu$wacc, mcc = cur.accu$mcc)  # use utility function
      accu_lbl <- paste0("*", accu_lbl, " (from freq)")  # Explicitly mark accu_from_freq case

    } else {

      # (b) Compute accuracy from (exact) prob == non-rounded freq:
      cur.accu <- comp_accu_prob(prev = prob$prev, sens = prob$sens, spec = prob$spec, w = w.acc)  # use current prob values + w.acc
      accu_lbl <- make_accu_lbl(acc = cur.accu$acc, w = w.acc, wacc = cur.accu$wacc, mcc = cur.accu$mcc)  # use utility function

    }

    mtext(accu_lbl, side = 1, line = 0, adj = 1, col = m_col, cex = m_cex)  # print accuracy label

  } # if (show_accu) ...

  ## B2. Decision / p(dec) label:
  if (show_dec) {
    dec_lbl <- make_dec_lbl(prob$ppod, prob$PPV, prob$NPV)
    mtext(dec_lbl, side = 1, line = 1, adj = 1, col = m_col, cex = m_cex)  # print decision label
  }

  ## B3. Imprint:
  imprint_lbl <- ""  # "[\uA9riskyr]"
  mtext(paste0(imprint_lbl, " "), side = 1, line = 2, adj = 1, col = m_col, cex = m_cex)


  ## (3) Outer margin text: ----

  if (n_lines_oma > 0) {

    m_col <- grey(.14, .99)
    m_cex <- .75

    ## (7) Note:
    if (nchar(note) > 0) {
      note_lbl <- paste0("Note:  ", note, "")
      mtext(paste0("  ", note_lbl, ""), side = 1, line = 0, adj = 0, col = m_col, cex = m_cex, outer = TRUE)
    }

    ## (8) Imprint:
    imprint_lbl <- "" # [\uA9riskyr]"
    mtext(paste0(imprint_lbl, " "), side = 1, line = 0, adj = 1, col = m_col, cex = m_cex, outer = TRUE)

  }

  ## (4) Finish: ----

  invisible()  # restores par(opar)
}

## Check:
# plot_mar(note = "Some comment here.")  # plots on existing plot, OR starts new plot (+ warning)
# plot_mar(accu_from_freq = TRUE, note = "Accuracy from current (rounded or non-rounded) frequencies.")












## (5) Miscellaneous plotting functions: ----------

## factors_min_diff: Dynamic calculation of block size (in plot_iconarray.R) ------

factors_min_diff <- function (n) {
  n_sqrt <- sqrt(n)
  lower <- floor(n_sqrt)
  upper <- ceiling(n_sqrt)

  while (lower * upper != n) {
    if (lower * upper > n) { lower <- lower - 1 }
    if (lower * upper < n) { upper <- upper + 1 }
  }

  return(c(lower, upper))
}



## box_text: Add text with background box to a plot ------
## from https://stackoverflow.com/questions/45366243/text-labels-with-background-colour-in-r

## Add text with background box to a plot

# \code{box_text} places a text given in the vector \code{labels}
# onto a plot in the base graphics system and places a coloured box behind
# it to make it stand out from the background.

# @param x numeric vector of x-coordinates where the text labels should be
# written. If the length of \code{x} and \code{y} differs, the shorter one
# is recycled.
# @param y numeric vector of y-coordinates where the text labels should be
# written.
# @param labels a character vector specifying the text to be written.
# @param col.text the colour of the text
# @param col.bg color(s) to fill or shade the rectangle(s) with. The default
# \code{NA} means do not fill, i.e., draw transparent rectangles.
# @param border.bg color(s) for rectangle border(s). The default \code{NA}
# omits borders.
# @param adj one or two values in [0, 1] which specify the x (and optionally
# y) adjustment of the labels.
# @param pos a position specifier for the text. If specified this overrides
# any adj value given. Values of 1, 2, 3 and 4, respectively indicate
# positions below, to the left of, above and to the right of the specified
# coordinates.
# @param offset when \code{pos} is specified, this value gives the offset of
# the label from the specified coordinate in fractions of a character width.
# @param padding factor used for the padding of the box around
# the text. Padding is specified in fractions of a character width. If a
# vector of length two is specified then different factors are used for the
# padding in x- and y-direction.
# @param cex numeric character expansion factor; multiplied by
# code{par("cex")} yields the final character size.
# @param font the font to be used
#
# @return Returns the coordinates of the background rectangle(s). If
# multiple labels are placed in a vactor then the coordinates are returned
# as a matrix with columns corresponding to xleft, xright, ybottom, ytop.
# If just one label is placed, the coordinates are returned as a vector.
#
# @author Ian Kopacka
#
# @examples
# ## Create noisy background
# plot(x = runif(1000), y = runif(1000), type = "p", pch = 16,
# col = "#40404060")
# box_text(x = 0.5, y = 0.5, labels = "some Text", col.bg = "#b2f4f480",
#    pos = 4, font = 2, cex = 1.3, padding = 1)

box_text <- function(x, y, labels = NA, col.text = NULL, col.bg = NA,
                     border.bg = NA, adj = NULL, pos = NULL, offset = 0.5,
                     padding = c(0.5, 0.5), cex = 1, font = graphics::par('font')){

  ## The Character expansion factro to be used:
  theCex <- graphics::par('cex')*cex

  ## Is y provided:
  if (missing(y)) y <- x

  ## Recycle coords if necessary:
  if (length(x) != length(y)){
    lx <- length(x)
    ly <- length(y)
    if (lx > ly){
      y <- rep(y, ceiling(lx/ly))[1:lx]
    } else {
      x <- rep(x, ceiling(ly/lx))[1:ly]
    }
  }

  ## Width and height of text
  textHeight <- graphics::strheight(labels, cex = theCex, font = font)
  textWidth <- graphics::strwidth(labels, cex = theCex, font = font)

  ## Width of one character:
  charWidth <- graphics::strwidth("e", cex = theCex, font = font)

  ## Is 'adj' of length 1 or 2?
  if (!is.null(adj)){
    if (length(adj == 1)){
      adj <- c(adj[1], 0.5)
    }
  } else {
    adj <- c(0.5, 0.5)
  }

  ## Is 'pos' specified?
  if (!is.null(pos)){
    if (pos == 1){
      adj <- c(0.5, 1)
      offsetVec <- c(0, -offset*charWidth)
    } else if (pos == 2){
      adj <- c(1, 0.5)
      offsetVec <- c(-offset*charWidth, 0)
    } else if (pos == 3){
      adj <- c(0.5, 0)
      offsetVec <- c(0, offset*charWidth)
    } else if (pos == 4){
      adj <- c(0, 0.5)
      offsetVec <- c(offset*charWidth, 0)
    } else {
      stop('Invalid argument pos')
    }
  } else {
    offsetVec <- c(0, 0)
  }

  ## Padding for boxes:
  if (length(padding) == 1){
    padding <- c(padding[1], padding[1])
  }

  ## Midpoints for text:
  xMid <- x + (-adj[1] + 1/2)*textWidth + offsetVec[1]
  yMid <- y + (-adj[2] + 1/2)*textHeight + offsetVec[2]

  ## Draw rectangles:
  rectWidth <- textWidth + 2*padding[1]*charWidth
  rectHeight <- textHeight + 2*padding[2]*charWidth
  graphics::rect(xleft = xMid - rectWidth/2,
                 ybottom = yMid - rectHeight/2,
                 xright = xMid + rectWidth/2,
                 ytop = yMid + rectHeight/2,
                 col = col.bg, border = border.bg)

  ## Place the text:
  graphics::text(xMid, yMid, labels, col = col.text, cex = theCex, font = font,
                 adj = c(0.5, 0.5))

  ## Return value:
  if (length(xMid) == 1){
    invisible(c(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                yMid + rectHeight/2))
  } else {
    invisible(cbind(xMid - rectWidth/2, xMid + rectWidth/2, yMid - rectHeight/2,
                    yMid + rectHeight/2))
  }
}

## Check:

# ## Create a noisy background:
# plot(x = runif(1000), y = runif(1000), type = "p", pch = 16, col = "#40404060")
#
# ## Vector of labels, using argument 'pos' to position right of coordinates:
# box_text(x = c(0.1, 0.8), y = c(0.1, 0.7), labels = c("some Text", "something else"),
#         col.bg = "gold", pos = 4, padding = 0.2)
#
# ## Tweak cex, font and adj:
# box_text(x = 0.2, y = 0.4, labels = "some big and bold text",
#         col.bg = "skyblue", adj = c(0, 0.6), font = 2, cex = 1.8)



## add_legend: Reformat the plot area to place legend outside of plot ------

add_legend <- function(...) {
  ## Reformat the plotting area to allow placing legend outside of a plot
  ## Source: https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics

  opar <- par(fig = c(0, 1, 0, 1),
              mar = c(0, 0, 0, 0),
              oma = c(0, 0, 0, 0),
              new = TRUE)

  on.exit(par(opar))

  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')

  legend(...)
}

## Check:
# add_legend()  # requires a legend argument.





## (*) Done: ----------

## - Moved many helper functions for plotting from "comp_util.R" to "plot_util.R".  [2018 08 27]
## - Defined box class and print.box and plot.box methods [2018 08 19].
## - Started this collection [2018 08 16].

## (+) ToDo: ----------

## - plot_boxes fn. that takes many boxes as input,
##   determines their current freq/prob values,
##   and plots them in some order (e.g., from largest to smallest, to prevent occlusion of labels).

## - consider separating plotting freq boxes (via plot_fbox or plot_cbox)
##   from plotting labels (via plot_freq_label) to allow plotting labels
##   after (on top of) boxes.

## eof. ----------

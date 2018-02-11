## plot_fnet.R | riskyr
## 2018 02 11
## -----------------------------------------------
## Plot a network diagram of frequencies
## (as nodes) and probabilities (as edges)
## -----------------------------------------------
## Version 4: Generalization of plot_tree

## Options available in this version:
## - by    ... "cd", "dc", "cddc" (default), "dccd".
## - area  ... "no", "sq" (default), "hr", "vr".
## - p.lbl ... "nam", "num" (default), "mix", "min".
## - show.accu ... show current accuracy metrics (with bacc/wacc).

## -----------------------------------------------
## Dependencies:

# library("diagram") # moved to "Imports:" in in DESCRIPTION!

## -----------------------------------------------
## plot_fnet is a generalization of plot_tree:
## Plot a network or tree diagram of frequencies
## (as nodes) and probabilities (as edges)
## (using only necessary arguments with good defaults):

## Assuming that freq$N (+ num txt pal) is known!

#' Plot a network diagram of frequencies and probabilities.
#'
#' \code{plot_fnet} draws a network diagram of
#' frequencies (as nodes) and probabilities (as edges)
#' from a sufficient and valid set of 3 essential probabilities
#' (\code{\link{prev}}, and
#' \code{\link{sens}} or its complement \code{\link{mirt}}, and
#' \code{\link{spec}} or its complement \code{\link{fart}})
#' or existing frequency information \code{\link{freq}}
#' and a population size of \code{\link{N}} individuals.
#'
#' \code{plot_fnet} is a generalization of \code{\link{plot_tree}}
#' and offers the additional option of plotting the interplay
#' between the 9 frequencies of \code{\link{freq}} and
#' and the 10 probabilities of \code{\link{prob}}
#' in a single network diagram.
#'
#' The option \code{by} (as 2 or 4 characters) allows specifying
#' 4 different ways of arranging frequencies:
#' \enumerate{
#'  \item \code{"cd"} plots a tree diagram in which the population is split by condition;
#'  \item \code{"dc"} plots a tree diagram in which the population is split by decision;
#'  \item \code{"cddc"} plots a network diagram in which the population is split 1st by condition, 2nd by decision (default);
#'  \item \code{"dccd"} is yet to be implemented.
#'}
#'
#' The option \code{area} (as 2 characters) allows specifying
#' 4 different box shapes and sizes:
#' \enumerate{
#'  \item \code{"no"} shows all boxes in the same size (default);
#'  \item \code{"sq"} shows boxes as squares with area sizes proportional to frequencies;
#'  \item \code{"hr"} shows boxes as horizontal rectangles of area sizes proportional to frequencies;
#'  \item \code{"vr"} shows boxes as vertical rectangles of area sizes proportional to frequencies.
#' The resulting shapes and their relative proportions correspond to the areas in \code{\link{plot_mosaic}}.
#'}
#'
#' If a prevalence value \code{\link{prev}} is provided, a new list of
#' natural frequencies \code{\link{freq}} is computed by \code{\link{comp_freq}}.
#' By contrast, if no prevalence value \code{\link{prev}} is provided,
#' the values currently contained in \code{\link{freq}} are used.
#' By default, \code{\link{comp_freq}} rounds frequencies to nearest integers
#' to avoid decimal values in \code{\link{freq}}.
#'
#' \code{plot_fnet} requires and uses the R package "diagram"
#' (\code{library("diagram")}).
#'
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
#'
#' @param mirt The decision's miss rate \code{\link{mirt}}
#' (i.e., the conditional probability of a negative decision
#' provided that the condition is \code{TRUE}).
#' \code{mirt} is optional when its complement \code{sens} is provided.
#'
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when its complement \code{fart} is provided.
#'
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#'
#' @param N The number of individuals in the population.
#' A suitable value of \code{\link{N}} is computed, if not provided.
#'
#'
#' @param round A Boolean option specifying whether computed frequencies
#' are rounded to integers. Default: \code{round = TRUE}.
#'
#'
#' @param by A character code specifying the perspective (or 1st category by which the population is split into subsets) with 4 options:
#'   \enumerate{
#'   \item \code{"cd"} ... by condition;
#'   \item \code{"dc"} ... by decision;
#'   \item \code{"cddc"} ... 1st by condition, 2nd by decision;
#'   \item \code{"dccd"} ... 1st by decision, 2nd by condition.
#'   }
#'
#' @param area A character code specifying the area of the boxes (or their relative sizes) with 4 options:
#'   \enumerate{
#'   \item \code{"no"} ... all boxes are shown with the same size;
#'   \item \code{"sq"} ... boxes are squares with area sizes scaled proportional to frequencies (default);
#'   \item \code{"hr"} ... boxes are horizontal rectangles with area sizes scaled proportional to frequencies;
#'   \item \code{"vr"} ... boxes are vertical rectangles with area sizes scaled proportional to frequencies.
#'   }
#'
#' @param p.lbl A character code specifying the type of probability information (on edges) with 4 options:
#'   \enumerate{
#'   \item \code{"nam"} ... names of probabilities;
#'   \item \code{"num"} ... numeric values of probabilities (rounded to 3 decimals) (default);
#'   \item \code{"mix"} ... names of essential probabilities, values of complements;
#'   \item \code{"min"} ... minimal labels: names of essential probabilities.
#'   }
#'
#'
#' @param show.accu Option for showing current
#' accuracy metrics \code{\link{accu}} in the plot.
#' Default: \code{show.accu = TRUE}.
#'
#' @param w.acc Weigthing parameter \code{w} used to compute
#' weighted accuracy \code{w.acc} in \code{\link{comp_accu}}.
#' Default: \code{w.acc = .50}.
#'
#' Various other options allow the customization of text labels and colors:
#'
#' @param title.lbl Text label for current plot title.
#' Default: \code{title.lbl = txt$scen.lbl}.
#'
#' @param popu.lbl Text label for current population \code{\link{popu}}.
#'
#' @param cond.true.lbl Text label for current cases of \code{\link{cond.true}}.
#' @param cond.false.lbl Text label for current cases of \code{\link{cond.false}}.
#'
#' @param dec.pos.lbl Text label for current cases of \code{\link{dec.pos}}.
#' @param dec.neg.lbl Text label for current cases of \code{\link{dec.neg}}.
#'
#' @param hi.lbl Text label for hits \code{\link{hi}}.
#' @param mi.lbl Text label for misses \code{\link{mi}}.
#' @param fa.lbl Text label for false alarms \code{\link{fa}}.
#' @param cr.lbl Text label for correct rejections \code{\link{cr}}.
#'
#' @param col.txt Color for text labels (in boxes).
#' @param box.cex Scaling factor for text (in boxes).
#' Default: \code{box.cex = .90}.
#'
#' @param col.boxes Colors of boxes (a single color or a vector with named colors matching the number of current boxes).
#' Default: Current color information contained in \code{\link{pal}}.
#' @param col.border Color of borders.
#' Default: \code{col.border = grey(.33, alpha = .99)}.
#'
#' @param lwd Width of arrows.
#' @param box.lwd Width of boxes.
#'
#' @param col.shadow Color of box shadows.
#' Default: \code{col.shadow = grey(.11, alpha = .99)}.
#' @param cex.shadow Scaling factor of shadows (values > 0 showing shadows).
#' Default: \code{cex.shadow = 0}.
#'
#'
#' @return Nothing (NULL).
#'
#'
#' @examples
#' # Plotting existing freq:
#' plot_fnet()  # => plot current freq with default options
#' plot_fnet(by = "dccd")
#' plot_fnet(area = "no")
#' plot_fnet(p.lbl = "num")
#' plot_fnet(title.lbl = "")
#' plot_fnet(N = 33)
#' plot_fnet(N = NA)
#'
#' # Computing and plotting new frequencies from probabilities:
#' plot_fnet(prev = 1/3)  # => changes prev, but uses current defaults of sens and spec
#' plot_fnet(prev = 1/3, N = 55)
#' plot_fnet(prev = 1/3, N = NA)
#' plot_fnet(prev = 1/3, round = FALSE)
#' plot_fnet(prev = .10, sens = .90, spec = 1/3, N = 100)
#' plot_fnet(prev = .10, sens = .90, spec = NA, fart = 1/3, N = 33)
#' plot_fnet(prev = .10, sens = .90, spec = 1/3, fart = NA, N = NA)
#' plot_fnet(prev = .10, sens = .90, spec = NA, fart = 1/3, N = NA)
#'
#' # Perspective options:
#' plot_fnet(by = "cd")    # => 1. Tree diagram (by condition)
#' plot_fnet(by = "dc")    # => 2. Tree diagram (by decision)
#' plot_fnet(by = "cddc")  # => 3. Network diagram (1st by cond, 2nd by dec) (default)
#' plot_fnet(by = "dccd")  # => 4. Network diagram (1st by dec, 2nd by cond)
#'
#' # Area options:
#' plot_fnet(area = "sq")  # => (default)
#' plot_fnet(area = "no")
#' plot_fnet(area = "sq", round = FALSE)
#' plot_fnet(area = "hr")
#' plot_fnet(area = "vr", round = FALSE)
#'
#' # Accuracy:
#' plot_fnet(show.accu = TRUE)               # => default w = .5 (balanced accuracy "bacc")
#' plot_fnet(show.accu = TRUE, w.acc = 1/3)  # => (weighted accuracy "wacc")
#' plot_fnet(show.accu = FALSE)              # => no accuracy info.
#'
#' # Rounding:
#' plot_fnet(prev = .1, sens = .7, spec = .9, N = 10, by = "cddc", area = "sq",
#'           p.lbl = "num", round = TRUE)    # => mi = 0
#' plot_fnet(prev = .1, sens = .7, spec = .9, N = 10, by = "cddc", area = "sq",
#'           p.lbl = "num", round = FALSE)   # => mi = 0.3
#'
#' # Combining perspectives, areas, and label options:
#' plot_fnet(by = "cd", area = "sq", p.lbl = "nam")  # => by cond + sq + prob names
#' plot_fnet(by = "cd", area = "hr", p.lbl = "num")  # => by cond + hr + prob numbers
#' plot_fnet(by = "dc", area = "sq", p.lbl = "num")  # => by dec  + sq + mix names and numbers
#' plot_fnet(by = "dc", area = "vr", p.lbl = "mix")  # => by dec  + vr + min. labels
#'
#' # Custom colors and shadows:
#' plot_fnet(prev = .08, sens = .92, spec = .95, N = 10000, area = "hr")
#' plot_fnet(area = "sq", col.boxes = "gold", col.border = "steelblue4",
#'           col.shadow = "steelblue4", cex.shadow = .008)
#' plot_fnet(N = NA, area = "vr", col.txt = "steelblue4", col.boxes = "lightyellow",
#'           col.border = grey(.3, .7), cex.shadow = .008, col.shadow = grey(.1, .9))
#'
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{pal}} contains current color settings;
#' \code{\link{txt}} contains current text settings;
#' \code{\link{comp_min_N}} computes a suitable minimum population size \code{\link{N}}.
#'
#' @export

plot_fnet <- function(prev = num$prev,             # probabilities
                      sens = num$sens, mirt = NA,
                      spec = num$spec, fart = NA,  # was: num$fart,
                      N = freq$N,    # ONLY freq used (so far)
                      ## Options:
                      round = TRUE,  # Boolean: round freq (if computed), default: round = TRUE.
                      by = "cddc",   # 4 perspectives: "cd" by condition, "dc" by decision; "cddc" by condition and decision (default), "dccd" by decision and condition.
                      area = "sq",   # 4 area types: "no" none, "sq" square (default), "hr" horizontal rectangles, "vr" vertical rectangles
                      p.lbl = "num", # 4 probability (edge) label types: "nam" names, "num" numeric values (default), "mix" essential names + complement values, "min" minimal.
                      ## Compute and show accuracy info:
                      show.accu = TRUE,  # compute and show accuracy metrics
                      w.acc = .50,       # weight w for wacc (from 0 to 1)
                      ## Labels:
                      title.lbl = txt$scen.lbl,
                      popu.lbl = txt$popu.lbl,
                      ## Condition labels:
                      cond.true.lbl = txt$cond.true.lbl,
                      cond.false.lbl = txt$cond.false.lbl,
                      ## Decision labels:
                      dec.pos.lbl = txt$dec.pos.lbl,
                      dec.neg.lbl = txt$dec.neg.lbl,
                      ## SDT combinations:
                      hi.lbl = txt$hi.lbl,
                      mi.lbl = txt$mi.lbl,
                      fa.lbl = txt$fa.lbl,
                      cr.lbl = txt$cr.lbl,
                      ## Box settings:
                      col.txt = grey(.01, alpha = .99),  # black
                      box.cex = .85,                     # relative text size
                      col.boxes = pal, # pal[c(1:9)],    # box colors (9 frequencies/boxes/colors)
                      col.border = grey(.33, alpha = .99),  # grey
                      ## Widths of arrows and box borders:
                      lwd = 1.5,      # width of arrows
                      box.lwd = 1.5,  # set to 0.001 to show boxes without borders (but =0 yields ERROR)
                      ## Shadows:
                      col.shadow = col.sand.dark,
                      cex.shadow = 0  # [values > 0 show shadows]
){

  ## (0) Compute or collect all current frequencies: ----------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur.freq based on current parameters (N and probabilities):
    cur.freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = round)  # compute freq

    ## Assign elements of cur.freq:
    N <- cur.freq$N
    n.true  <- cur.freq$cond.true
    n.false <- cur.freq$cond.false
    n.pos <- cur.freq$dec.pos
    n.neg <- cur.freq$dec.neg
    n.hi  <- cur.freq$hi
    n.mi  <- cur.freq$mi
    n.fa  <- cur.freq$fa
    n.cr  <- cur.freq$cr

  } else { # (B) NO valid set of probabilities was provided:

    ## Use the current values of freq:
    N <- freq$N
    n.true  <- freq$cond.true
    n.false <- freq$cond.false
    n.pos <- freq$dec.pos
    n.neg <- freq$dec.neg
    n.hi  <- freq$hi
    n.mi  <- freq$mi
    n.fa  <- freq$fa
    n.cr  <- freq$cr

  } # if (is_valid_prob_set...)

  if (by != "cd") {  # in ANY case NOT solely by condition:

    # Compute current PVs from current frequencies:
    ppod <- n.pos/N
    PPV  <- n.hi/n.pos
    NPV  <- n.cr/n.neg

  } # if (by...)


  ## (1) Color of boxes: ----------

  if ((length(col.boxes) == length(pal)) &&
      all.equal(col.boxes, pal)) {  # no change from default:

    ## Use current color information of pal:

    if (by == "cd") {  # (a) by condition:

      ## 7 boxes (including cond.true and cond.false):
      # col.boxes <- col.boxes[c(1:3, 6:9)]  # select 7 of 9 colors
      col.boxes <- c(pal["N"], pal["true"], pal["false"],
                     pal["hi"], pal["mi"], pal["fa"], pal["cr"])

    } else if (by == "dc") {  # (b) by decision:

      ## 7 boxes (including dec.pos and dec.neg):
      # col.boxes <- col.boxes[c(1, 4:9)  ]  # select 7 of 9 colors
      col.boxes <- c(pal["N"], pal["pos"], pal["neg"],
                     pal["hi"], pal["mi"], pal["fa"], pal["cr"])

    } else if (by == "cddc") {  # (c) by condition + decision:

      ## 10 boxes (top: cond.true and cond.false; bot: dec.pos and dec.neg):
      # col.boxes <- col.boxes[c(1:3, 6:9, 4:5, 1)  ]  # select 9 of 9 colors
      col.boxes <- c(pal["N"],
                     pal["true"], pal["false"],
                     pal["hi"], pal["mi"], pal["fa"], pal["cr"],
                     pal["pos"], pal["neg"],
                     pal["N"])

    } else if (by == "dccd") {  # (d) 1st by decision, then by condition:

      ## 10 boxes (top: dec.pos and dec.neg; bot: cond.true and cond.false):
      # col.boxes <- col.boxes[c(1, 4:9, 2:3, 1)  ]  # select 9 of 9 colors
      col.boxes <- c(pal["N"],
                     pal["pos"], pal["neg"],
                     pal["hi"], pal["mi"], pal["fa"], pal["cr"],
                     pal["true"], pal["false"],
                     pal["N"])

    } else {  # ANY other by-setting:

      col.boxes <- pal["N"]  # to prevent errors for other entries

    } # if (by...)

  } # if (all.equal(col.boxes, pal))...


  ## (2) Text/labels in 7 or 10 boxes: ----------

  if (by == "cd") {  # (a) by condition:

    if (area == "no") {  # default box labels:

      ## Full box labels (label: value):
      names <- c(paste0("Population", ":\n", "N = ", N),  # popu.lbl (NOT used yet)
                 paste0(cond.true.lbl, ":\n",  round(n.true, 2)),
                 paste0(cond.false.lbl, ":\n", round(n.false, 2)),
                 paste0(hi.lbl, ":\n", round(n.hi, 2)),
                 paste0(mi.lbl, ":\n", round(n.mi, 2)),
                 paste0(fa.lbl, ":\n", round(n.fa, 2)),
                 paste0(cr.lbl, ":\n", round(n.cr, 2))
      )

    } else {  # shorter box labels:

      ## Reduced labels (as areas get quite small):
      names <- c(paste0("N = ", N),  # popu.lbl
                 paste0("true:\n",  round(n.true, 2)),
                 paste0("false:\n", round(n.false, 2)),
                 paste0("hi:\n", round(n.hi, 2)),
                 paste0("mi:\n", round(n.mi, 2)),
                 paste0("fa:\n", round(n.fa, 2)),
                 paste0("cr:\n", round(n.cr, 2))
      )

    }  # if (area...)

  } else if (by == "dc") {  # (b) by decision:

    if (area == "no") {  # default box labels:

      ## Full box labels (label: value):
      names <- c(paste0("Population", ":\n", "N = ", N),  # popu.lbl (NOT used yet)
                 paste0(dec.pos.lbl, ":\n",  round(n.pos, 2)),
                 paste0(dec.neg.lbl, ":\n", round(n.neg, 2)),
                 paste0(hi.lbl, ":\n", round(n.hi, 2)),
                 paste0(mi.lbl, ":\n", round(n.mi, 2)),
                 paste0(fa.lbl, ":\n", round(n.fa, 2)),
                 paste0(cr.lbl, ":\n", round(n.cr, 2))
      )

    } else {  # shorter box labels:

      ## Reduced labels (as areas get quite small):
      names <- c(paste0("N = ", N),  # popu.lbl
                 paste0("positive:\n",  round(n.pos, 2)),
                 paste0("negative:\n", round(n.neg, 2)),
                 paste0("hi:\n", round(n.hi, 2)),
                 paste0("mi:\n", round(n.mi, 2)),
                 paste0("fa:\n", round(n.fa, 2)),
                 paste0("cr:\n", round(n.cr, 2))
      )

    }  # if (area...)

  } else if (by == "cddc") {  # (c) by condition + decision:

    if (area == "no") {  # default box labels:

      ## Full box labels (label: value):
      names <- c(paste0("Population", ":\n", "N = ", N),  # popu.lbl (NOT used yet)
                 paste0(cond.true.lbl, ":\n",  round(n.true, 2)),
                 paste0(cond.false.lbl, ":\n", round(n.false, 2)),
                 paste0(hi.lbl, ":\n", round(n.hi, 2)),
                 paste0(mi.lbl, ":\n", round(n.mi, 2)),
                 paste0(fa.lbl, ":\n", round(n.fa, 2)),
                 paste0(cr.lbl, ":\n", round(n.cr, 2)),
                 paste0(dec.pos.lbl, ":\n", round(n.pos, 2)),
                 paste0(dec.neg.lbl, ":\n", round(n.neg, 2)),
                 paste0("Population", ":\n", "N = ", N)  # popu.lbl
      )

    } else {  # shorter box labels:

      ## Reduced labels (as areas get quite small):
      names <- c(paste0("N = ", N),  # popu.lbl
                 paste0("true:\n",  round(n.true, 2)),
                 paste0("false:\n", round(n.false, 2)),
                 paste0("hi:\n", round(n.hi, 2)),
                 paste0("mi:\n", round(n.mi, 2)),
                 paste0("fa:\n", round(n.fa, 2)),
                 paste0("cr:\n", round(n.cr, 2)),
                 paste0("positive:\n",  round(n.pos, 2)),
                 paste0("negative:\n", round(n.neg, 2)),
                 paste0("N = ", N)   # popu.lbl
      )

    }  # if (area...)

    # } else if (by == "dccd") {  # (d) 1st by decision, then by condition:

  } else {  # ANY other by-case:

    if (area == "no") {  # default box labels:

      ## Full box labels (label: value):
      names <- c(paste0("Population", ":\n", "N = ", N),  # popu.lbl (NOT used yet)
                 paste0(dec.pos.lbl, ":\n", round(n.pos, 2)),
                 paste0(dec.neg.lbl, ":\n", round(n.neg, 2)),
                 paste0(hi.lbl, ":\n", round(n.hi, 2)),
                 paste0(mi.lbl, ":\n", round(n.mi, 2)),
                 paste0(fa.lbl, ":\n", round(n.fa, 2)),
                 paste0(cr.lbl, ":\n", round(n.cr, 2)),
                 paste0(cond.true.lbl, ":\n",  round(n.true, 2)),
                 paste0(cond.false.lbl, ":\n", round(n.false, 2)),
                 paste0("Population", ":\n", "N = ", N)  # popu.lbl
      )

    } else {  # shorter box labels:

      ## Reduced labels (as areas get quite small):
      names <- c(paste0("N = ", N),  # popu.lbl
                 paste0("positive:\n",  round(n.pos, 2)),
                 paste0("negative:\n", round(n.neg, 2)),
                 paste0("hi:\n", round(n.hi, 2)),
                 paste0("mi:\n", round(n.mi, 2)),
                 paste0("fa:\n", round(n.fa, 2)),
                 paste0("cr:\n", round(n.cr, 2)),
                 paste0("true:\n",  round(n.true, 2)),
                 paste0("false:\n", round(n.false, 2)),
                 paste0("N = ", N)   # popu.lbl
      )

    }  # if (area...)

  } # if (by...)


  ## (3) Make matrix M: ----------

  if (by == "cd" || by == "dc") {  # (a) by condition OR (b) by decision:

    M <- matrix(nrow = 7, ncol = 8, byrow = TRUE, data = 0)

    # } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

  } else { # ANY other setting:

    M <- matrix(nrow = 10, ncol = 11, byrow = TRUE, data = 0)

  }  # if (by...)


  ## (4) Arrow/edge labels: ----------

  ## ToDo: Use more informative arrow/edge labels:
  # prev.lbl <- paste0("prev = ", as_pc(prev), "%")
  # prev.lbl <- paste0("prev = ", prev) # ERROR: WHY does prev.lbl not work with spaces???

  if (by == "cd") {  # by condition:

    # 4 types of edge labels: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
    if (p.lbl == "nam") {  # default labels: names of essential probabilities + their complements:

      M[2, 1] <- "prev"         # "prevalence"
      M[3, 1] <- "(1 - prev)"   # "(N - n.true)"
      M[4, 2] <- "sens"         # "sensitivity"
      M[5, 2] <- "mirt"         # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 3] <- "fart"         # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 3] <- "spec"         # "specificity"

    } else if (p.lbl == "num") {  # numeric values (rounded to 3 decimals):

      M[2, 1] <- round(prev, 3)
      M[3, 1] <- round((1 - prev), 3)  # (1 - prev) "n(N - true)"
      M[4, 2] <- round(sens, 3)
      M[5, 2] <- round((1 - sens), 3)  # mirt = (1 - sens) = "n(true - hi)"
      M[6, 3] <- round((1 - spec), 3)  # fart = (1 - spec) = "n(false - cr)"
      M[7, 3] <- round(spec, 3)

    } else if (p.lbl == "mix") {  # mixed labels: essential names + numeric complements:

      M[2, 1] <- "prev"                # "prevalence"
      M[3, 1] <- round((1 - prev), 3)  # (1 - prev) "n(N - true)"
      M[4, 2] <- "sens"                # "sensitivity"
      M[5, 2] <- round((1 - sens), 3)  # mirt = (1 - sens) = "n(true - hi)"
      M[6, 3] <- round((1 - spec), 3)  # fart = (1 - spec) = "n(false - cr)"
      M[7, 3] <- "spec"                # "specificity"

    } else {  # "min" minimal labels:

      M[2, 1] <- "prev"         # "prevalence"
      M[3, 1] <- ""             # "(N - n.true)"
      M[4, 2] <- "sens"         # "sensitivity"
      M[5, 2] <- ""             # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 3] <- ""             # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 3] <- "spec"         # "specificity"

    } # (p.lbl ==...

  } else if (by == "dc") {  # by decision:

    # 4 types of edge labels: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.

    if (p.lbl == "nam") {  # default labels: names of essential probabilities + their complements:

      M[2, 1] <- "ppod"         # ppod  ERROR: WHY does prev.lbl not work with spaces???
      M[3, 1] <-  "(1 - ppod)"  # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"          # PPV
      M[6, 2] <- "FDR"          # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- "FOR"          # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"          # NPV

    } else if (p.lbl == "num") {  # numeric values (rounded to 3 decimals):

      M[2, 1] <- round(ppod, 3)        # ppod
      M[3, 1] <- round((1 - ppod), 3)  # "n(N - dec.pos)"
      M[4, 2] <- round(PPV, 3)         # PPV
      M[6, 2] <- round((1 - PPV), 3)   # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- round((1 - NPV), 3)   # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- round(NPV, 3)         # NPV

    } else if (p.lbl == "mix") {  # mixed labels: essential names + numeric complements:

      M[2, 1] <- "ppod"                # ppod  ERROR: WHY does prev.lbl not work with spaces???
      M[3, 1] <- round((1 - ppod), 3)  # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"                 # PPV
      M[6, 2] <- round((1 - PPV), 3)   # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- round((1 - NPV), 3)   # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"                 # NPV

    } else {  # "min" minimal labels:

      M[2, 1] <- "ppod"         # ppod   ERROR: WHY does prev.lbl not work with spaces???
      M[3, 1] <-  ""            # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"          # PPV
      M[6, 2] <- ""             # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- ""             # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"          # NPV

    } # (p.lbl ==...

  } else if (by == "cddc") {  # (c) by condition + decision:

    # 4 types of edge labels: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
    if (p.lbl == "nam") {  # default labels: names of essential probabilities + their complements:

      M[2, 1] <- "prev"         # "prevalence"
      M[3, 1] <- "(1 - prev)"   # "(N - n.true)"
      M[4, 2] <- "sens"         # "sensitivity"
      M[5, 2] <- "mirt"         # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 3] <- "fart"         # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 3] <- "spec"         # "specificity"

      M[4, 8]  <- "PPV"         # PPV
      M[6, 8]  <- "FDR"         # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 9]  <- "FOR"         # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 9]  <- "NPV"         # NPV
      M[8, 10] <- "ppod"        # ppod  ERROR: WHY does prev.lbl not work with spaces???
      M[9, 10] <- "(1 - ppod)"  # (1 - ppod) = "n(N - dec.pos)"

    } else if (p.lbl == "num") {  # numeric values (rounded to 3 decimals):

      M[2, 1] <- round(prev, 3)
      M[3, 1] <- round((1 - prev), 3)  # (1 - prev) "n(N - true)"
      M[4, 2] <- round(sens, 3)
      M[5, 2] <- round((1 - sens), 3)  # mirt = (1 - sens) = "n(true - hi)"
      M[6, 3] <- round((1 - spec), 3)  # fart = (1 - spec) = "n(false - cr)"
      M[7, 3] <- round(spec, 3)

      M[4, 8]  <- round(PPV, 3)         # PPV
      M[6, 8]  <- round((1 - PPV), 3)   # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 9]  <- round((1 - NPV), 3)   # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 9]  <- round(NPV, 3)         # NPV
      M[8, 10] <- round(ppod, 3)        # ppod
      M[9, 10] <- round((1 - ppod), 3)  # "n(N - dec.pos)"

    } else if (p.lbl == "mix") {  # mixed labels: essential names + numeric complements:

      M[2, 1] <- "prev"                # "prevalence"
      M[3, 1] <- round((1 - prev), 3)  # (1 - prev) "n(N - true)"
      M[4, 2] <- "sens"                # "sensitivity"
      M[5, 2] <- round((1 - sens), 3)  # mirt = (1 - sens) = "n(true - hi)"
      M[6, 3] <- round((1 - spec), 3)  # fart = (1 - spec) = "n(false - cr)"
      M[7, 3] <- "spec"                # "specificity"

      M[4, 8]  <- "PPV"                 # PPV
      M[6, 8]  <- round((1 - PPV), 3)   # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 9]  <- round((1 - NPV), 3)   # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 9]  <- "NPV"                 # NPV
      M[8, 10] <- "ppod"                # ppod  ERROR: WHY does prev.lbl not work with spaces???
      M[9, 10] <- round((1 - ppod), 3)  # (1 - ppod) = "n(N - dec.pos)"

    } else {  # "min" minimal labels:

      M[2, 1] <- "prev"         # "prevalence"
      M[3, 1] <- ""             # "(N - n.true)"
      M[4, 2] <- "sens"         # "sensitivity"
      M[5, 2] <- ""             # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 3] <- ""             # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 3] <- "spec"         # "specificity"

      M[4, 8]  <- "PPV"         # PPV
      M[6, 8]  <- ""            # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 9]  <- ""            # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 9]  <- "NPV"         # NPV
      M[8, 10] <- "ppod"        # ppod   ERROR: WHY does prev.lbl not work with spaces???
      M[9, 10] <-  ""           # (1 - ppod) = "n(N - dec.pos)"

    } # (p.lbl ==...

    # } else if (by == "dccd") {  # (d) 1st by decision, then by condition:

  } else { # ANY other by-setting:

    # 4 types of edge labels: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
    if (p.lbl == "nam") {  # default labels: names of essential probabilities + their complements:

      M[2, 1] <- "ppod"        # ppod  ERROR: WHY does prev.lbl not work with spaces???
      M[3, 1] <- "(1 - ppod)"  # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"         # PPV
      M[6, 2] <- "FDR"         # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- "FOR"         # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"         # NPV

      M[4, 8]  <- "sens"         # "sensitivity"
      M[5, 8]  <- "mirt"         # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 9]  <- "fart"         # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 9]  <- "spec"         # "specificity"
      M[8, 10] <- "prev"         # "prevalence"
      M[9, 10] <- "(1 - prev)"   # "(N - n.true)"

    } else if (p.lbl == "num") {  # numeric values (rounded to 3 decimals):

      M[2, 1] <- round(ppod, 3)        # ppod
      M[3, 1] <- round((1 - ppod), 3)  # "n(N - dec.pos)"
      M[4, 2] <- round(PPV, 3)         # PPV
      M[6, 2] <- round((1 - PPV), 3)   # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- round((1 - NPV), 3)   # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- round(NPV, 3)         # NPV

      M[4, 8]  <- round(sens, 3)
      M[5, 8]  <- round((1 - sens), 3)  # mirt = (1 - sens) = "n(true - hi)"
      M[6, 9]  <- round((1 - spec), 3)  # fart = (1 - spec) = "n(false - cr)"
      M[7, 9]  <- round(spec, 3)
      M[8, 10] <- round(prev, 3)
      M[9, 10] <- round((1 - prev), 3)  # (1 - prev) "n(N - true)"

    } else if (p.lbl == "mix") {  # mixed labels: essential names + numeric complements:

      M[2, 1] <- "ppod"                # ppod  ERROR: WHY does prev.lbl not work with spaces???
      M[3, 1] <- round((1 - ppod), 3)  # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"                 # PPV
      M[6, 2] <- round((1 - PPV), 3)   # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- round((1 - NPV), 3)   # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"                 # NPV

      M[4, 8]  <- "sens"                # "sensitivity"
      M[5, 8]  <- round((1 - sens), 3)  # mirt = (1 - sens) = "n(true - hi)"
      M[6, 9]  <- round((1 - spec), 3)  # fart = (1 - spec) = "n(false - cr)"
      M[7, 9]  <- "spec"                # "specificity"
      M[8, 10] <- "prev"                # "prevalence"
      M[9, 10] <- round((1 - prev), 3)  # (1 - prev) "n(N - true)"

    } else {  # "min" minimal labels:

      M[2, 1] <- "ppod"        # ppod   ERROR: WHY does prev.lbl not work with spaces???
      M[3, 1] <-  ""           # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"         # PPV
      M[6, 2] <- ""            # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- ""            # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"         # NPV

      M[4, 8]  <- "sens"         # "sensitivity"
      M[5, 8]  <- ""             # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 9]  <- ""             # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 9]  <- "spec"         # "specificity"
      M[8, 10] <- "prev"         # "prevalence"
      M[9, 10] <- ""             # "(N - n.true)"

    } # (p.lbl ==...

  } # if (by...)


  ## (5) Distinguish 4 plot types (based on area setting): ----------

  ## 5a. Default case: Rectangles of same width and height (non-proportional)
  if (area == "no") {

    if (by == "cd" || by == "dc") {  # (a) by condition OR (b) by decision:

      ## Box size and proportions:
      x.pop <- .11    # basic width of population box
      x.y.pop <- 2/3  # basic proportion is rectangular (width > height)

      ## Collect all sizes and proportions:
      x.boxes <- rep(x.pop, 7)     # all boxes have the same width
      x.y.prop <- rep(x.y.pop, 7)  # all boxes have the same proportion

    } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

      x.pop <- .10    # basic width of population box
      x.y.pop <- 5/9  # basic proportion is rectangular (width > height)

      ## Collect all sizes and proportions:
      x.boxes <- rep(x.pop, 10)     # all boxes have the same width
      x.y.prop <- rep(x.y.pop, 10)  # all boxes have the same proportion

    } # if (by...)

  } # if (area == "no")...


  ## 5b. Squares that sum to the area of the next higher level:

  else if (area == "sq") {

    ## Level 1: Population square

    x.pop <- .10    # basic width of population box: Area N = x.pop^2
    x.y.pop <- 1/1  # square

    ## Determine all other box sizes by their relative proportions in freq:

    if (by == "cd") {  # (a) by condition:

      ## Level 2: cond is true vs. false

      x.true  <- sqrt(n.true/N  * x.pop^2)  # cond.true cases
      x.false <- sqrt(n.false/N * x.pop^2)  # cond.false cases

      if (!all.equal(x.pop^2, sum(c(x.true^2, x.false^2), na.rm = TRUE))) {
        warning("sumtree 1a: Sum of cond.true and cond.false area differs from population area.")
      }

    } else if (by == "dc") {  # (b) by decision:

      ## Level 2: dec is pos vs. neg

      x.pos  <- sqrt(n.pos/N * x.pop^2)  # dec.pos cases
      x.neg <- sqrt(n.neg/N * x.pop^2)   # dec.neg cases

      if (!all.equal(x.pop^2, sum(c(x.pos^2, x.neg^2), na.rm = TRUE))) {
        warning("sumtree 1b: Sum of dec.pos and dec.neg area differs from population area.")
      }

    } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

      ## Level 1: Population square
      x.pop <- .08    # shrink basic width of population box: Area N = x.pop^2
      x.y.pop <- 1/1  # square

      ## Level 2 (top): cond is true vs. false
      x.true  <- sqrt(n.true/N  * x.pop^2)  # cond.true cases
      x.false <- sqrt(n.false/N * x.pop^2)  # cond.false cases

      if (!all.equal(x.pop^2, sum(c(x.true^2, x.false^2), na.rm = TRUE))) {
        warning("sumtree 1c+d: Sum of cond.true and cond.false area differs from population area.")
      }

      ## Level 2 (bot): dec is pos vs. neg
      x.pos  <- sqrt(n.pos/N * x.pop^2)  # dec.pos cases
      x.neg <- sqrt(n.neg/N * x.pop^2)   # dec.neg cases

      if (!all.equal(x.pop^2, sum(c(x.pos^2, x.neg^2), na.rm = TRUE))) {
        warning("sumtree 1c+d: Sum of dec.pos and dec.neg area differs from population area.")
      }

    } # (if by...)

    ## Level 3: 4 SDT squares (irrespective of current by option!)
    x.hi <- sqrt(n.hi/N * x.pop^2)
    x.mi <- sqrt(n.mi/N * x.pop^2)
    x.fa <- sqrt(n.fa/N * x.pop^2)
    x.cr <- sqrt(n.cr/N * x.pop^2)

    ## Checks by condition/decision:
    if (by == "cd") {  # (a) by condition:

      if (!all.equal(x.true^2, sum(c(x.hi^2, x.mi^2), na.rm = TRUE))) {
        warning("sumtree 2a: Sum of HI and MI area differs from cond.TRUE area.")
      }

      if (!all.equal(x.false^2, sum(c(x.fa^2, x.cr^2), na.rm = TRUE))) {
        warning("sumtree 3a: Sum of FA and CR area differs from cond.FALSE area.")
      }

    } else if (by == "dc") {  # (b) by decision:

      if (!all.equal(x.pos^2, sum(c(x.hi^2, x.fa^2), na.rm = TRUE))) {
        warning("sumtree 2b: Sum of HI and FA area differs from dec.POS area.")
      }

      if (!all.equal(x.neg^2, sum(c(x.mi^2, x.cr^2), na.rm = TRUE))) {
        warning("sumtree 3b: Sum of MI and CR area differs from dec.NEG area.")
      }

    } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

      if (!all.equal(x.true^2, sum(c(x.hi^2, x.mi^2), na.rm = TRUE))) {
        warning("sumtree 2c: Sum of HI and MI area differs from cond.TRUE area.")
      }

      if (!all.equal(x.false^2, sum(c(x.fa^2, x.cr^2), na.rm = TRUE))) {
        warning("sumtree 2c: Sum of FA and CR area differs from cond.FALSE area.")
      }

      if (!all.equal(x.pos^2, sum(c(x.hi^2, x.fa^2), na.rm = TRUE))) {
        warning("sumtree 2d: Sum of HI and FA area differs from dec.POS area.")
      }

      if (!all.equal(x.neg^2, sum(c(x.mi^2, x.cr^2), na.rm = TRUE))) {
        warning("sumtree 3d: Sum of MI and CR area differs from dec.NEG area.")
      }

    } # (if by...)

    ## Check (irrespective of current by option):
    if (!all.equal(x.pop^2, sum(c(x.hi^2, x.mi^2, x.fa^2, x.cr^2), na.rm = TRUE))) {
      warning("sumtree 4: Population area differs from the area sum of all 4 SDT cases.")
    }

    ## Collect box sizes and proportions:

    if (by == "cd") {  # (a) by condition:

      ## Widths and proportions of 7 boxes:
      x.boxes <- c(x.pop,
                   x.true, x.false,
                   x.hi, x.mi, x.fa, x.cr)
      x.y.prop <- rep(x.y.pop, 7) # all 7 boxes have the same proportion (squares)

    } else if (by == "dc") {  # (b) by decision:

      ## Widths and proportions of 7 boxes:
      x.boxes <- c(x.pop,
                   x.pos, x.neg,
                   x.hi, x.mi, x.fa, x.cr)
      x.y.prop <- rep(x.y.pop, 7) # all 7 boxes have the same proportion (squares)

    } else if (by == "cddc") {  # (c) by condition and by decision:

      ## Widths and proportions of 10 boxes:
      x.boxes <- c(x.pop,
                   x.true, x.false,
                   x.hi, x.mi, x.fa, x.cr,
                   x.pos, x.neg,
                   x.pop)
      x.y.prop <- rep(x.y.pop, 10) # all 10 boxes have the same proportion (squares)

    } else if (by == "dccd") {  # (d) 1st by decision, 2nd by condition:

      ## Widths and proportions of 10 boxes:
      x.boxes <- c(x.pop,
                   x.pos, x.neg,
                   x.hi, x.mi, x.fa, x.cr,
                   x.true, x.false,
                   x.pop)
      x.y.prop <- rep(x.y.pop, 10) # all 10 boxes have the same proportion (squares)

    } # (if by...)

  } # (area == "sq")


  ## 5c. Rectangles that sum to the area of the next higher level:

  else if (area == "hr") {

    ## Level 1: Population square

    x.pop <- .10   # basic width x of population box: Area N = x.pop^2
    x.y.pop <- 1/1 # square

    ## Determine all other box sizes by their relative proportions in freq:

    if (by == "cd") {  # (a) by condition:

      ## Level 2: 2 vertical rectangles

      x.true <- (n.true/N) * x.pop  # scale x.pop by proportion of cond.true cases
      x.y.true <- x.pop/x.true

      x.false <- n.false/N * x.pop  # scale x.pop by proportion of cond.false cases
      x.y.false <- x.pop/x.false

      if (!all.equal(x.pop^2, sum(c((x.true * x.pop), (x.false * x.pop)), na.rm = TRUE))) {
        warning("hrectree 1a: Sum of cond.true + cond.false areas differs from population area.")
      }

      ## Level 3:
      ## 4 rectangles with heights matching the widths of the Level 2 rectangles
      ## (and their widths summing up to the height of the Level 2 rectangles)

      ## Box cond.true = hi + mi:
      x.hi <- (n.hi/n.true) * x.pop  # hi: scale x.pop by sens = n.hi/n.true
      x.y.hi <- x.true/x.hi

      x.mi <-  (1 - (n.hi/n.true)) * x.pop  # mi: scale x.pop by mirt = (1 - sens)
      x.y.mi <- x.true/x.mi

      if (!all.equal((x.true * x.pop),
                     sum(c((x.hi * x.true), (x.mi * x.true)), na.rm = TRUE))) {
        warning("hrectree 2a: Sum of HI + MI area differs from cond.TRUE area.")
      }

      ## Box cond.false = fa + cr:
      x.cr <- (n.cr/n.false) * x.pop  # cr: scale x.pop by spec = n.cr/n.false
      x.y.cr <- x.false/x.cr

      x.fa <- (1 - (n.cr/n.false)) * x.pop  # fa: scale x.pop by (1 - spec)
      x.y.fa <- x.false/x.fa

      if (!all.equal((x.false * x.pop),
                     sum(c((x.fa * x.false), (x.cr * x.false)), na.rm = TRUE))) {
        warning("hrectree 3a: Sum of FA + CR area differs from cond.FALSE area.")
      }

      if (!all.equal((x.pop^2),
                     sum(c((x.hi * x.true), (x.mi * x.true), (x.fa * x.false), (x.cr * x.false)), na.rm = TRUE))) {
        warning("hrectree 4a: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 7 boxes:
      x.boxes <-  c(x.pop,  x.true, x.false,  x.hi, x.mi, x.fa, x.cr)  # specific widths
      x.y.prop <- c(x.y.pop,  x.y.true, x.y.false,  x.y.hi, x.y.mi, x.y.fa, x.y.cr)  # specific proportions


    } else if (by == "dc") {  # (b) by decision:


      ## Level 2: 2 vertical rectangles

      x.pos <- (n.pos/N) * x.pop  # scale x.pop by proportion of dec.pos cases
      x.y.pos <- x.pop/x.pos

      x.neg <- n.neg/N * x.pop  # scale x.pop by proportion of dec.neg cases
      x.y.neg <- x.pop/x.neg

      if (!all.equal(x.pop^2, sum(c((x.pos * x.pop), (x.neg * x.pop)), na.rm = TRUE))) {
        warning("hrectree 1b: Sum of dec.pos + dec.neg areas differs from population area.")
      }

      ## Level 3:
      ## 4 rectangles with heights matching the widths of the Level 2 rectangles
      ## (and their widths summing up to the height of the Level 2 rectangles)

      ## Box dec.pos = hi + FA:
      x.hi <- (n.hi/n.pos) * x.pop  # hi: scale x.pop by PPV = n.hi/dec.pos
      x.y.hi <- x.pos/x.hi

      # FA (now belongs to dec.pos):
      x.fa <-  (1 - (n.hi/n.pos)) * x.pop  # FA: scale x.pop by (1 - PPV)
      x.y.fa <- x.pos/x.fa

      if (!all.equal((x.pos * x.pop),
                     sum(c((x.hi * x.pos), (x.fa * x.pos)), na.rm = TRUE))) {
        warning("hrectree 2b: Sum of HI + FA area differs from dec.POS area.")
      }

      ## Box dec.neg = cr + MI:
      x.cr <- (n.cr/n.neg) * x.pop  # cr: scale x.pop by NPV = n.cr/dec.false
      x.y.cr <- x.neg/x.cr

      # MI (now belongs to dec.neg):
      x.mi <- (1 - (n.cr/n.neg)) * x.pop  # MI: scale x.pop by (1 - NPV)
      x.y.mi <- x.neg/x.mi

      if (!all.equal((x.neg * x.pop),
                     sum(c((x.mi * x.neg), (x.cr * x.neg)), na.rm = TRUE))) {
        warning("hrectree 3b: Sum of MI + CR area differs from dec.NEG area.")
      }

      if (!all.equal((x.pop^2),
                     sum(c((x.hi * x.pos),
                           (x.fa * x.pos),
                           (x.mi * x.neg),
                           (x.cr * x.neg)), na.rm = TRUE))) {
        warning("hrectree 4b: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 7 boxes:
      x.boxes <-  c(x.pop,
                    x.pos, x.neg,
                    x.hi, x.mi, x.fa, x.cr)  # specific widths
      x.y.prop <- c(x.y.pop,
                    x.y.pos, x.y.neg,
                    x.y.hi, x.y.mi, x.y.fa, x.y.cr)  # specific proportions


    } else if (by == "cddc") {  # (c) by condition and by decision:


      ## Level 1: Population square
      x.pop <- .08    # shrink basic width of population box: Area N = x.pop^2
      x.y.pop <- 1/1  # square

      ## Level 2 (top): 2 vertical rectangles by condition:
      x.true <- (n.true/N) * x.pop  # scale x.pop by proportion of cond.true cases
      x.y.true <- x.pop/x.true

      x.false <- n.false/N * x.pop  # scale x.pop by proportion of cond.false cases
      x.y.false <- x.pop/x.false

      if (!all.equal(x.pop^2, sum(c((x.true * x.pop), (x.false * x.pop)), na.rm = TRUE))) {
        warning("hrectree 1cddc: Sum of cond.true + cond.false areas differs from population area.")
      }

      ## Level 2 (bot): 2 vertical rectangles by decision:
      x.pos <- (n.pos/N) * x.pop  # scale x.pop by proportion of dec.pos cases
      x.y.pos <- x.pop/x.pos

      x.neg <- n.neg/N * x.pop  # scale x.pop by proportion of dec.neg cases
      x.y.neg <- x.pop/x.neg

      if (!all.equal(x.pop^2, sum(c((x.pos * x.pop), (x.neg * x.pop)), na.rm = TRUE))) {
        warning("hrectree 1c: Sum of dec.pos + dec.neg areas differs from population area.")
      }


      ## Level 3:  (Note: Simply copied from (by == "cd") above!)
      ## 4 rectangles with heights matching the widths of the Level 2 (top) rectangles
      ## (and their widths summing up to the height of the Level 2 rectangles of (top) by condition)

      ## Box cond.true = hi + mi:
      x.hi <- (n.hi/n.true) * x.pop  # hi: scale x.pop by sens = n.hi/n.true
      x.y.hi <- x.true/x.hi

      x.mi <-  (1 - (n.hi/n.true)) * x.pop  # mi: scale x.pop by mirt = (1 - sens)
      x.y.mi <- x.true/x.mi

      if (!all.equal((x.true * x.pop),
                     sum(c((x.hi * x.true), (x.mi * x.true)), na.rm = TRUE))) {
        warning("hrectree 2c: Sum of HI + MI area differs from cond.TRUE area.")
      }

      ## Box cond.false = fa + cr:
      x.cr <- (n.cr/n.false) * x.pop  # cr: scale x.pop by spec = n.cr/n.false
      x.y.cr <- x.false/x.cr

      x.fa <- (1 - (n.cr/n.false)) * x.pop  # fa: scale x.pop by (1 - spec)
      x.y.fa <- x.false/x.fa

      if (!all.equal((x.false * x.pop),
                     sum(c((x.fa * x.false), (x.cr * x.false)), na.rm = TRUE))) {
        warning("hrectree 3c: Sum of FA + CR area differs from cond.FALSE area.")
      }

      if (!all.equal((x.pop^2),
                     sum(c((x.hi * x.true), (x.mi * x.true), (x.fa * x.false), (x.cr * x.false)), na.rm = TRUE))) {
        warning("hrectree 4c: Population area differs from the area sum of all 4 SDT cases.")
      }


      ## Collect widths and proportions of 10 boxes:
      x.boxes <-  c(x.pop,  x.true, x.false,
                    x.hi, x.mi, x.fa, x.cr,
                    x.pos, x.neg,  x.pop)  # specific widths
      x.y.prop <- c(x.y.pop,  x.y.true, x.y.false,
                    x.y.hi, x.y.mi, x.y.fa, x.y.cr,
                    x.y.pos, x.y.neg,  x.y.pop)  # specific proportions


    } else if (by == "dccd") {  # (d) 1st by decision, 2nd by condition:

      ## Level 1: Population square
      x.pop <- .08    # shrink basic width of population box: Area N = x.pop^2
      x.y.pop <- 1/1  # square


      ## Level 2 (top): 2 vertical rectangles by decision:
      x.pos <- (n.pos/N) * x.pop  # scale x.pop by proportion of dec.pos cases
      x.y.pos <- x.pop/x.pos

      x.neg <- (n.neg/N) * x.pop  # scale x.pop by proportion of dec.neg cases
      x.y.neg <- x.pop/x.neg

      if (!all.equal(x.pop^2, sum(c((x.pos * x.pop), (x.neg * x.pop)), na.rm = TRUE))) {
        warning("hrectree 1d: Sum of dec.pos + dec.neg areas differs from population area.")
      }


      ## Level 2 (bot): 2 vertical rectangles by condition:
      x.true <- (n.true/N) * x.pop  # scale x.pop by proportion of cond.true cases
      x.y.true <- x.pop/x.true

      x.false <- (n.false/N) * x.pop  # scale x.pop by proportion of cond.false cases
      x.y.false <- x.pop/x.false

      if (!all.equal(x.pop^2, sum(c((x.true * x.pop), (x.false * x.pop)), na.rm = TRUE))) {
        warning("hrectree 1d: Sum of cond.true + cond.false areas differs from population area.")
      }


      ## Level 3:
      ## 4 rectangles with heights matching the widths of the Level 2 (top) rectangles
      ## (and their widths summing up to the height of the Level 2 rectangles of (top) by condition)

      ## Box dec.POS = hi + FA:
      x.hi <- (n.hi/n.pos) * x.pop  # hi: scale x.pop by PPV = n.hi/n.pos
      x.y.hi <- x.pos/x.hi

      x.fa <- (1 - (n.hi/n.pos)) * x.pop  # FA: scale x.pop by (1 - PPV) = FDR
      x.y.fa <- x.pos/x.fa

      if (!all.equal((x.pos * x.pop),
                     sum(c((x.hi * x.pos), (x.fa * x.pos)), na.rm = TRUE))) {
        warning("hrectree 2d: Sum of HI + FA area differs from dec.POS area.")
      }

      ## Box dec.NEG = cr + MI:
      x.cr <- (n.cr/n.neg) * x.pop  # cr: scale x.pop by NPV = n.cr/n.neg
      x.y.cr <- x.neg/x.cr

      x.mi <-  (1 - (n.cr/n.neg)) * x.pop  # MI: scale x.pop by (1 - NPV) = FOR
      x.y.mi <- x.neg/x.mi

      if (!all.equal((x.neg * x.pop),
                     sum(c((x.mi * x.neg), (x.cr * x.neg)), na.rm = TRUE))) {
        warning("hrectree 3d: Sum of MI + CR area differs from dec.NEG area.")
      }

      if (!all.equal((x.pop^2),
                     sum(c((x.hi * x.pos), (x.mi * x.neg), (x.fa * x.pos), (x.cr * x.neg)), na.rm = TRUE))) {
        warning("hrectree 4d: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 10 boxes:
      x.boxes <-  c(x.pop,
                    x.pos, x.neg,
                    x.hi, x.mi, x.fa, x.cr,
                    x.true, x.false,
                    x.pop)        # specific widths
      x.y.prop <- c(x.y.pop,
                    x.y.pos, x.y.neg,
                    x.y.hi, x.y.mi, x.y.fa, x.y.cr,
                    x.y.true, x.y.false,
                    x.y.pop)  # specific proportions

    } # (if by...)

  } # if (area == "hr")...


  ## 5d. Rectangles that sum to the area of the next higher level
  ##     (= 3. flipped by 90 degrees on Level 3 to correspond to 4 SDT areas of mosaic plot):
  else if (area == "vr") {

    ## Level 1: Population square
    x.pop <- .10   # basic width x of population box: Area N = x.pop^2
    x.y.pop <- 1/1 # square

    ## Determine all other box sizes by their relative proportions in freq:

    if (by == "cd") {  # (a) by condition:

      ## Level 2:
      ## 2 vertical rectangles for a) cond.true vs.cond.false:
      x.true <- (n.true/N) * x.pop # scale x.pop by proportion true
      x.y.true <- x.pop/x.true

      x.false <- (n.false/N) * x.pop # scale x.pop by proportion false
      x.y.false <- x.pop/x.false

      if (!all.equal(x.pop^2, sum(c((x.true * x.pop), (x.false * x.pop)), na.rm = TRUE))) {
        warning("vrectree 1a: Sum of cond.TRUE + cond.FALSE areas differs from Population area.")
      }

      ## Level 3:
      ## 4 rectangles with widths matching the widths of the Level 2 rectangles
      ## (and their heights summing up to the height of the Level 2 rectangles)

      x.hi <- x.true # keep constant
      x.y.hi <- x.y.true * (n.hi/n.true) # scale previous prop by prop hi

      x.mi <- x.true # keep constant
      x.y.mi <- x.y.true * (n.mi/n.true) # scale previous prop by prop mi

      if (!all.equal((x.true * x.pop),
                     sum(c((x.hi * (x.hi * x.y.hi)), (x.mi * (x.mi * x.y.mi))), na.rm = TRUE))) {
        warning("vrectree 2a: Sum of HI + MI area differs from Cond TRUE area.")
      }

      x.fa <- x.false # keep constant
      x.y.fa <- x.y.false * (n.fa/n.false) # scale previous prop by prop fa

      x.cr <- x.false # keep constant
      x.y.cr <- x.y.false * (n.cr/n.false) # scale previous prop by prop cr

      if (!all.equal((x.false * x.pop),
                     sum(c((x.fa * (x.fa * x.y.fa)), (x.cr * (x.cr * x.y.cr))), na.rm = TRUE))) {
        warning("vrectree 3a: Sum of FA + CR area differs from Cond FALSE area.")
      }

      if (!all.equal((x.pop^2),
                     sum(c((x.hi * (x.hi * x.y.hi)),
                           (x.mi * (x.mi * x.y.mi)),
                           (x.fa * (x.fa * x.y.fa)),
                           (x.cr * (x.cr * x.y.cr))), na.rm = TRUE))) {
        warning("vrectree 4a: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 7 boxes:
      x.boxes <- c(x.pop,  x.true, x.false,
                   x.hi, x.mi, x.fa, x.cr) # specific widths
      x.y.prop <- c(x.y.pop,  x.y.true, x.y.false,
                    x.y.hi, x.y.mi, x.y.fa, x.y.cr) # specific proportions

    } else if (by == "dc") {  # (b) by decision:

      ## Level 2:
      ## 2 vertical rectangles for b) dec.pos vs. dec.neg:
      x.pos <- (n.pos/N) * x.pop  # scale x.pop by proportion of dec.pos
      x.y.pos <- x.pop/x.pos

      x.neg <- (n.neg/N) * x.pop  # scale x.pop by proportion of dec.neg
      x.y.neg <- x.pop/x.neg

      if (!all.equal(x.pop^2, sum(c((x.pos * x.pop), (x.neg * x.pop)), na.rm = TRUE))) {
        warning("vrectree 1b: Sum of dec.POS + dec.FALSE areas differs from Population area.")
      }

      ## Level 3:
      ## 4 rectangles with widths matching the widths of the Level 2 rectangles
      ## (and their heights summing up to the height of the Level 2 rectangles)

      x.hi <- x.pos  # keep constant to Level 2
      x.y.hi <- x.y.pos * (n.hi/n.pos)  # scale previous prop by prop n.hi/n.pos = PPV

      # FA (now belongs to dec.pos):
      x.fa <- x.pos  # keep constant to Level 2
      x.y.fa <- x.y.pos * (n.fa/n.pos)  # scale previous prop by prop n.fa/n.pos = (1 - PPV)

      if (!all.equal((x.pos * x.pop),
                     sum(c((x.hi * (x.hi * x.y.hi)), (x.fa * (x.fa * x.y.fa))), na.rm = TRUE))) {
        warning("vrectree 2b: Sum of HI + FA area differs from dec.POS area.")
      }

      x.cr <- x.neg  # keep constant to Level 2
      x.y.cr <- x.y.neg * (n.cr/n.neg)  # scale previous prop by prop n.cr/n.neg = NPV

      # MI (now belongs to dec.neg):
      x.mi <- x.neg  # keep constant to Level 2
      x.y.mi <- x.y.neg * (n.mi/n.neg)  # scale previous prop by prop n.mi/n.neg = (1 - NPV)

      if (!all.equal((x.neg * x.pop),
                     sum(c((x.mi * (x.mi * x.y.mi)), (x.cr * (x.cr * x.y.cr))), na.rm = TRUE))) {
        warning("vrectree 3b: Sum of MI + CR area differs from dec.NEG area.")
      }

      if (!all.equal((x.pop^2),
                     sum(c((x.hi * (x.hi * x.y.hi)),
                           (x.mi * (x.mi * x.y.mi)),
                           (x.fa * (x.fa * x.y.fa)),
                           (x.cr * (x.cr * x.y.cr))), na.rm = TRUE))) {
        warning("vrectree 4b: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 7 boxes:
      x.boxes <- c(x.pop,  x.pos, x.neg,
                   x.hi, x.mi, x.fa, x.cr) # specific widths
      x.y.prop <- c(x.y.pop,  x.y.pos, x.y.neg,
                    x.y.hi, x.y.mi, x.y.fa, x.y.cr) # specific proportions


    } else if (by == "cddc") {  # (c) by condition and by decision:


      ## Level 1: Population square
      x.pop <- .08    # shrink basic width of population box: Area N = x.pop^2
      x.y.pop <- 1/1  # square


      ## Level 2 (top):
      ## 2 vertical rectangles for a) cond.true vs.cond.false:
      x.true <- (n.true/N) * x.pop # scale x.pop by proportion true
      x.y.true <- x.pop/x.true

      x.false <- (n.false/N) * x.pop # scale x.pop by proportion false
      x.y.false <- x.pop/x.false

      if (!all.equal(x.pop^2, sum(c((x.true * x.pop), (x.false * x.pop)), na.rm = TRUE))) {
        warning("vrectree 1c: Sum of cond.TRUE + cond.FALSE areas differs from Population area.")
      }


      ## Level 2 (bot):
      ## 2 vertical rectangles for b) dec.pos vs. dec.neg:
      x.pos <- (n.pos/N) * x.pop  # scale x.pop by proportion of dec.pos
      x.y.pos <- x.pop/x.pos

      x.neg <- (n.neg/N) * x.pop  # scale x.pop by proportion of dec.neg
      x.y.neg <- x.pop/x.neg

      if (!all.equal(x.pop^2, sum(c((x.pos * x.pop), (x.neg * x.pop)), na.rm = TRUE))) {
        warning("vrectree 1c: Sum of dec.POS + dec.FALSE areas differs from Population area.")
      }


      ## Level 3:  (Note: Simply copied from (by == "cd") above!)
      ## 4 rectangles with widths matching the widths of the Level 2 rectangles
      ## (and their heights summing up to the height of the Level 2 rectangles)

      x.hi <- x.true  # keep constant
      x.y.hi <- x.y.true * (n.hi/n.true)  # scale previous prop by prop hi

      x.mi <- x.true # keep constant
      x.y.mi <- x.y.true * (n.mi/n.true)  # scale previous prop by prop mi

      if (!all.equal((x.true * x.pop),
                     sum(c((x.hi * (x.hi * x.y.hi)), (x.mi * (x.mi * x.y.mi))), na.rm = TRUE))) {
        warning("vrectree 2c: Sum of HI + MI area differs from Cond TRUE area.")
      }

      x.fa <- x.false  # keep constant
      x.y.fa <- x.y.false * (n.fa/n.false)  # scale previous prop by prop fa

      x.cr <- x.false  # keep constant
      x.y.cr <- x.y.false * (n.cr/n.false)  # scale previous prop by prop cr

      if (!all.equal((x.false * x.pop),
                     sum(c((x.fa * (x.fa * x.y.fa)), (x.cr * (x.cr * x.y.cr))), na.rm = TRUE))) {
        warning("vrectree 3c: Sum of FA + CR area differs from Cond FALSE area.")
      }

      if (!all.equal((x.pop^2),
                     sum(c((x.hi * (x.hi * x.y.hi)),
                           (x.mi * (x.mi * x.y.mi)),
                           (x.fa * (x.fa * x.y.fa)),
                           (x.cr * (x.cr * x.y.cr))), na.rm = TRUE))) {
        warning("vrectree 4c: Population area differs from the area sum of all 4 SDT cases.")
      }


      ## Collect widths and proportions of 10 boxes:
      x.boxes <- c(x.pop,  x.true, x.false,
                   x.hi, x.mi, x.fa, x.cr,
                   x.pos, x.neg,  x.pop)             # specific widths
      x.y.prop <- c(x.y.pop,  x.y.true, x.y.false,
                    x.y.hi, x.y.mi, x.y.fa, x.y.cr,
                    x.y.pos, x.y.neg,  x.y.pop)      # specific proportions


    } else if (by == "dccd") {  # (d) 1st by decision, 2nd by condition:


      ## Level 1: Population square
      x.pop <- .08    # shrink basic width of population box: Area N = x.pop^2
      x.y.pop <- 1/1  # square


      ## Level 2 (top):
      ## 2 vertical rectangles for b) dec.pos vs. dec.neg:
      x.pos <- (n.pos/N) * x.pop  # scale x.pop by proportion of dec.pos
      x.y.pos <- x.pop/x.pos

      x.neg <- (n.neg/N) * x.pop  # scale x.pop by proportion of dec.neg
      x.y.neg <- x.pop/x.neg

      if (!all.equal(x.pop^2, sum(c((x.pos * x.pop), (x.neg * x.pop)), na.rm = TRUE))) {
        warning("vrectree 1d: Sum of dec.POS + dec.FALSE areas differs from Population area.")
      }

      ## Level 2 (bot):
      ## 2 vertical rectangles for a) cond.true vs.cond.false:
      x.true <- (n.true/N) * x.pop # scale x.pop by proportion true
      x.y.true <- x.pop/x.true

      x.false <- (n.false/N) * x.pop # scale x.pop by proportion false
      x.y.false <- x.pop/x.false

      if (!all.equal(x.pop^2, sum(c((x.true * x.pop), (x.false * x.pop)), na.rm = TRUE))) {
        warning("vrectree 1d: Sum of cond.TRUE + cond.FALSE areas differs from Population area.")
      }


      ## Level 3:
      ## 4 rectangles with widths matching the widths of the Level 2 rectangles
      ## (and their heights summing up to the height of the Level 2 rectangles)

      ## dec.POS cases:
      x.hi <- x.pos  # keep constant
      x.y.hi <- x.y.pos * (n.hi/n.pos)  # scale previous prop by prop n.hi/n.pos (PPV)

      x.fa <- x.pos  # keep constant
      x.y.fa <- x.y.pos * (n.fa/n.pos)  # scale previous prop by prop n.FA/n.pos (1 - PPV)

      if (!all.equal((x.pos * x.pop),
                     sum(c((x.hi * (x.hi * x.y.hi)), (x.fa * (x.fa * x.y.fa))), na.rm = TRUE))) {
        warning("vrectree 2d: Sum of HI + FA area differs from dec.POS area.")
      }


      ## dec.NEG cases:
      x.cr <- x.neg  # keep constant
      x.y.cr <- x.y.neg * (n.cr/n.neg)  # scale previous prop by prop n.cr/n.neg (NPV)

      x.mi <- x.neg # keep constant
      x.y.mi <- x.y.neg * (n.mi/n.neg)  # scale previous prop by prop n.mi/n.neg (1 - NPV)


      if (!all.equal((x.neg * x.pop),
                     sum(c((x.mi * (x.mi * x.y.mi)), (x.cr * (x.cr * x.y.cr))), na.rm = TRUE))) {
        warning("vrectree 3d: Sum of MI + CR area differs from dec.NEG area.")
      }

      if (!all.equal((x.pop^2),
                     sum(c((x.hi * (x.hi * x.y.hi)),
                           (x.mi * (x.mi * x.y.mi)),
                           (x.fa * (x.fa * x.y.fa)),
                           (x.cr * (x.cr * x.y.cr))), na.rm = TRUE))) {
        warning("vrectree 4d: Population area differs from the area sum of all 4 SDT cases.")
      }


      ## Collect widths and proportions of 10 boxes:
      x.boxes <- c(x.pop,
                   x.pos, x.neg,
                   x.hi, x.mi, x.fa, x.cr,
                   x.true, x.false,
                   x.pop)             # specific widths
      x.y.prop <- c(x.y.pop,
                    x.y.pos, x.y.neg,
                    x.y.hi, x.y.mi, x.y.fa, x.y.cr,
                    x.y.true, x.y.false,
                    x.y.pop)      # specific proportions

    } # (if by...)

  } # (area == "vr")...

  else {  # 5e. ANY other area-setting:

    if (by == "cd" || by == "dc") {  # (a) by condition OR (b) by decision:

      ## Box size and proportions:
      x.pop <- .10    # basic width of population box
      x.y.pop <- 2/3  # basic proportion is rectangular (width > height)

      ## Collect all sizes and proportions:
      x.boxes <- rep(x.pop, 7)     # all boxes have the same width
      x.y.prop <- rep(x.y.pop, 7)  # all boxes have the same proportion

    } else {  # (c) + (d) + ANY other by-setting:

      x.pop <- .05    # basic width of population box
      x.y.pop <- 1/1  # basic proportion is square (width = height)

      ## Collect all sizes and proportions:
      x.boxes <- rep(x.pop, 10)     # all boxes have the same width
      x.y.prop <- rep(x.y.pop, 10)  # all boxes have the same proportion

    } # if (by...)

  } # if (area == ...)



  ## (6) Plot matrix M (from diagram package): ----------

  if (by == "cd" || by == "dc") {  # (a) by condition OR (b) by decision:

    box.pos = c(1, 2, 4)  # 7 boxes

  } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

    box.pos = c(1, 2, 4, 2, 1)  # 10 boxes

  } else { # ANY other by-setting:

    box.pos = c(1, 2, 4, 2, 1)  # 10 boxes

  } # if (by...)


  pp <- diagram::plotmat(M, # square coefficient matrix, specifying the links (rows = to, cols = from)
                         pos = box.pos,  # 7 or 10 boxes
                         curve = 0.0, # no curve (> 0 curve left, < 0 curve right)
                         name = names,
                         relsize	= .98, # a scaling factor for the size of the graph
                         lwd = lwd,  # width of arrows
                         ## Boxes:
                         box.size = x.boxes,   # widths of boxes
                         box.prop = x.y.prop,  # proportionality (length/width) ratio of boxes
                         box.type = "rect",    # "rect", "ellipse", "diamond", "circle", "hexa", "multi", "none"
                         box.col = col.boxes,  # scalar or vector of length 7.
                         # c(col.N, col.true, col.false, col.hi, col.mi, col.fa, col.cr), # WAS: "lightyellow"
                         box.lcol = col.border,  # col.boxes,
                         box.lwd = box.lwd,  # set to 0.001 to show boxes without borders (but =0 yields error)
                         lcol = col.border,  # default color for box and arrow lines
                         ## Text in Boxes:
                         txt.col = col.txt,
                         box.cex = box.cex,  # relative size of text in boxes
                         txt.font = 1,       # 1 = plain, 2 = bold, ...
                         ## Arrows:
                         cex.txt = .80,  # relative size of arrow text
                         arr.pos = .50,  # relative position of arrowhead on arrow segment/curve
                         arr.type = "triangle", # one of "curved", "triangle", "circle", "ellipse", "T", "simple"
                         arr.length = .20,
                         arr.width = .15,
                         arr.col = col.border,
                         shadow.size = cex.shadow,  # .005
                         shadow.col = col.shadow    #,
                         # main = paste0(title.lbl, ":\n", "Sum tree of natural frequencies (N = ", N, ")")
  )

  ## (7) Title: ----------
  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  if ((by == "cd") || (by == "dc")) {type.lbl <- "Tree"} else {type.lbl <- "Network"}

  if (area == "no") {area.lbl <- ""}
  else if (area == "sq") {area.lbl <- "Areas represent relative frequencies"}
  else if (area == "hr") {area.lbl <- "Areas represent relative frequencies"}
  else if (area == "vr") {area.lbl <- "Areas represent relative frequencies"}
  else {area.lbl <- ""}  # to prevent errors for other entries

  if (by == "cd") {by.lbl <- "(by condition)"}
  else if (by == "dc") {by.lbl <- "(by decision)"}
  else if (by == "cddc") {by.lbl <- "(by condition and decision)"}
  else if (by == "dccd") {by.lbl <- "(by decision and condition)"}
  else {by.lbl <- ""}  # to prevent errors for other entries

  cur.title.lbl <- paste0(title.lbl, type.lbl, " of frequencies and probabilities ", by.lbl)  # , "(N = ", N, ")")
  title(cur.title.lbl, adj = 0.5, line = 1.0, font.main = 1)  # (centered, raised, normal font)


  ## (8) Margin text: ----------

  ## (a) by condition: 3 basic probabilities
  cur.cond.lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  mtext(cur.cond.lbl, side = 1, line = 2, adj = 0, col = grey(.33, .99), cex = .85)  # print label

  # (b) by decision:
  if (by != "cd") {

    cur.dec.lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label
    mtext(cur.dec.lbl, side = 1, line = 3, adj = 0, col = grey(.33, .99), cex = .85)  # print label

  } # else {
  #   cur.dec.lbl <- ""
  # }

  ## (c) Accuracy: Compute and show accuracy metrics
  if (show.accu) {
    cur.accu <- comp_accu(hi = n.hi, mi = n.mi, fa = n.fa, cr = n.cr, w = w.acc)  # compute accuracy info
    cur.accu.lbl <- make_accu_lbl(acc = cur.accu$acc, w = w.acc, wacc = cur.accu$wacc, mcc = cur.accu$mcc)  # use utility function
    mtext(cur.accu.lbl, side = 1, line = 2, adj = 1, col = grey(.33, .99), cex = .85)  # print label
  }

  ## (d) Note that areas represent frequencies:
  if (area != "no") {

    cur.area.lbl <- paste0("(", area.lbl, ")")
    mtext(cur.area.lbl, side = 1, line = 3, adj = 1, col = grey(.33, .99), cex = .85)  # print label

  }


  ## (9) Return what? : ----------
  # return(pp)      # returns diagram object
  # return()        # returns nothing
  # return("nice")  # returns nothing

}

## Check:
{

  # # Plotting existing freq:
  # plot_fnet()  # => plot current freq with default options
  # plot_fnet(by = "dccd")
  # plot_fnet(area = "no")
  # plot_fnet(p.lbl = "num")
  # plot_fnet(title.lbl = "")
  # plot_fnet(N = 33)
  # plot_fnet(N = NA)
  #
  # # Computing and plotting new frequencies from probabilities:
  # plot_fnet(prev = 1/3)  # => changes prev, but uses current defaults of sens and spec
  # plot_fnet(prev = 1/3, N = 55)
  # plot_fnet(prev = 1/3, N = NA)
  # plot_fnet(prev = 1/3, round = FALSE)
  # plot_fnet(prev = .10, sens = .90, spec = 1/3, N = 100)
  # plot_fnet(prev = .10, sens = .90, spec = NA, fart = 1/3, N = 33)
  # plot_fnet(prev = .10, sens = .90, spec = 1/3, fart = NA, N = NA)
  # plot_fnet(prev = .10, sens = .90, spec = NA, fart = 1/3, N = NA)
  #
  # # Perspective options:
  # plot_fnet(by = "cd")    # => 1. Tree diagram (by condition)
  # plot_fnet(by = "dc")    # => 2. Tree diagram (by decision)
  # plot_fnet(by = "cddc")  # => 3. Network diagram (1st by condition, 2nd by decision) (default)
  # plot_fnet(by = "dccd")  # => 4. Network diagram (1st by decision, 2nd by condition)
  #
  # # Area options:
  # plot_fnet(area = "sq")  # => (default)
  # plot_fnet(area = "no")
  # plot_fnet(area = "sq", round = FALSE)
  # plot_fnet(area = "hr")
  # plot_fnet(area = "vr", round = FALSE)
  #
  # # Combining perspectives, areas, and label options:
  # plot_fnet(by = "cd", area = "sq", p.lbl = "nam")  # => by condition + squares               + probability names
  # plot_fnet(by = "cd", area = "hr", p.lbl = "num")  # => by condition + horizontal rectangles + probability numbers
  # plot_fnet(by = "dc", area = "sq", p.lbl = "num")  # => by decision  + squares               + mix of names and numbers
  # plot_fnet(by = "dc", area = "vr", p.lbl = "mix")  # => by decision  + vertical rectangles   + minimal labels
  #
  # # Rounding:
  # plot_fnet(prev = .10, sens = .70, spec = .90, N = 10, by = "cddc", area = "sq", p.lbl = "num", round = TRUE)  # => mi = 0
  # plot_fnet(prev = .10, sens = .70, spec = .90, N = 10, by = "cddc", area = "sq", p.lbl = "num", round = FALSE) # => mi = 0.3
  #
  # # Custom colors and shadows:
  # plot_fnet(prev = .08, sens = .92, spec = .95, N = 10000, area = "hr")
  # plot_fnet(area = "sq", col.boxes = "gold", col.border = "steelblue4", col.shadow = "steelblue4", cex.shadow = .008)
  # plot_fnet(N = NA, area = "vr", col.txt = "steelblue4", col.boxes = "lightyellow", col.border = grey(.3, .7), cex.shadow = .008, col.shadow = grey(.1, .9))

}

## -----------------------------------------------
## (+) ToDo:

## - Add a 3rd perspective:
##   "by correctness" or correspondence of condition and decision:
##   "dec.corr" vs. "dec.err" (i.e., diagonal of confusion matrix)

## -----------------------------------------------
## eof.

## plot_fnet.R | riskyr
## 2018 12 06
## Plot a network diagram of frequencies
## (as nodes) and probabilities (as edges)
## -----------------------------------------------
## Version 4: Generalization of plot_tree

## Options available in this version:
## - by    ... "cd", "dc", "cddc" (default), "dccd".
## - area  ... "no", "sq" (default), "hr", "vr".
## - p_lbl ... "nam", "num" (default), "mix", "min".
## - show_accu ... show current accuracy metrics (with bacc/wacc).

## Dependencies:

# library("diagram") # moved to "Imports:" in in DESCRIPTION!

## plot_fnet is a generalization of plot_tree:
## Plot a network or tree diagram of frequencies
## (as nodes) and probabilities (as edges)
## (using only necessary arguments with good defaults):

## Assuming that freq$N (+ num txt pal) is known!

## plot_fnet: Documentation: ---------

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
#' between 9 frequencies of \code{\link{freq}} and
#' and 10 probabilities of \code{\link{prob}}
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
#' @param N The number of individuals in the population.
#' A suitable value of \code{\link{N}} is computed, if not provided.
#'
#' @param round A Boolean option specifying whether computed frequencies
#' are rounded to integers. Default: \code{round = TRUE}.
#'
#' @param by A character code specifying the perspective (or 1st category by which the population is split into subsets) with 4 options:
#'   \enumerate{
#'   \item \code{"cd"} ... by condition;
#'   \item \code{"dc"} ... by decision;
#'   \item \code{"cddc"} ... 1st by condition, 2nd by decision;
#'   \item \code{"dccd"} ... 1st by decision, 2nd by condition.
#'   }
#'
#' @param area A character code specifying the area of the boxes (or their relative sizes) with
#' 4 options:
#'   \enumerate{
#'   \item \code{"no"} ... all boxes are shown with the same size;
#'   \item \code{"sq"} ... boxes are squares with area sizes scaled proportional to frequencies (default);
#'   \item \code{"hr"} ... boxes are horizontal rectangles with area sizes scaled proportional to frequencies;
#'   \item \code{"vr"} ... boxes are vertical rectangles with area sizes scaled proportional to frequencies.
#'   }
#'
#' @param p_lbl A character code specifying the type of probability information (on edges) with
#' 4 options:
#'   \enumerate{
#'   \item \code{"nam"} ... names of probabilities;
#'   \item \code{"num"} ... numeric values of probabilities (rounded to 3 decimals, default);
#'   \item \code{"mix"} ... names of essential probabilities, values of complements;
#'   \item \code{"min"} ... minimal labels: names of essential probabilities.
#'   }
#'
#' @param show_accu Option for showing current
#' accuracy metrics \code{\link{accu}} on the margin of the plot.
#' Default: \code{show_accu = TRUE}.
#'
#' @param w_acc Weigthing parameter \code{w} used to compute
#' weighted accuracy \code{w_acc} in \code{\link{comp_accu_freq}}.
#' Default: \code{w_acc = .50}.
#'
#' Various other options allow the customization of text labels and colors:
#'
#' @param title_lbl Text label for current plot title.
#' Default: \code{title_lbl = txt$scen_lbl}.
#'
#' @param popu_lbl Text label for current population \code{\link{popu}}.
#'
#' @param cond.true_lbl Text label for current cases of \code{\link{cond.true}}.
#' @param cond.false_lbl Text label for current cases of \code{\link{cond.false}}.
#'
#' @param dec.pos_lbl Text label for current cases of \code{\link{dec.pos}}.
#' @param dec.neg_lbl Text label for current cases of \code{\link{dec.neg}}.
#'
#' @param hi_lbl Text label for hits \code{\link{hi}}.
#' @param mi_lbl Text label for misses \code{\link{mi}}.
#' @param fa_lbl Text label for false alarms \code{\link{fa}}.
#' @param cr_lbl Text label for correct rejections \code{\link{cr}}.
#'
#' @param col_txt Color for text labels (in boxes).
#'
#' @param cex_lbl Scaling factor for text labels (in boxes and on arrows).
#' Default: \code{cex_lbl = .90}.
#'
#' @param box_cex Deprecated argument: Please use \code{cex_lbl} instead.
#'
#' @param col_boxes Colors of boxes (a single color or a vector with named colors matching the number of current boxes).
#' Default: Current color information contained in \code{\link{pal}}.
#'
#' @param col_border Color of borders.
#' Default: \code{col_border = grey(.33, alpha = .99)}.
#'
#' @param lwd Width of arrows.
#'
#' @param box_lwd Width of boxes.
#'
#' @param col_shadow Color of box shadows.
#' Default: \code{col_shadow = grey(.11, alpha = .99)}.
#'
#' @param cex_shadow Scaling factor of shadows (values > 0 showing shadows).
#' Default: \code{cex_shadow = 0}.
#'
#' @return Nothing (NULL).
#'
#' @examples
#' # Plotting existing freq:
#' plot_fnet()  # => plot current freq with default options
#' plot_fnet(by = "dccd")
#' plot_fnet(area = "no")
#' plot_fnet(p_lbl = "num")
#' plot_fnet(title_lbl = "")
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
#' plot_fnet(show_accu = TRUE)               # => default w = .5 (balanced accuracy "bacc")
#' plot_fnet(show_accu = TRUE, w_acc = 1/3)  # => (weighted accuracy "wacc")
#' plot_fnet(show_accu = FALSE)              # => no accuracy info.
#'
#' # Rounding:
#' plot_fnet(prev = .1, sens = .7, spec = .9, N = 10, by = "cddc", area = "sq",
#'           p_lbl = "num", round = TRUE)    # => mi = 0
#' plot_fnet(prev = .1, sens = .7, spec = .9, N = 10, by = "cddc", area = "sq",
#'           p_lbl = "num", round = FALSE)   # => mi = 0.3
#'
#' # Combining perspectives, areas, and label options:
#' plot_fnet(by = "cd", area = "sq", p_lbl = "nam")  # => by cond + sq + prob names
#' plot_fnet(by = "cd", area = "hr", p_lbl = "num")  # => by cond + hr + prob numbers
#' plot_fnet(by = "dc", area = "sq", p_lbl = "num")  # => by dec  + sq + mix names and numbers
#' plot_fnet(by = "dc", area = "vr", p_lbl = "mix")  # => by dec  + vr + min. labels
#'
#' # Custom colors and shadows:
#' plot_fnet(prev = .08, sens = .92, spec = .95, N = 10000, area = "hr")
#' plot_fnet(area = "sq", col_boxes = "gold", col_border = "steelblue4",
#'           col_shadow = "steelblue4", cex_shadow = .008)
#' plot_fnet(N = NA, area = "vr", col_txt = "steelblue4", col_boxes = "lightyellow",
#'           col_border = grey(.3, .7), cex_shadow = .008, col_shadow = grey(.1, .9))
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
#' @importFrom diagram plotmat
#' @importFrom graphics title
#' @importFrom graphics mtext
#'
#' @export

## plot_fnet: Definition: ---------

plot_fnet <- function(prev = num$prev,             # probabilities
                      sens = num$sens, mirt = NA,
                      spec = num$spec, fart = NA,  # was: num$fart,
                      N = freq$N,    # ONLY freq used (so far)
                      ## Options:
                      round = TRUE,  # Boolean: round freq (if computed), default: round = TRUE.
                      by = "cddc",   # 4 perspectives: "cd" by condition, "dc" by decision; "cddc" by condition and decision (default), "dccd" by decision and condition.
                      area = "sq",   # 4 area types: "no" none, "sq" square (default), "hr" horizontal rectangles, "vr" vertical rectangles
                      p_lbl = "num", # 4 probability (edge) label types: "nam" names, "num" numeric values (default), "mix" essential names + complement values, "min" minimal.
                      ## Compute and show accuracy info:
                      show_accu = TRUE,  # compute and show accuracy metrics
                      w_acc = .50,       # weight w for wacc (from 0 to 1)
                      ## Labels:
                      title_lbl = txt$scen_lbl,
                      popu_lbl = txt$popu_lbl,
                      ## Condition labels:
                      cond.true_lbl = txt$cond.true_lbl,
                      cond.false_lbl = txt$cond.false_lbl,
                      ## Decision labels:
                      dec.pos_lbl = txt$dec.pos_lbl,
                      dec.neg_lbl = txt$dec.neg_lbl,
                      ## SDT combinations:
                      hi_lbl = txt$hi_lbl,
                      mi_lbl = txt$mi_lbl,
                      fa_lbl = txt$fa_lbl,
                      cr_lbl = txt$cr_lbl,
                      ## Box settings:
                      col_txt = grey(.01, alpha = .99),  # black
                      cex_lbl = .85,                     # relative text size (of box and arrow labels)
                      box_cex,                           # deprecated predecessor of cex_lbl
                      col_boxes = pal, # pal[c(1:9)],    # box colors (9 frequencies/boxes/colors)
                      col_border = grey(.33, alpha = .99),  # mid grey
                      ## Widths of arrows and box borders:
                      lwd = 1.5,      # width of arrows
                      box_lwd = 1.5,  # set to 0.001 to show boxes without borders (but =0 yields ERROR)
                      ## Shadows:
                      col_shadow = grey(.11, alpha = .99),  # dark grey
                      cex_shadow = 0  # [values > 0 show shadows]
){

  ## (0) Handle deprecated arguments: ----------

  if (!missing(box_cex)) {
    warning("argument 'box_cex' is deprecated; please use 'cex_lbl' instead.",
            call. = FALSE)
    cex_lbl <- box_cex
  }

  ## Increase robustness by anticipating and correcting common entry errors: ------

  if ( !is.null(by) && !is.na(by) ) { by <- tolower(by) }  # express by in lowercase
  if (by == "any" || by == "all" || by == "default" || by == "def" || is.null(by) || is.na(by) )  { by <- "cddc" }  # default/null
  if (by == "cond" || by == "condition" ) { by <- "cd" }
  if (by == "dec"  || by == "decision"  ) { by <- "dc" }
  # if (by == "acc" || by == "accuracy" || by == "cor" )  { by <- "ac" }  # currently unsupported

  if ( !is.null(area) && !is.na(area) ) {
    area <- tolower(area)  # express area in lowercase
  }
  if (area == "none" || is.null(area) || is.na(area) ) { area <- "no" }          # null
  if (area == "square" || area == "def" || area == "default" ) { area <- "sq" }  # default
  if (area == "rect")   { area <- "vr" }

  if ( !is.null(p_lbl) && !is.na(p_lbl) ) {
    p_lbl <- tolower(p_lbl)  # express p_lbl in lowercase
  }
  if (p_lbl == "def" || p_lbl == "default" || is.null(p_lbl) || is.na(p_lbl) ) { p_lbl <- "mix" }  # default/null
  if (p_lbl == "namnum" || p_lbl == "namval") { p_lbl <- "mix" }
  if (p_lbl == "val") { p_lbl <- "num" }


  ## (0.1) Compute or collect current frequencies: ----------

  ## (A) If a valid set of probabilities was provided:
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = mirt, spec = spec, fart = fart, tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

    ## (b) Compute cur_freq based on current parameters (N and probabilities):
    cur_freq <- comp_freq(prev = prev, sens = sens, spec = spec, N = N, round = round)  # compute freq

    ## Assign elements of cur_freq:
    N <- cur_freq$N
    n_true  <- cur_freq$cond.true
    n_false <- cur_freq$cond.false
    n_pos <- cur_freq$dec.pos
    n_neg <- cur_freq$dec.neg
    n_hi  <- cur_freq$hi
    n_mi  <- cur_freq$mi
    n_fa  <- cur_freq$fa
    n_cr  <- cur_freq$cr

  } else { # (B) NO valid set of probabilities was provided:

    ## Use the current values of freq:
    N <- freq$N
    n_true  <- freq$cond.true
    n_false <- freq$cond.false
    n_pos <- freq$dec.pos
    n_neg <- freq$dec.neg
    n_hi  <- freq$hi
    n_mi  <- freq$mi
    n_fa  <- freq$fa
    n_cr  <- freq$cr

  } # if (is_valid_prob_set...)

  if (by != "cd") {  # in ANY case NOT solely by condition:

    # Compute current PVs from current frequencies:
    ppod <- n_pos/N
    PPV  <- n_hi/n_pos
    NPV  <- n_cr/n_neg

  } # if (by...)

  ## (0.2) Define plotting area: -----

  ## Record graphical parameters (par):
  opar <- par(no.readonly = TRUE)  # all par settings that can be changed.
  on.exit(par(opar))

  ## Define margin areas:
  n_lines_mar <- 3
  n_lines_oma <- 0
  par(mar = c(n_lines_mar, 1, 2, 1) + 0.1)  # margins; default: par("mar") = 5.1 4.1 4.1 2.1.
  par(oma = c(n_lines_oma, 0, 0, 0) + 0.1)  # outer margins; default: par("oma") = 0 0 0 0.

  ## (1) Color of boxes: ----------

  if ((length(col_boxes) == length(pal))    # length of col_boxes corresponds to pal
      # && isTRUE(all.equal(col_boxes, pal))  # values of col_boxes correspond to pal
      && isTRUE(all.equal(names(col_boxes), names(pal)))  # names of col_boxes correspond to pal
  ) {  # use named colors of col_boxes:

    ## Get current color vector from pal:

    if (by == "cd") {  # (a) by condition:

      ## 7 boxes (including cond.true and cond.false):
      # col_boxes <- col_boxes[c(1:3, 6:9)]  # select 7 of 9 colors
      col_boxes <- c(col_boxes["N"], col_boxes["true"], col_boxes["false"],
                     col_boxes["hi"], col_boxes["mi"], col_boxes["fa"], col_boxes["cr"])

    } else if (by == "dc") {  # (b) by decision:

      ## 7 boxes (including dec.pos and dec.neg):
      # col_boxes <- col_boxes[c(1, 4:9)  ]  # select 7 of 9 colors
      col_boxes <- c(col_boxes["N"], col_boxes["pos"], col_boxes["neg"],
                     col_boxes["hi"], col_boxes["mi"], col_boxes["fa"], col_boxes["cr"])

    } else if (by == "cddc") {  # (c) by condition + decision:

      ## 10 boxes (top: cond.true and cond.false; bot: dec.pos and dec.neg):
      # col_boxes <- col_boxes[c(1:3, 6:9, 4:5, 1)  ]  # select 9 of 9 colors
      col_boxes <- c(col_boxes["N"],
                     col_boxes["true"], col_boxes["false"],
                     col_boxes["hi"], col_boxes["mi"], col_boxes["fa"], col_boxes["cr"],
                     col_boxes["pos"], col_boxes["neg"],
                     col_boxes["N"])

    } else if (by == "dccd") {  # (d) 1st by decision, then by condition:

      ## 10 boxes (top: dec.pos and dec.neg; bot: cond.true and cond.false):
      # col_boxes <- col_boxes[c(1, 4:9, 2:3, 1)  ]  # select 9 of 9 colors
      col_boxes <- c(col_boxes["N"],
                     col_boxes["pos"], col_boxes["neg"],
                     col_boxes["hi"], col_boxes["mi"], col_boxes["fa"], col_boxes["cr"],
                     col_boxes["true"], col_boxes["false"],
                     col_boxes["N"])

    } else {  # ANY other by-setting:

      col_boxes <- pal["N"]  # to prevent errors for other entries

    } # if (by...)

  } # if (length(col_boxes) == length(pal)) #
  #     # && all.equal(col_boxes, pal))...


  ## (2) Text/labels in 7 or 10 boxes: ----------

  if (by == "cd") {  # (a) by condition:

    if (area == "no") {  # default box labels:

      ## Full box labels (label: value):
      names <- c(paste0("Population", ":\n", "N = ", N),  # popu_lbl (NOT used yet)
                 paste0(cond.true_lbl, ":\n",  round(n_true, 2)),
                 paste0(cond.false_lbl, ":\n", round(n_false, 2)),
                 paste0(hi_lbl, ":\n", round(n_hi, 2)),
                 paste0(mi_lbl, ":\n", round(n_mi, 2)),
                 paste0(fa_lbl, ":\n", round(n_fa, 2)),
                 paste0(cr_lbl, ":\n", round(n_cr, 2))
      )

    } else {  # shorter box labels:

      ## Reduced labels (as areas get quite small):
      names <- c(paste0("N = ", N),  # popu_lbl
                 paste0("true:\n",  round(n_true, 2)),
                 paste0("false:\n", round(n_false, 2)),
                 paste0("hi:\n", round(n_hi, 2)),
                 paste0("mi:\n", round(n_mi, 2)),
                 paste0("fa:\n", round(n_fa, 2)),
                 paste0("cr:\n", round(n_cr, 2))
      )

    }  # if (area...)

  } else if (by == "dc") {  # (b) by decision:

    if (area == "no") {  # default box labels:

      ## Full box labels (label: value):
      names <- c(paste0("Population", ":\n", "N = ", N),  # popu_lbl (NOT used yet)
                 paste0(dec.pos_lbl, ":\n",  round(n_pos, 2)),
                 paste0(dec.neg_lbl, ":\n", round(n_neg, 2)),
                 paste0(hi_lbl, ":\n", round(n_hi, 2)),
                 paste0(mi_lbl, ":\n", round(n_mi, 2)),
                 paste0(fa_lbl, ":\n", round(n_fa, 2)),
                 paste0(cr_lbl, ":\n", round(n_cr, 2))
      )

    } else {  # shorter box labels:

      ## Reduced labels (as areas get quite small):
      names <- c(paste0("N = ", N),  # popu_lbl
                 paste0("positive:\n",  round(n_pos, 2)),
                 paste0("negative:\n", round(n_neg, 2)),
                 paste0("hi:\n", round(n_hi, 2)),
                 paste0("mi:\n", round(n_mi, 2)),
                 paste0("fa:\n", round(n_fa, 2)),
                 paste0("cr:\n", round(n_cr, 2))
      )

    }  # if (area...)

  } else if (by == "cddc") {  # (c) by condition + decision:

    if (area == "no") {  # default box labels:

      ## Full box labels (label: value):
      names <- c(paste0("Population", ":\n", "N = ", N),  # popu_lbl (NOT used yet)
                 paste0(cond.true_lbl, ":\n",  round(n_true, 2)),
                 paste0(cond.false_lbl, ":\n", round(n_false, 2)),
                 paste0(hi_lbl, ":\n", round(n_hi, 2)),
                 paste0(mi_lbl, ":\n", round(n_mi, 2)),
                 paste0(fa_lbl, ":\n", round(n_fa, 2)),
                 paste0(cr_lbl, ":\n", round(n_cr, 2)),
                 paste0(dec.pos_lbl, ":\n", round(n_pos, 2)),
                 paste0(dec.neg_lbl, ":\n", round(n_neg, 2)),
                 paste0("Population", ":\n", "N = ", N)  # popu_lbl
      )

      # ## ToDo: Names with more explicit labels:
      # names <- c(paste0("Population", ":\n", popu_lbl, ":\n", "N = ", N),                # using argument popu_lbl
      #            paste0(txt$cond_lbl, ":\n", cond.true_lbl,  ":\n", round(n_true, 1)),  # using global setting of txt
      #            paste0(txt$cond_lbl, ":\n", cond.false_lbl, ":\n", round(n_false, 1)), # using global setting of txt
      #            paste0("Hits", ":\n", hi_lbl, ":\n", round(n_hi, 1)),
      #            paste0("Misses", ":\n", mi_lbl, ":\n", round(n_mi, 1)),
      #            paste0("False alarms", ":\n", fa_lbl, ":\n", round(n_fa, 1)),
      #            paste0("Correct rejections", ":\n", cr_lbl, ":\n", round(n_cr, 1)),
      #            paste0(txt$dec_lbl, ":\n", dec.pos_lbl, ":\n", round(n_pos, 2)), # using global setting of txt
      #            paste0(txt$dec_lbl, ":\n", dec.neg_lbl, ":\n", round(n_neg, 2)), # using global setting of txt
      #            paste0("Population", ":\n", popu_lbl, ":\n", "N = ", N)           # using argument popu_lbl
      # )

    } else {  # shorter box labels:

      ## Reduced labels (as areas get quite small):
      names <- c(paste0("N = ", N),  # popu_lbl
                 paste0("true:\n",  round(n_true, 2)),
                 paste0("false:\n", round(n_false, 2)),
                 paste0("hi:\n", round(n_hi, 2)),
                 paste0("mi:\n", round(n_mi, 2)),
                 paste0("fa:\n", round(n_fa, 2)),
                 paste0("cr:\n", round(n_cr, 2)),
                 paste0("positive:\n",  round(n_pos, 2)),
                 paste0("negative:\n", round(n_neg, 2)),
                 paste0("N = ", N)   # popu_lbl
      )

    }  # if (area...)

    # } else if (by == "dccd") {  # (d) 1st by decision, then by condition:

  } else {  # ANY other by-case:

    if (area == "no") {  # default box labels:

      ## Full box labels (label: value):
      names <- c(paste0("Population", ":\n", "N = ", N),  # popu_lbl (NOT used yet)
                 paste0(dec.pos_lbl, ":\n", round(n_pos, 2)),
                 paste0(dec.neg_lbl, ":\n", round(n_neg, 2)),
                 paste0(hi_lbl, ":\n", round(n_hi, 2)),
                 paste0(mi_lbl, ":\n", round(n_mi, 2)),
                 paste0(fa_lbl, ":\n", round(n_fa, 2)),
                 paste0(cr_lbl, ":\n", round(n_cr, 2)),
                 paste0(cond.true_lbl, ":\n",  round(n_true, 2)),
                 paste0(cond.false_lbl, ":\n", round(n_false, 2)),
                 paste0("Population", ":\n", "N = ", N)  # popu_lbl
      )

      # ## ToDo: Names with more explicit labels:
      # names <- c(paste0("Population", ":\n", popu_lbl, ":\n", "N = ", N),         # using argument popu_lbl
      #            paste0(txt$dec_lbl, ":\n", dec.pos_lbl, ":\n", round(n_pos, 2)), # using global setting of txt
      #            paste0(txt$dec_lbl, ":\n", dec.neg_lbl, ":\n", round(n_neg, 2)), # using global setting of txt
      #            paste0("Hits", ":\n", hi_lbl, ":\n", round(n_hi, 1)),
      #            paste0("Misses", ":\n", mi_lbl, ":\n", round(n_mi, 1)),
      #            paste0("False alarms", ":\n", fa_lbl, ":\n", round(n_fa, 1)),
      #            paste0("Correct rejections", ":\n", cr_lbl, ":\n", round(n_cr, 1)),
      #            paste0(txt$cond_lbl, ":\n", cond.true_lbl,  ":\n", round(n_true, 1)),  # using global setting of txt
      #            paste0(txt$cond_lbl, ":\n", cond.false_lbl, ":\n", round(n_false, 1)), # using global setting of txt
      #            paste0("Population", ":\n", popu_lbl, ":\n", "N = ", N)                # using argument popu_lbl
      # )

    } else {  # shorter box labels:

      ## Reduced labels (as areas get quite small):
      names <- c(paste0("N = ", N),  # popu_lbl
                 paste0("positive:\n",  round(n_pos, 2)),
                 paste0("negative:\n", round(n_neg, 2)),
                 paste0("hi:\n", round(n_hi, 2)),
                 paste0("mi:\n", round(n_mi, 2)),
                 paste0("fa:\n", round(n_fa, 2)),
                 paste0("cr:\n", round(n_cr, 2)),
                 paste0("true:\n",  round(n_true, 2)),
                 paste0("false:\n", round(n_false, 2)),
                 paste0("N = ", N)   # popu_lbl
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
  # prev_lbl <- paste0("prev = ", as_pc(prev), "%")
  # prev_lbl <- paste0("prev = ", prev) # ERROR: WHY does prev_lbl not work with spaces???

  if (cex_lbl > .50) { cex_arr <- (cex_lbl - .05) } else { cex_arr <- cex_lbl } # cex_arr <= cex_lbl

  if (by == "cd") {  # by condition:

    # 4 types of edge labels: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
    if (p_lbl == "nam") {  # default labels: names of essential probabilities + their complements:

      M[2, 1] <- "prev"         # "prevalence"
      M[3, 1] <- "(1 - prev)"   # "(N - n_true)"
      M[4, 2] <- "sens"         # "sensitivity"
      M[5, 2] <- "mirt"         # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 3] <- "fart"         # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 3] <- "spec"         # "specificity"

    } else if (p_lbl == "num") {  # numeric values (rounded to 3 decimals):

      M[2, 1] <- round(prev, 3)
      M[3, 1] <- round((1 - prev), 3)  # (1 - prev) "n(N - true)"
      M[4, 2] <- round(sens, 3)
      M[5, 2] <- round((1 - sens), 3)  # mirt = (1 - sens) = "n(true - hi)"
      M[6, 3] <- round((1 - spec), 3)  # fart = (1 - spec) = "n(false - cr)"
      M[7, 3] <- round(spec, 3)

    } else if (p_lbl == "mix") {  # mixed labels: essential names + numeric complements:

      M[2, 1] <- "prev"                # "prevalence"
      M[3, 1] <- round((1 - prev), 3)  # (1 - prev) "n(N - true)"
      M[4, 2] <- "sens"                # "sensitivity"
      M[5, 2] <- round((1 - sens), 3)  # mirt = (1 - sens) = "n(true - hi)"
      M[6, 3] <- round((1 - spec), 3)  # fart = (1 - spec) = "n(false - cr)"
      M[7, 3] <- "spec"                # "specificity"

    } else {  # "min" minimal labels:

      M[2, 1] <- "prev"         # "prevalence"
      M[3, 1] <- ""             # "(N - n_true)"
      M[4, 2] <- "sens"         # "sensitivity"
      M[5, 2] <- ""             # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 3] <- ""             # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 3] <- "spec"         # "specificity"

    } # (p_lbl ==...

  } else if (by == "dc") {  # by decision:

    # 4 types of edge labels: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.

    if (p_lbl == "nam") {  # default labels: names of essential probabilities + their complements:

      M[2, 1] <- "ppod"         # ppod  ERROR: WHY does prev_lbl not work with spaces???
      M[3, 1] <-  "(1 - ppod)"  # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"          # PPV
      M[6, 2] <- "FDR"          # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- "FOR"          # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"          # NPV

    } else if (p_lbl == "num") {  # numeric values (rounded to 3 decimals):

      M[2, 1] <- round(ppod, 3)        # ppod
      M[3, 1] <- round((1 - ppod), 3)  # "n(N - dec.pos)"
      M[4, 2] <- round(PPV, 3)         # PPV
      M[6, 2] <- round((1 - PPV), 3)   # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- round((1 - NPV), 3)   # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- round(NPV, 3)         # NPV

    } else if (p_lbl == "mix") {  # mixed labels: essential names + numeric complements:

      M[2, 1] <- "ppod"                # ppod  ERROR: WHY does prev_lbl not work with spaces???
      M[3, 1] <- round((1 - ppod), 3)  # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"                 # PPV
      M[6, 2] <- round((1 - PPV), 3)   # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- round((1 - NPV), 3)   # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"                 # NPV

    } else {  # "min" minimal labels:

      M[2, 1] <- "ppod"         # ppod   ERROR: WHY does prev_lbl not work with spaces???
      M[3, 1] <-  ""            # (1 - ppod) = "n(N - dec.pos)"
      M[4, 2] <- "PPV"          # PPV
      M[6, 2] <- ""             # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 3] <- ""             # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 3] <- "NPV"          # NPV

    } # (p_lbl ==...

  } else if (by == "cddc") {  # (c) by condition + decision:

    # 4 types of edge labels: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
    if (p_lbl == "nam") {  # default labels: names of essential probabilities + their complements:

      M[2, 1] <- "prev"         # "prevalence"
      M[3, 1] <- "(1 - prev)"   # "(N - n_true)"
      M[4, 2] <- "sens"         # "sensitivity"
      M[5, 2] <- "mirt"         # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 3] <- "fart"         # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 3] <- "spec"         # "specificity"

      M[4, 8]  <- "PPV"         # PPV
      M[6, 8]  <- "FDR"         # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 9]  <- "FOR"         # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 9]  <- "NPV"         # NPV
      M[8, 10] <- "ppod"        # ppod  ERROR: WHY does prev_lbl not work with spaces???
      M[9, 10] <- "(1 - ppod)"  # (1 - ppod) = "n(N - dec.pos)"

    } else if (p_lbl == "num") {  # numeric values (rounded to 3 decimals):

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

    } else if (p_lbl == "mix") {  # mixed labels: essential names + numeric complements:

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
      M[8, 10] <- "ppod"                # ppod  ERROR: WHY does prev_lbl not work with spaces???
      M[9, 10] <- round((1 - ppod), 3)  # (1 - ppod) = "n(N - dec.pos)"

    } else {  # "min" minimal labels:

      M[2, 1] <- "prev"         # "prevalence"
      M[3, 1] <- ""             # "(N - n_true)"
      M[4, 2] <- "sens"         # "sensitivity"
      M[5, 2] <- ""             # "(miss_rate)" = "n(true - hi)" =  mirt = miss rate
      M[6, 3] <- ""             # "(false_alarm_rate)" = "n(false - cr)" = fart = false alarm rate
      M[7, 3] <- "spec"         # "specificity"

      M[4, 8]  <- "PPV"         # PPV
      M[6, 8]  <- ""            # FDR = (1 - PPV) = "n(pos - hi)"
      M[5, 9]  <- ""            # FOR = (1 - NPV) = "n(neg - cr)"
      M[7, 9]  <- "NPV"         # NPV
      M[8, 10] <- "ppod"        # ppod   ERROR: WHY does prev_lbl not work with spaces???
      M[9, 10] <-  ""           # (1 - ppod) = "n(N - dec.pos)"

    } # (p_lbl ==...

    # } else if (by == "dccd") {  # (d) 1st by decision, then by condition:

  } else { # ANY other by-setting:

    # 4 types of edge labels: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
    if (p_lbl == "nam") {  # default labels: names of essential probabilities + their complements:

      M[2, 1] <- "ppod"        # ppod  ERROR: WHY does prev_lbl not work with spaces???
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
      M[9, 10] <- "(1 - prev)"   # "(N - n_true)"

    } else if (p_lbl == "num") {  # numeric values (rounded to 3 decimals):

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

    } else if (p_lbl == "mix") {  # mixed labels: essential names + numeric complements:

      M[2, 1] <- "ppod"                # ppod  ERROR: WHY does prev_lbl not work with spaces???
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

      M[2, 1] <- "ppod"        # ppod   ERROR: WHY does prev_lbl not work with spaces???
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
      M[9, 10] <- ""             # "(N - n_true)"

    } # (p_lbl ==...

  } # if (by...)


  ## (5) Distinguish 4 plot types (based on area setting): ----------

  ## 5a. Default case: Rectangles of same width and height (non-proportional)
  if (area == "no") {

    if (by == "cd" || by == "dc") {  # (a) by condition OR (b) by decision:

      ## Box size and proportions:
      x_pop <- .11    # basic width of population box
      x_y_pop <- 2/3  # basic proportion is rectangular (width > height)

      ## Collect all sizes and proportions:
      x_boxes <- rep(x_pop, 7)     # all boxes have the same width
      x_y_prop <- rep(x_y_pop, 7)  # all boxes have the same proportion

    } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

      x_pop <- .10    # basic width of population box
      x_y_pop <- 5/9  # basic proportion is rectangular (width > height)

      ## Collect all sizes and proportions:
      x_boxes <- rep(x_pop, 10)     # all boxes have the same width
      x_y_prop <- rep(x_y_pop, 10)  # all boxes have the same proportion

    } # if (by...)

  } # if (area == "no")...


  ## 5b. Squares that sum to the area of the next higher level:

  else if (area == "sq") {

    ## Level 1: Population square

    x_pop <- .10    # basic width of population box: Area N = x_pop^2
    x_y_pop <- 1/1  # square

    ## Determine all other box sizes by their relative proportions in freq:

    if (by == "cd") {  # (a) by condition:

      ## Level 2: cond is true vs. false

      x_true  <- sqrt(n_true/N  * x_pop^2)  # cond.true cases
      x_false <- sqrt(n_false/N * x_pop^2)  # cond.false cases

      if (!all.equal(x_pop^2, sum(c(x_true^2, x_false^2), na.rm = TRUE))) {
        warning("sumtree 1a: Sum of cond.true and cond.false area differs from population area.")
      }

    } else if (by == "dc") {  # (b) by decision:

      ## Level 2: dec is pos vs. neg

      x_pos  <- sqrt(n_pos/N * x_pop^2)  # dec.pos cases
      x_neg <- sqrt(n_neg/N * x_pop^2)   # dec.neg cases

      if (!all.equal(x_pop^2, sum(c(x_pos^2, x_neg^2), na.rm = TRUE))) {
        warning("sumtree 1b: Sum of dec.pos and dec.neg area differs from population area.")
      }

    } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

      ## Level 1: Population square
      x_pop <- .08    # shrink basic width of population box: Area N = x_pop^2
      x_y_pop <- 1/1  # square

      ## Level 2 (top): cond is true vs. false
      x_true  <- sqrt(n_true/N  * x_pop^2)  # cond.true cases
      x_false <- sqrt(n_false/N * x_pop^2)  # cond.false cases

      if (!all.equal(x_pop^2, sum(c(x_true^2, x_false^2), na.rm = TRUE))) {
        warning("sumtree 1c+d: Sum of cond.true and cond.false area differs from population area.")
      }

      ## Level 2 (bot): dec is pos vs. neg
      x_pos  <- sqrt(n_pos/N * x_pop^2)  # dec.pos cases
      x_neg <- sqrt(n_neg/N * x_pop^2)   # dec.neg cases

      if (!all.equal(x_pop^2, sum(c(x_pos^2, x_neg^2), na.rm = TRUE))) {
        warning("sumtree 1c+d: Sum of dec.pos and dec.neg area differs from population area.")
      }

    } # (if by...)

    ## Level 3: 4 SDT squares (irrespective of current by option!)
    x_hi <- sqrt(n_hi/N * x_pop^2)
    x_mi <- sqrt(n_mi/N * x_pop^2)
    x_fa <- sqrt(n_fa/N * x_pop^2)
    x_cr <- sqrt(n_cr/N * x_pop^2)

    ## Checks by condition/decision:
    if (by == "cd") {  # (a) by condition:

      if (!all.equal(x_true^2, sum(c(x_hi^2, x_mi^2), na.rm = TRUE))) {
        warning("sumtree 2a: Sum of HI and MI area differs from cond.TRUE area.")
      }

      if (!all.equal(x_false^2, sum(c(x_fa^2, x_cr^2), na.rm = TRUE))) {
        warning("sumtree 3a: Sum of FA and CR area differs from cond.FALSE area.")
      }

    } else if (by == "dc") {  # (b) by decision:

      if (!all.equal(x_pos^2, sum(c(x_hi^2, x_fa^2), na.rm = TRUE))) {
        warning("sumtree 2b: Sum of HI and FA area differs from dec.POS area.")
      }

      if (!all.equal(x_neg^2, sum(c(x_mi^2, x_cr^2), na.rm = TRUE))) {
        warning("sumtree 3b: Sum of MI and CR area differs from dec.NEG area.")
      }

    } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

      if (!all.equal(x_true^2, sum(c(x_hi^2, x_mi^2), na.rm = TRUE))) {
        warning("sumtree 2c: Sum of HI and MI area differs from cond.TRUE area.")
      }

      if (!all.equal(x_false^2, sum(c(x_fa^2, x_cr^2), na.rm = TRUE))) {
        warning("sumtree 2c: Sum of FA and CR area differs from cond.FALSE area.")
      }

      if (!all.equal(x_pos^2, sum(c(x_hi^2, x_fa^2), na.rm = TRUE))) {
        warning("sumtree 2d: Sum of HI and FA area differs from dec.POS area.")
      }

      if (!all.equal(x_neg^2, sum(c(x_mi^2, x_cr^2), na.rm = TRUE))) {
        warning("sumtree 3d: Sum of MI and CR area differs from dec.NEG area.")
      }

    } # (if by...)

    ## Check (irrespective of current by option):
    if (!all.equal(x_pop^2, sum(c(x_hi^2, x_mi^2, x_fa^2, x_cr^2), na.rm = TRUE))) {
      warning("sumtree 4: Population area differs from the area sum of all 4 SDT cases.")
    }

    ## Collect box sizes and proportions:

    if (by == "cd") {  # (a) by condition:

      ## Widths and proportions of 7 boxes:
      x_boxes <- c(x_pop,
                   x_true, x_false,
                   x_hi, x_mi, x_fa, x_cr)
      x_y_prop <- rep(x_y_pop, 7) # all 7 boxes have the same proportion (squares)

    } else if (by == "dc") {  # (b) by decision:

      ## Widths and proportions of 7 boxes:
      x_boxes <- c(x_pop,
                   x_pos, x_neg,
                   x_hi, x_mi, x_fa, x_cr)
      x_y_prop <- rep(x_y_pop, 7) # all 7 boxes have the same proportion (squares)

    } else if (by == "cddc") {  # (c) by condition and by decision:

      ## Widths and proportions of 10 boxes:
      x_boxes <- c(x_pop,
                   x_true, x_false,
                   x_hi, x_mi, x_fa, x_cr,
                   x_pos, x_neg,
                   x_pop)
      x_y_prop <- rep(x_y_pop, 10) # all 10 boxes have the same proportion (squares)

    } else if (by == "dccd") {  # (d) 1st by decision, 2nd by condition:

      ## Widths and proportions of 10 boxes:
      x_boxes <- c(x_pop,
                   x_pos, x_neg,
                   x_hi, x_mi, x_fa, x_cr,
                   x_true, x_false,
                   x_pop)
      x_y_prop <- rep(x_y_pop, 10) # all 10 boxes have the same proportion (squares)

    } # (if by...)

  } # (area == "sq")


  ## 5c. Rectangles that sum to the area of the next higher level:

  else if (area == "hr") {

    ## Level 1: Population square

    x_pop <- .10   # basic width x of population box: Area N = x_pop^2
    x_y_pop <- 1/1 # square

    ## Determine all other box sizes by their relative proportions in freq:

    if (by == "cd") {  # (a) by condition:

      ## Level 2: 2 vertical rectangles

      x_true <- (n_true/N) * x_pop  # scale x_pop by proportion of cond.true cases
      x_y_true <- x_pop/x_true

      x_false <- n_false/N * x_pop  # scale x_pop by proportion of cond.false cases
      x_y_false <- x_pop/x_false

      if (!all.equal(x_pop^2, sum(c((x_true * x_pop), (x_false * x_pop)), na.rm = TRUE))) {
        warning("hrectree 1a: Sum of cond.true + cond.false areas differs from population area.")
      }

      ## Level 3:
      ## 4 rectangles with heights matching the widths of the Level 2 rectangles
      ## (and their widths summing up to the height of the Level 2 rectangles)

      ## Box cond.true = hi + mi:
      x_hi <- (n_hi/n_true) * x_pop  # hi: scale x_pop by sens = n_hi/n_true
      x_y_hi <- x_true/x_hi

      x_mi <-  (1 - (n_hi/n_true)) * x_pop  # mi: scale x_pop by mirt = (1 - sens)
      x_y_mi <- x_true/x_mi

      if (!all.equal((x_true * x_pop),
                     sum(c((x_hi * x_true), (x_mi * x_true)), na.rm = TRUE))) {
        warning("hrectree 2a: Sum of HI + MI area differs from cond.TRUE area.")
      }

      ## Box cond.false = fa + cr:
      x_cr <- (n_cr/n_false) * x_pop  # cr: scale x_pop by spec = n_cr/n_false
      x_y_cr <- x_false/x_cr

      x_fa <- (1 - (n_cr/n_false)) * x_pop  # fa: scale x_pop by (1 - spec)
      x_y_fa <- x_false/x_fa

      if (!all.equal((x_false * x_pop),
                     sum(c((x_fa * x_false), (x_cr * x_false)), na.rm = TRUE))) {
        warning("hrectree 3a: Sum of FA + CR area differs from cond.FALSE area.")
      }

      if (!all.equal((x_pop^2),
                     sum(c((x_hi * x_true), (x_mi * x_true), (x_fa * x_false), (x_cr * x_false)), na.rm = TRUE))) {
        warning("hrectree 4a: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 7 boxes:
      x_boxes <-  c(x_pop,  x_true, x_false,  x_hi, x_mi, x_fa, x_cr)  # specific widths
      x_y_prop <- c(x_y_pop,  x_y_true, x_y_false,  x_y_hi, x_y_mi, x_y_fa, x_y_cr)  # specific proportions


    } else if (by == "dc") {  # (b) by decision:


      ## Level 2: 2 vertical rectangles

      x_pos <- (n_pos/N) * x_pop  # scale x_pop by proportion of dec.pos cases
      x_y_pos <- x_pop/x_pos

      x_neg <- n_neg/N * x_pop  # scale x_pop by proportion of dec.neg cases
      x_y_neg <- x_pop/x_neg

      if (!all.equal(x_pop^2, sum(c((x_pos * x_pop), (x_neg * x_pop)), na.rm = TRUE))) {
        warning("hrectree 1b: Sum of dec.pos + dec.neg areas differs from population area.")
      }

      ## Level 3:
      ## 4 rectangles with heights matching the widths of the Level 2 rectangles
      ## (and their widths summing up to the height of the Level 2 rectangles)

      ## Box dec.pos = hi + FA:
      x_hi <- (n_hi/n_pos) * x_pop  # hi: scale x_pop by PPV = n_hi/dec.pos
      x_y_hi <- x_pos/x_hi

      # FA (now belongs to dec.pos):
      x_fa <-  (1 - (n_hi/n_pos)) * x_pop  # FA: scale x_pop by (1 - PPV)
      x_y_fa <- x_pos/x_fa

      if (!all.equal((x_pos * x_pop),
                     sum(c((x_hi * x_pos), (x_fa * x_pos)), na.rm = TRUE))) {
        warning("hrectree 2b: Sum of HI + FA area differs from dec.POS area.")
      }

      ## Box dec.neg = cr + MI:
      x_cr <- (n_cr/n_neg) * x_pop  # cr: scale x_pop by NPV = n_cr/dec.false
      x_y_cr <- x_neg/x_cr

      # MI (now belongs to dec.neg):
      x_mi <- (1 - (n_cr/n_neg)) * x_pop  # MI: scale x_pop by (1 - NPV)
      x_y_mi <- x_neg/x_mi

      if (!all.equal((x_neg * x_pop),
                     sum(c((x_mi * x_neg), (x_cr * x_neg)), na.rm = TRUE))) {
        warning("hrectree 3b: Sum of MI + CR area differs from dec.NEG area.")
      }

      if (!all.equal((x_pop^2),
                     sum(c((x_hi * x_pos),
                           (x_fa * x_pos),
                           (x_mi * x_neg),
                           (x_cr * x_neg)), na.rm = TRUE))) {
        warning("hrectree 4b: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 7 boxes:
      x_boxes <-  c(x_pop,
                    x_pos, x_neg,
                    x_hi, x_mi, x_fa, x_cr)  # specific widths
      x_y_prop <- c(x_y_pop,
                    x_y_pos, x_y_neg,
                    x_y_hi, x_y_mi, x_y_fa, x_y_cr)  # specific proportions


    } else if (by == "cddc") {  # (c) by condition and by decision:


      ## Level 1: Population square
      x_pop <- .08    # shrink basic width of population box: Area N = x_pop^2
      x_y_pop <- 1/1  # square

      ## Level 2 (top): 2 vertical rectangles by condition:
      x_true <- (n_true/N) * x_pop  # scale x_pop by proportion of cond.true cases
      x_y_true <- x_pop/x_true

      x_false <- n_false/N * x_pop  # scale x_pop by proportion of cond.false cases
      x_y_false <- x_pop/x_false

      if (!all.equal(x_pop^2, sum(c((x_true * x_pop), (x_false * x_pop)), na.rm = TRUE))) {
        warning("hrectree 1cddc: Sum of cond.true + cond.false areas differs from population area.")
      }

      ## Level 2 (bot): 2 vertical rectangles by decision:
      x_pos <- (n_pos/N) * x_pop  # scale x_pop by proportion of dec.pos cases
      x_y_pos <- x_pop/x_pos

      x_neg <- n_neg/N * x_pop  # scale x_pop by proportion of dec.neg cases
      x_y_neg <- x_pop/x_neg

      if (!all.equal(x_pop^2, sum(c((x_pos * x_pop), (x_neg * x_pop)), na.rm = TRUE))) {
        warning("hrectree 1c: Sum of dec.pos + dec.neg areas differs from population area.")
      }


      ## Level 3:  (Note: Simply copied from (by == "cd") above!)
      ## 4 rectangles with heights matching the widths of the Level 2 (top) rectangles
      ## (and their widths summing up to the height of the Level 2 rectangles of (top) by condition)

      ## Box cond.true = hi + mi:
      x_hi <- (n_hi/n_true) * x_pop  # hi: scale x_pop by sens = n_hi/n_true
      x_y_hi <- x_true/x_hi

      x_mi <-  (1 - (n_hi/n_true)) * x_pop  # mi: scale x_pop by mirt = (1 - sens)
      x_y_mi <- x_true/x_mi

      if (!all.equal((x_true * x_pop),
                     sum(c((x_hi * x_true), (x_mi * x_true)), na.rm = TRUE))) {
        warning("hrectree 2c: Sum of HI + MI area differs from cond.TRUE area.")
      }

      ## Box cond.false = fa + cr:
      x_cr <- (n_cr/n_false) * x_pop  # cr: scale x_pop by spec = n_cr/n_false
      x_y_cr <- x_false/x_cr

      x_fa <- (1 - (n_cr/n_false)) * x_pop  # fa: scale x_pop by (1 - spec)
      x_y_fa <- x_false/x_fa

      if (!all.equal((x_false * x_pop),
                     sum(c((x_fa * x_false), (x_cr * x_false)), na.rm = TRUE))) {
        warning("hrectree 3c: Sum of FA + CR area differs from cond.FALSE area.")
      }

      if (!all.equal((x_pop^2),
                     sum(c((x_hi * x_true), (x_mi * x_true), (x_fa * x_false), (x_cr * x_false)), na.rm = TRUE))) {
        warning("hrectree 4c: Population area differs from the area sum of all 4 SDT cases.")
      }


      ## Collect widths and proportions of 10 boxes:
      x_boxes <-  c(x_pop,  x_true, x_false,
                    x_hi, x_mi, x_fa, x_cr,
                    x_pos, x_neg,  x_pop)  # specific widths
      x_y_prop <- c(x_y_pop,  x_y_true, x_y_false,
                    x_y_hi, x_y_mi, x_y_fa, x_y_cr,
                    x_y_pos, x_y_neg,  x_y_pop)  # specific proportions


    } else if (by == "dccd") {  # (d) 1st by decision, 2nd by condition:

      ## Level 1: Population square
      x_pop <- .08    # shrink basic width of population box: Area N = x_pop^2
      x_y_pop <- 1/1  # square


      ## Level 2 (top): 2 vertical rectangles by decision:
      x_pos <- (n_pos/N) * x_pop  # scale x_pop by proportion of dec.pos cases
      x_y_pos <- x_pop/x_pos

      x_neg <- (n_neg/N) * x_pop  # scale x_pop by proportion of dec.neg cases
      x_y_neg <- x_pop/x_neg

      if (!all.equal(x_pop^2, sum(c((x_pos * x_pop), (x_neg * x_pop)), na.rm = TRUE))) {
        warning("hrectree 1d: Sum of dec.pos + dec.neg areas differs from population area.")
      }


      ## Level 2 (bot): 2 vertical rectangles by condition:
      x_true <- (n_true/N) * x_pop  # scale x_pop by proportion of cond.true cases
      x_y_true <- x_pop/x_true

      x_false <- (n_false/N) * x_pop  # scale x_pop by proportion of cond.false cases
      x_y_false <- x_pop/x_false

      if (!all.equal(x_pop^2, sum(c((x_true * x_pop), (x_false * x_pop)), na.rm = TRUE))) {
        warning("hrectree 1d: Sum of cond.true + cond.false areas differs from population area.")
      }


      ## Level 3:
      ## 4 rectangles with heights matching the widths of the Level 2 (top) rectangles
      ## (and their widths summing up to the height of the Level 2 rectangles of (top) by condition)

      ## Box dec.POS = hi + FA:
      x_hi <- (n_hi/n_pos) * x_pop  # hi: scale x_pop by PPV = n_hi/n_pos
      x_y_hi <- x_pos/x_hi

      x_fa <- (1 - (n_hi/n_pos)) * x_pop  # FA: scale x_pop by (1 - PPV) = FDR
      x_y_fa <- x_pos/x_fa

      if (!all.equal((x_pos * x_pop),
                     sum(c((x_hi * x_pos), (x_fa * x_pos)), na.rm = TRUE))) {
        warning("hrectree 2d: Sum of HI + FA area differs from dec.POS area.")
      }

      ## Box dec.NEG = cr + MI:
      x_cr <- (n_cr/n_neg) * x_pop  # cr: scale x_pop by NPV = n_cr/n_neg
      x_y_cr <- x_neg/x_cr

      x_mi <-  (1 - (n_cr/n_neg)) * x_pop  # MI: scale x_pop by (1 - NPV) = FOR
      x_y_mi <- x_neg/x_mi

      if (!all.equal((x_neg * x_pop),
                     sum(c((x_mi * x_neg), (x_cr * x_neg)), na.rm = TRUE))) {
        warning("hrectree 3d: Sum of MI + CR area differs from dec.NEG area.")
      }

      if (!all.equal((x_pop^2),
                     sum(c((x_hi * x_pos), (x_mi * x_neg), (x_fa * x_pos), (x_cr * x_neg)), na.rm = TRUE))) {
        warning("hrectree 4d: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 10 boxes:
      x_boxes <-  c(x_pop,
                    x_pos, x_neg,
                    x_hi, x_mi, x_fa, x_cr,
                    x_true, x_false,
                    x_pop)        # specific widths
      x_y_prop <- c(x_y_pop,
                    x_y_pos, x_y_neg,
                    x_y_hi, x_y_mi, x_y_fa, x_y_cr,
                    x_y_true, x_y_false,
                    x_y_pop)  # specific proportions

    } # (if by...)

  } # if (area == "hr")...


  ## 5d. Rectangles that sum to the area of the next higher level
  ##     (= 3. flipped by 90 degrees on Level 3 to correspond to 4 SDT areas of mosaic plot):
  else if (area == "vr") {

    ## Level 1: Population square
    x_pop <- .10   # basic width x of population box: Area N = x_pop^2
    x_y_pop <- 1/1 # square

    ## Determine all other box sizes by their relative proportions in freq:

    if (by == "cd") {  # (a) by condition:

      ## Level 2:
      ## 2 vertical rectangles for a) cond.true vs.cond.false:
      x_true <- (n_true/N) * x_pop # scale x_pop by proportion true
      x_y_true <- x_pop/x_true

      x_false <- (n_false/N) * x_pop # scale x_pop by proportion false
      x_y_false <- x_pop/x_false

      if (!all.equal(x_pop^2, sum(c((x_true * x_pop), (x_false * x_pop)), na.rm = TRUE))) {
        warning("vrectree 1a: Sum of cond.TRUE + cond.FALSE areas differs from Population area.")
      }

      ## Level 3:
      ## 4 rectangles with widths matching the widths of the Level 2 rectangles
      ## (and their heights summing up to the height of the Level 2 rectangles)

      x_hi <- x_true # keep constant
      x_y_hi <- x_y_true * (n_hi/n_true) # scale previous prop by prop hi

      x_mi <- x_true # keep constant
      x_y_mi <- x_y_true * (n_mi/n_true) # scale previous prop by prop mi

      if (!all.equal((x_true * x_pop),
                     sum(c((x_hi * (x_hi * x_y_hi)), (x_mi * (x_mi * x_y_mi))), na.rm = TRUE))) {
        warning("vrectree 2a: Sum of HI + MI area differs from Cond TRUE area.")
      }

      x_fa <- x_false # keep constant
      x_y_fa <- x_y_false * (n_fa/n_false) # scale previous prop by prop fa

      x_cr <- x_false # keep constant
      x_y_cr <- x_y_false * (n_cr/n_false) # scale previous prop by prop cr

      if (!all.equal((x_false * x_pop),
                     sum(c((x_fa * (x_fa * x_y_fa)), (x_cr * (x_cr * x_y_cr))), na.rm = TRUE))) {
        warning("vrectree 3a: Sum of FA + CR area differs from Cond FALSE area.")
      }

      if (!all.equal((x_pop^2),
                     sum(c((x_hi * (x_hi * x_y_hi)),
                           (x_mi * (x_mi * x_y_mi)),
                           (x_fa * (x_fa * x_y_fa)),
                           (x_cr * (x_cr * x_y_cr))), na.rm = TRUE))) {
        warning("vrectree 4a: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 7 boxes:
      x_boxes <- c(x_pop,  x_true, x_false,
                   x_hi, x_mi, x_fa, x_cr) # specific widths
      x_y_prop <- c(x_y_pop,  x_y_true, x_y_false,
                    x_y_hi, x_y_mi, x_y_fa, x_y_cr) # specific proportions

    } else if (by == "dc") {  # (b) by decision:

      ## Level 2:
      ## 2 vertical rectangles for b) dec.pos vs. dec.neg:
      x_pos <- (n_pos/N) * x_pop  # scale x_pop by proportion of dec.pos
      x_y_pos <- x_pop/x_pos

      x_neg <- (n_neg/N) * x_pop  # scale x_pop by proportion of dec.neg
      x_y_neg <- x_pop/x_neg

      if (!all.equal(x_pop^2, sum(c((x_pos * x_pop), (x_neg * x_pop)), na.rm = TRUE))) {
        warning("vrectree 1b: Sum of dec.POS + dec.FALSE areas differs from Population area.")
      }

      ## Level 3:
      ## 4 rectangles with widths matching the widths of the Level 2 rectangles
      ## (and their heights summing up to the height of the Level 2 rectangles)

      x_hi <- x_pos  # keep constant to Level 2
      x_y_hi <- x_y_pos * (n_hi/n_pos)  # scale previous prop by prop n_hi/n_pos = PPV

      # FA (now belongs to dec.pos):
      x_fa <- x_pos  # keep constant to Level 2
      x_y_fa <- x_y_pos * (n_fa/n_pos)  # scale previous prop by prop n_fa/n_pos = (1 - PPV)

      if (!all.equal((x_pos * x_pop),
                     sum(c((x_hi * (x_hi * x_y_hi)), (x_fa * (x_fa * x_y_fa))), na.rm = TRUE))) {
        warning("vrectree 2b: Sum of HI + FA area differs from dec.POS area.")
      }

      x_cr <- x_neg  # keep constant to Level 2
      x_y_cr <- x_y_neg * (n_cr/n_neg)  # scale previous prop by prop n_cr/n_neg = NPV

      # MI (now belongs to dec.neg):
      x_mi <- x_neg  # keep constant to Level 2
      x_y_mi <- x_y_neg * (n_mi/n_neg)  # scale previous prop by prop n_mi/n_neg = (1 - NPV)

      if (!all.equal((x_neg * x_pop),
                     sum(c((x_mi * (x_mi * x_y_mi)), (x_cr * (x_cr * x_y_cr))), na.rm = TRUE))) {
        warning("vrectree 3b: Sum of MI + CR area differs from dec.NEG area.")
      }

      if (!all.equal((x_pop^2),
                     sum(c((x_hi * (x_hi * x_y_hi)),
                           (x_mi * (x_mi * x_y_mi)),
                           (x_fa * (x_fa * x_y_fa)),
                           (x_cr * (x_cr * x_y_cr))), na.rm = TRUE))) {
        warning("vrectree 4b: Population area differs from the area sum of all 4 SDT cases.")
      }

      ## Collect widths and proportions of 7 boxes:
      x_boxes <- c(x_pop,  x_pos, x_neg,
                   x_hi, x_mi, x_fa, x_cr) # specific widths
      x_y_prop <- c(x_y_pop,  x_y_pos, x_y_neg,
                    x_y_hi, x_y_mi, x_y_fa, x_y_cr) # specific proportions


    } else if (by == "cddc") {  # (c) by condition and by decision:


      ## Level 1: Population square
      x_pop <- .08    # shrink basic width of population box: Area N = x_pop^2
      x_y_pop <- 1/1  # square


      ## Level 2 (top):
      ## 2 vertical rectangles for a) cond.true vs.cond.false:
      x_true <- (n_true/N) * x_pop # scale x_pop by proportion true
      x_y_true <- x_pop/x_true

      x_false <- (n_false/N) * x_pop # scale x_pop by proportion false
      x_y_false <- x_pop/x_false

      if (!all.equal(x_pop^2, sum(c((x_true * x_pop), (x_false * x_pop)), na.rm = TRUE))) {
        warning("vrectree 1c: Sum of cond.TRUE + cond.FALSE areas differs from Population area.")
      }


      ## Level 2 (bot):
      ## 2 vertical rectangles for b) dec.pos vs. dec.neg:
      x_pos <- (n_pos/N) * x_pop  # scale x_pop by proportion of dec.pos
      x_y_pos <- x_pop/x_pos

      x_neg <- (n_neg/N) * x_pop  # scale x_pop by proportion of dec.neg
      x_y_neg <- x_pop/x_neg

      if (!all.equal(x_pop^2, sum(c((x_pos * x_pop), (x_neg * x_pop)), na.rm = TRUE))) {
        warning("vrectree 1c: Sum of dec.POS + dec.FALSE areas differs from Population area.")
      }


      ## Level 3:  (Note: Simply copied from (by == "cd") above!)
      ## 4 rectangles with widths matching the widths of the Level 2 rectangles
      ## (and their heights summing up to the height of the Level 2 rectangles)

      x_hi <- x_true  # keep constant
      x_y_hi <- x_y_true * (n_hi/n_true)  # scale previous prop by prop hi

      x_mi <- x_true # keep constant
      x_y_mi <- x_y_true * (n_mi/n_true)  # scale previous prop by prop mi

      if (!all.equal((x_true * x_pop),
                     sum(c((x_hi * (x_hi * x_y_hi)), (x_mi * (x_mi * x_y_mi))), na.rm = TRUE))) {
        warning("vrectree 2c: Sum of HI + MI area differs from Cond TRUE area.")
      }

      x_fa <- x_false  # keep constant
      x_y_fa <- x_y_false * (n_fa/n_false)  # scale previous prop by prop fa

      x_cr <- x_false  # keep constant
      x_y_cr <- x_y_false * (n_cr/n_false)  # scale previous prop by prop cr

      if (!all.equal((x_false * x_pop),
                     sum(c((x_fa * (x_fa * x_y_fa)), (x_cr * (x_cr * x_y_cr))), na.rm = TRUE))) {
        warning("vrectree 3c: Sum of FA + CR area differs from Cond FALSE area.")
      }

      if (!all.equal((x_pop^2),
                     sum(c((x_hi * (x_hi * x_y_hi)),
                           (x_mi * (x_mi * x_y_mi)),
                           (x_fa * (x_fa * x_y_fa)),
                           (x_cr * (x_cr * x_y_cr))), na.rm = TRUE))) {
        warning("vrectree 4c: Population area differs from the area sum of all 4 SDT cases.")
      }


      ## Collect widths and proportions of 10 boxes:
      x_boxes <- c(x_pop,  x_true, x_false,
                   x_hi, x_mi, x_fa, x_cr,
                   x_pos, x_neg,  x_pop)             # specific widths
      x_y_prop <- c(x_y_pop,  x_y_true, x_y_false,
                    x_y_hi, x_y_mi, x_y_fa, x_y_cr,
                    x_y_pos, x_y_neg,  x_y_pop)      # specific proportions


    } else if (by == "dccd") {  # (d) 1st by decision, 2nd by condition:


      ## Level 1: Population square
      x_pop <- .08    # shrink basic width of population box: Area N = x_pop^2
      x_y_pop <- 1/1  # square


      ## Level 2 (top):
      ## 2 vertical rectangles for b) dec.pos vs. dec.neg:
      x_pos <- (n_pos/N) * x_pop  # scale x_pop by proportion of dec.pos
      x_y_pos <- x_pop/x_pos

      x_neg <- (n_neg/N) * x_pop  # scale x_pop by proportion of dec.neg
      x_y_neg <- x_pop/x_neg

      if (!all.equal(x_pop^2, sum(c((x_pos * x_pop), (x_neg * x_pop)), na.rm = TRUE))) {
        warning("vrectree 1d: Sum of dec.POS + dec.FALSE areas differs from Population area.")
      }

      ## Level 2 (bot):
      ## 2 vertical rectangles for a) cond.true vs.cond.false:
      x_true <- (n_true/N) * x_pop # scale x_pop by proportion true
      x_y_true <- x_pop/x_true

      x_false <- (n_false/N) * x_pop # scale x_pop by proportion false
      x_y_false <- x_pop/x_false

      if (!all.equal(x_pop^2, sum(c((x_true * x_pop), (x_false * x_pop)), na.rm = TRUE))) {
        warning("vrectree 1d: Sum of cond.TRUE + cond.FALSE areas differs from Population area.")
      }


      ## Level 3:
      ## 4 rectangles with widths matching the widths of the Level 2 rectangles
      ## (and their heights summing up to the height of the Level 2 rectangles)

      ## dec.POS cases:
      x_hi <- x_pos  # keep constant
      x_y_hi <- x_y_pos * (n_hi/n_pos)  # scale previous prop by prop n_hi/n_pos (PPV)

      x_fa <- x_pos  # keep constant
      x_y_fa <- x_y_pos * (n_fa/n_pos)  # scale previous prop by prop n.FA/n_pos (1 - PPV)

      if (!all.equal((x_pos * x_pop),
                     sum(c((x_hi * (x_hi * x_y_hi)), (x_fa * (x_fa * x_y_fa))), na.rm = TRUE))) {
        warning("vrectree 2d: Sum of HI + FA area differs from dec.POS area.")
      }


      ## dec.NEG cases:
      x_cr <- x_neg  # keep constant
      x_y_cr <- x_y_neg * (n_cr/n_neg)  # scale previous prop by prop n_cr/n_neg (NPV)

      x_mi <- x_neg # keep constant
      x_y_mi <- x_y_neg * (n_mi/n_neg)  # scale previous prop by prop n_mi/n_neg (1 - NPV)


      if (!all.equal((x_neg * x_pop),
                     sum(c((x_mi * (x_mi * x_y_mi)), (x_cr * (x_cr * x_y_cr))), na.rm = TRUE))) {
        warning("vrectree 3d: Sum of MI + CR area differs from dec.NEG area.")
      }

      if (!all.equal((x_pop^2),
                     sum(c((x_hi * (x_hi * x_y_hi)),
                           (x_mi * (x_mi * x_y_mi)),
                           (x_fa * (x_fa * x_y_fa)),
                           (x_cr * (x_cr * x_y_cr))), na.rm = TRUE))) {
        warning("vrectree 4d: Population area differs from the area sum of all 4 SDT cases.")
      }


      ## Collect widths and proportions of 10 boxes:
      x_boxes <- c(x_pop,
                   x_pos, x_neg,
                   x_hi, x_mi, x_fa, x_cr,
                   x_true, x_false,
                   x_pop)             # specific widths
      x_y_prop <- c(x_y_pop,
                    x_y_pos, x_y_neg,
                    x_y_hi, x_y_mi, x_y_fa, x_y_cr,
                    x_y_true, x_y_false,
                    x_y_pop)      # specific proportions

    } # (if by...)

  } # (area == "vr")...

  else {  # 5e. ANY other area-setting:

    if (by == "cd" || by == "dc") {  # (a) by condition OR (b) by decision:

      ## Box size and proportions:
      x_pop <- .10    # basic width of population box
      x_y_pop <- 2/3  # basic proportion is rectangular (width > height)

      ## Collect all sizes and proportions:
      x_boxes <- rep(x_pop, 7)     # all boxes have the same width
      x_y_prop <- rep(x_y_pop, 7)  # all boxes have the same proportion

    } else {  # (c) + (d) + ANY other by-setting:

      x_pop <- .05    # basic width of population box
      x_y_pop <- 1/1  # basic proportion is square (width = height)

      ## Collect all sizes and proportions:
      x_boxes <- rep(x_pop, 10)     # all boxes have the same width
      x_y_prop <- rep(x_y_pop, 10)  # all boxes have the same proportion

    } # if (by...)

  } # if (area == ...)



  ## (6) Plot matrix M (from diagram package): ----------

  if (by == "cd" || by == "dc") {  # (a) by condition OR (b) by decision:

    box_pos = c(1, 2, 4)  # 7 boxes

  } else if (by == "cddc" || by == "dccd") {  # (c) + (d) by condition AND by decision:

    box_pos = c(1, 2, 4, 2, 1)  # 10 boxes

  } else { # ANY other by-setting:

    box_pos = c(1, 2, 4, 2, 1)  # 10 boxes

  } # if (by...)


  pp <- diagram::plotmat(M, # square coefficient matrix, specifying the links (rows = to, cols = from)
                         pos = box_pos,  # 7 or 10 boxes
                         curve = 0.0, # no curve (> 0 curve left, < 0 curve right)
                         name = names,
                         relsize	= .98, # a scaling factor for the size of the graph
                         lwd = lwd,  # width of arrows
                         ## Boxes:
                         box.size = x_boxes,   # widths of boxes
                         box.prop = x_y_prop,  # proportionality (length/width) ratio of boxes
                         box.type = "rect",    # "rect", "ellipse", "diamond", "circle", "hexa", "multi", "none"
                         box.col = col_boxes,  # scalar or vector (of length 7 or 10?).
                         # c(col.N, col.true, col.false, col.hi, col.mi, col.fa, col.cr), # WAS: "lightyellow"
                         box.lcol = col_border,  # col_boxes,
                         box.lwd = box_lwd,  # set to 0.001 to show boxes without borders (but =0 yields error)
                         lcol = col_border,  # default color for box and arrow lines
                         ## Text in Boxes:
                         txt.col = col_txt,
                         box.cex = cex_lbl,  # relative size of text in boxes
                         txt.font = 1,       # 1 = plain, 2 = bold, ...
                         ## Arrows:
                         cex.txt = cex_arr,  # relative size of arrow text
                         arr.pos = .50,  # relative position of arrowhead on arrow segment/curve
                         arr.type = "triangle", # one of "curved", "triangle", "circle", "ellipse", "T", "simple"
                         arr.length = .20,
                         arr.width = .15,
                         arr.col = col_border,
                         shadow.size = cex_shadow,  # .005
                         shadow.col = col_shadow    #,
                         # main = paste0(title_lbl, ":\n", "Sum tree of natural frequencies (N = ", N, ")")
  )

  ## (7) Title: ----------

  if (nchar(title_lbl) > 0) { title_lbl <- paste0(title_lbl, ":\n") }  # put on top (in separate line)
  if ((by == "cd") || (by == "dc")) {type_lbl <- "Tree"} else {type_lbl <- "Net"}

  if (area == "no") {area_lbl <- ""}
  else if (area == "sq") {area_lbl <- "Areas represent relative frequencies"}
  else if (area == "hr") {area_lbl <- "Areas represent relative frequencies"}
  else if (area == "vr") {area_lbl <- "Areas represent relative frequencies"}
  else {area_lbl <- ""}  # to prevent errors for other entries

  if (by == "cd") {by_lbl <- "(by condition)"}
  else if (by == "dc") {by_lbl <- "(by decision)"}
  else if (by == "cddc") {by_lbl <- "(by condition and decision)"}
  else if (by == "dccd") {by_lbl <- "(by decision and condition)"}
  else {by_lbl <- ""}  # to prevent errors for other entries

  cur_title_lbl <- paste0(title_lbl, type_lbl, " of frequencies and probabilities ", by_lbl)  # , "(N = ", N, ")")
  title(cur_title_lbl, line = 0, adj = 0, font.main = 1, cex.main = 1.2)  # (centered, not raised, normal font)


  ## (8) Margin text: ----------

  ## Text parameters:
  m_col <- grey(.33, .99)  # color
  m_cex <- .85             # size

  ##   (A) on left side (adj = 0): ----

  ## A1. freq label:
  freq_lbl <- make_freq_lbl(hi = n_hi, mi = n_mi, fa = n_fa, cr = n_cr)   # use current freq values
  mtext(freq_lbl, side = 1, line = 0, adj = 0, col = m_col, cex = m_cex)  # print freq label

  ## A2. Condition / p(cond) label:
  cur_cond_lbl <- make_cond_lbl(prev, sens, spec)  # use utility function to format label
  mtext(cur_cond_lbl, side = 1, line = 1, adj = 0, col = m_col, cex = m_cex)  # print label

  ## A3. Note that areas represent frequencies:
  if (area != "no") {

    cur_area_lbl <- paste0("(", area_lbl, ")")
    mtext(cur_area_lbl, side = 1, line = 2, adj = 0, col = m_col, cex = m_cex)  # print label

  }

  ##   (B) on right side (adj = 1): ----

  ## B1. Accuracy label: Compute and show accuracy metrics
  if (show_accu) {

    # (1) Compute accuracy info based on current freq (which may be rounded OR not rounded):
    cur_accu <- comp_accu_freq(hi = n_hi, mi = n_mi, fa = n_fa, cr = n_cr, w = w_acc)

    # Note: If freq are NOT rounded, then
    #       cur_accu <- comp_accu_prob(prev = prev, sens = sens, spec = spec, w = w_acc)
    #       would yield the same results.

    # (2) Make label:
    cur_accu_lbl <- make_accu_lbl(acc = cur_accu$acc, w = w_acc, wacc = cur_accu$wacc, mcc = cur_accu$mcc)  # use utility function

    # (3) Mark IF accu was based on rounded freq:
    if (round) {  # freq were rounded:
      cur_accu_lbl <- paste0("*", cur_accu_lbl, " (rounded)")
    }

    # (4) Plot accu label:
    mtext(cur_accu_lbl, side = 1, line = 0, adj = 1, col = m_col, cex = m_cex)  # print label

  } # if (show_accu)...


  ## B2. Decision / p(dec) label:
  if (by != "cd") {

    cur_dec_lbl <- make_dec_lbl(ppod, PPV, NPV)  # use utility function to format label
    mtext(cur_dec_lbl, side = 1, line = 1, adj = 1, col = m_col, cex = m_cex)  # print label

  }

  ## B3. Imprint:
  imprint_lbl <- ""  # "[\uA9riskyr]"
  mtext(paste0(imprint_lbl, " "), side = 1, line = 2, adj = 1, col = m_col, cex = m_cex)



  ## (9) Return what? : ----------
  # return(pp)      # returns diagram object
  # return()        # returns nothing
  # return("nice")  # returns nothing

  ## Finish: ----

  invisible()  # restores par(opar)

} # plot_fnet(...) end.

## Check: -----

# # Plotting existing freq:
# plot_fnet()  # => plot current freq with default options
# plot_fnet(by = "dccd")
# plot_fnet(area = "no")
# plot_fnet(p_lbl = "num")
# plot_fnet(title_lbl = "")
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
# plot_fnet(by = "cd", area = "sq", p_lbl = "nam")  # => by condition + squares               + probability names
# plot_fnet(by = "cd", area = "hr", p_lbl = "num")  # => by condition + horizontal rectangles + probability numbers
# plot_fnet(by = "dc", area = "sq", p_lbl = "num")  # => by decision  + squares               + mix of names and numbers
# plot_fnet(by = "dc", area = "vr", p_lbl = "mix")  # => by decision  + vertical rectangles   + minimal labels
#
# # Rounding:
# plot_fnet(prev = .10, sens = .70, spec = .90, N = 10, by = "cddc", area = "sq", p_lbl = "num", round = TRUE)  # => mi = 0
# plot_fnet(prev = .10, sens = .70, spec = .90, N = 10, by = "cddc", area = "sq", p_lbl = "num", round = FALSE) # => mi = 0.3
#
# # Custom colors and shadows:
# plot_fnet(prev = .08, sens = .92, spec = .95, N = 10000, area = "hr")
# plot_fnet(area = "sq", col_boxes = "gold", col_border = "steelblue4", col_shadow = "steelblue4", cex_shadow = .008)
# plot_fnet(N = NA, area = "vr", col_txt = "steelblue4", col_boxes = "lightyellow", col_border = grey(.3, .7), cex_shadow = .008, col_shadow = grey(.1, .9))


## (*) Done: -----------

## - Increase robustness by anticipating and
##   correcting common entry errors.         [2018 09 06]
## - Clean up code.                          [2018 08 24]
## - Update argument and variables
##   to new naming scheme (snake_case).      [2018 11 22]

## (+) ToDo: -----------

## - Add a 3rd perspective:
##   "by accuracy" or correctness/correspondence of condition and decision:
##   "dec.cor" vs. "dec.err" (i.e., diagonal of confusion matrix)
## - Retire function and re-direct to newer and more versatile
##   "plot_prism" function!

## eof. -----------

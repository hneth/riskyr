## init_pal.R | riskyr
## 2018 08 28
## Define and initialize the current set
## of custom colors (pal):
## -----------------------------------------------
## pal contains defaults for user inputs.

## (A) Define colors: ----------

## (1) Select and name some colors: ---------

## (1) from uni.kn:
seeblau <- rgb(0, 169, 224, max = 255) # seeblau.4 (non-transparent)

## (2) from https://bootswatch.com/sandstone/:
col.sand.light <- rgb(248, 245, 240, max = 255)
col.sand.mid   <- rgb(142, 140, 132, max = 255)
col.sand.dark  <- rgb(62, 63, 58, max = 255)

col.grey.1 <- rgb(181, 179, 174, max = 255)
col.grey.2 <- rgb(123, 121, 113, max = 255)
col.grey.3 <- "grey25"
col.grey.4 <- "grey10"

col.green.1 <- rgb(184, 217, 137, max = 255)
col.green.2 <- rgb(128, 177, 57, max = 255)

col.red.1 <- rgb(230, 142, 140, max = 255)
col.red.2 <- rgb(210, 52, 48, max = 255)

col.blue.1 <- rgb(115, 200, 234, max = 255)
col.blue.2 <- rgb(121, 149, 177, max = 255)
col.blue.3 <- rgb(29, 149, 198, max = 255)
col.blue.4 <- rgb(40, 74, 108, max = 255)

col.orange.1 <- rgb(247, 169, 127, max = 255)
col.orange.2 <- rgb(242, 100, 24, max = 255)

## (3) basic colors + transparency:

my.red   <- "tomato3"
my.blue  <- "steelblue3"
my.green <- "olivedrab4"

my.yellow <- "lightgoldenrod1"
my.orange <- "sienna1"

green.1 <- makeTransparent(my.green, alpha = .50)
green.2 <- makeTransparent(my.green, alpha = 1.0)

red.1 <- makeTransparent(my.red, alpha = .50)
red.2 <- makeTransparent(my.red, alpha = 1.0)

blue.1 <- makeTransparent(my.blue, alpha = .50)
blue.2 <- makeTransparent(my.blue, alpha = 1.0)

yellow.1 <- makeTransparent(my.yellow, alpha = .50)
yellow.2 <- makeTransparent(my.yellow, alpha = 1.0)

orange.1 <- makeTransparent(my.orange, alpha = .50)
orange.2 <- makeTransparent(my.orange, alpha = 1.0)

my.whitish <- "antiquewhite" # "whitesmoke"
my.bluish <- "aliceblue"


## (2) Assign colors to recurring elements/roles ----------
##     (to set default colors for plots and app display):

## (a) Define base color (for population N):

col.N <- grey(.90, .99)  # "white", col.grey.1

## (b) by condition: Define 2 colors for condition cases:

col.true  <- my.yellow        # "lightgoldenrod1" "gold1", col.orange.1, "yellow2"
col.false <- "lightskyblue2"  #, my.blue, "deepskyblue1" # "lightskyblue2" # col.blue.1
## Combine in a named vector:
cond.colors <- setNames(c(col.true, col.false),
                        c("true", "false"))

## (c) by decision: Define 3 colors for decision cases:

col.pos <- "rosybrown3" # "khaki", my.whitish
col.neg <- "lightsteelblue3" # "lightsteelblue2", "wheat", "lightsteelblue1", my.bluish, "chartreuse4"
## Combine in a named vector:
dec.colors <- setNames(c(col.pos, col.neg),
                       c("pos", "neg"))

## (d) Accuracy colors:

col.cor <- "palegreen3" # "forestgreen" # correct/accurate decisions
col.err <- "firebrick3" # erroneous/inaccurate decisions

acc.colors <- setNames(c(col.cor, col.err),
                       c("cor", "err"))

## (e) by condition x decision: Define 4 colors for SDT cases:

col.hi <- my.green        # "olivedrab4", "palegreen4", col.green.2, "chartreuse4"
col.mi <- my.red          # "tomato3", "orangered3", "firebrick3", col.red.2
col.fa <- "lightsalmon2"  # lightcoral" # "tomato1" # "orangered1" # "firebrick1", col.red.1
col.cr <- "olivedrab3"    # "springgreen2" # "palegreen3" # col.green.1
## Combine in a named vector:
sdt.colors <- setNames(c(col.hi, col.mi, col.fa, col.cr),
                       c("hi", "mi", "fa", "cr")
)

## (f) Define 2 colors for PVs:

col.ppv <- my.orange  # "sienna1" # col.orange.2 # "orange3" "firebrick" "red3"
col.npv <- my.blue    # "steelblue3", col.blue.3, "green4" "gray50" "brown4"

## (g) Other colors:

## Text and borders:

col.txt <- grey(.01, .99)  # near "black" text labels, NA removes text?
col.brd <- grey(.20, .99)  # greyish borders, NA removes borders

## Probability lines (within Mosaic/area plots):

prev.li <- "gold"       # "aliceblue"
sens.li <- "cornsilk"   # "darkseagreen1" "olivedrab1"
spec.li <- "whitesmoke" # "lemonchiffon" "antiquewhite"

col_p <- c(prev.li, sens.li, spec.li)  # 3 bright colors (visible on SDT rectangles)


## (3) Define corresponding default palette: ----------

pal.def <- c(col.N,
             cond.colors, dec.colors, acc.colors,
             sdt.colors,
             col.ppv, col.npv,
             col.txt, col.brd)  # vector of colors
pal.def <- setNames(object = pal.def,
                    nm = c("N",
                           names(cond.colors), names(dec.colors), names(acc.colors),
                           names(sdt.colors),
                           "ppv", "npv",
                           "txt", "brd")
)
n.colors <- length(pal.def)  # number of colors for which defaults are defined
# n.colors

## (B) Initialization function for all color elements (titles and labels): ----------

## init_pal: Documentation ------

#' Initialize basic color information.
#'
#' \code{init_pal} initializes basic color information
#' (i.e., all colors corresponding to functional roles in
#' the current scenario and used throughout the \code{riskyr} package).
#'
#' All color information of the current scenario
#' is stored as named colors in a list \code{pal}.
#' \code{init_pal} allows changing colors by assigning
#' new colors to existing names.
#'
#' @param col.N Color representing the \emph{population} of \code{\link{N}} cases or individuals.
#'
#' @param col.true Color representing cases of \code{\link{cond.true}}, for which the current condition is \code{TRUE}.
#' @param col.false Color representing cases of in \code{\link{cond.false}}, for which the current condition is \code{FALSE}.
#'
#' @param col.pos Color representing cases of \code{\link{dec.pos}}, for which the current decision is \code{positive}.
#' @param col.neg Color representing cases in \code{\link{dec.neg}}, for which the current decision is \code{negative}.
#'
#' @param col.cor Color representing cases of correct decisions \code{\link{dec.cor}}, for which the current decision is \code{accurate}.
#' @param col.err Color representing cases in erroneous decisions \code{\link{dec.err}}, for which the current decision is \code{inaccurate}.
#'
#' @param col.hi Color representing \emph{hits} or true positives in \code{\link{hi}}
#' (i.e., correct cases for which the current condition is TRUE and the decision is positive).
#' @param col.mi Color representing \emph{misses} or false negatives in \code{\link{mi}}
#' (i.e., incorrect cases for which the current condition is TRUE but the decision is negative).
#' @param col.fa Color representing \emph{false alarms} or false positives in \code{\link{fa}}
#' (i.e., incorrect cases for which the current condition is FALSE but the decision is positive).
#' @param col.cr Color representing \emph{correct rejections} or true negatives in \code{\link{cr}}
#' (i.e., correct cases for which the current condition is FALSE and the decision is negative).
#'
#' @param col.ppv Color representing \emph{positive predictive values} \code{\link{PPV}} (i.e., the conditional probability that
#' the condition is TRUE, provided that the decision is positive).
#' @param col.npv Color representing \emph{negative predictive values} \code{\link{NPV}} (i.e., the conditional probability that
#' the condition is FALSE, provided that the decision is negative).
#'
#' @param col.txt Color used for text labels.
#' @param col.brd Color used for borders (e.g., around bars or boxes).
#'
#'
#' @examples
#' init_pal()          # => define and return a vector of current (default) colors
#' length(init_pal())  # => 15 named colors
#' pal <- init_pal(col.N = "steelblue4")  # => change a color (stored in pal)
#' pal <- init_pal(col.brd = NA)          # => remove a color
#'
#'
#' @family functions initializing scenario information
#'
#'
#' @seealso
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{txt}} contains current text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @importFrom grDevices adjustcolor
#' @importFrom grDevices col2rgb
#' @importFrom grDevices grey
#' @importFrom grDevices rgb
#'
#' @importFrom stats setNames
#'
#' @export

## init_pal: Definition ------

init_pal <- function(col.N = pal.def["N"],          # population N
                     ## by condition:
                     col.true  = pal.def["true"],   # condition true
                     col.false = pal.def["false"],  # condition false
                     ## by decision:
                     col.pos  = pal.def["pos"],     # decision positive
                     col.neg = pal.def["neg"],      # decision negative
                     ## by accuracy:
                     col.cor  = pal.def["cor"],     # decision correct / accurate
                     col.err = pal.def["err"],      # decision erroneous / inaccurate
                     ## Combinations:
                     col.hi = pal.def["hi"],        # hits / true positives
                     col.mi = pal.def["mi"],        # misses / false negatives
                     col.fa = pal.def["fa"],        # false alarms / false positives
                     col.cr = pal.def["cr"],        # correct rejections / true negatives
                     ## Derived conditional probabilities:
                     col.ppv = pal.def["ppv"],      # positive predictive values
                     col.npv = pal.def["npv"],      # negative predictive values
                     ## Text labels and borders:
                     col.txt = pal.def["txt"],      # text labels
                     col.brd = pal.def["brd"]       # borders
) {

  ## 1. Initialize pal as a VECTOR:
  pal <- rep(NA, n.colors)

  ## 2. Pass arguments to VECTOR:
  pal <- c(col.N,      # population N
           ## by condition:
           col.true,   # condition true
           col.false,  # condition false
           ## by decision:
           col.pos,    # decision positive
           col.neg,    # decision negative
           ## by accuracy:
           col.cor,    # decision correct / accurate
           col.err,    # decision erroneous / inaccurate
           ## Combinations:
           col.hi,     # hits / true positives
           col.mi,     # misses / false negatives
           col.fa,     # false alarms / false positives
           col.cr,     # correct rejections / true negatives
           ## Derived conditional probabilities:
           col.ppv,    # positive predictive values
           col.npv,    # negative predictive values
           ## Text labels and borders:
           col.txt,    # text labels
           col.brd     # borders
  )

  ## 3. Name vector elements:
  pal <- setNames(object = pal,
                  nm = c("N",
                         names(cond.colors), names(dec.colors), names(acc.colors),
                         names(sdt.colors),
                         "ppv", "npv", "txt", "brd")
  )

  ## 4. Return vector:
  return(pal)

}

## Check:

# init_pal()          # => define and return a vector of current (default) colors
# length(init_pal())  # => 15 named colors
# pal <- init_pal(col.N = "steelblue4")  # => change a color (stored in pal)
# pal <- init_pal(col.brd = NA)          # => remove a color


## (C) Initialize vector pal with current color information: ----------

## pal: Documentation ------

#' List current values of scenario color palette.
#'
#' \code{pal} is initialized to a vector of named elements (colors)
#' to define the scenario color scheme that is
#' used throughout the \code{riskyr} package.
#'
#' All color information corresponding to the current scenario
#' is stored as named colors in a vector \code{pal}.
#' To change a color, assign a new color to an existing element name.
#'
#' \code{pal} currently contains colors with the following names:
#'
#' \enumerate{
#'
#' \item \code{N} Color representing the \emph{population} of \code{\link{N}} cases or individuals.
#'
#'
#' \item \code{true} Color representing cases of \code{\link{cond.true}}, for which the current condition is \code{TRUE}.
#'
#' \item \code{false} Color representing cases of in \code{\link{cond.false}}, for which the current condition is \code{FALSE}.
#'
#'
#' \item \code{pos} Color representing cases of \code{\link{dec.pos}}, for which the current decision is \code{positive}.
#'
#' \item \code{neg} Color representing cases in \code{\link{dec.neg}}, for which the current decision is \code{negative}.
#'
#'
#' \item \code{cor} Color representing cases of correct decisions \code{\link{dec.cor}}, for which the current decision is \code{accurate}.
#'
#' \item \code{err} Color representing cases of erroneous decisions \code{\link{dec.err}}, for which the current decision is \code{inaccurate}.
#'
#'
#' \item \code{hi} Color representing \emph{hits} or true positives in \code{\link{hi}}
#' (i.e., correct cases for which the current condition is TRUE and the decision is positive).
#'
#' \item \code{mi} Color representing \emph{misses} or false negatives in \code{\link{mi}}
#' (i.e., incorrect cases for which the current condition is TRUE but the decision is negative).
#'
#' \item \code{fa} Color representing \emph{false alarms} or false positives in \code{\link{fa}}
#' (i.e., incorrect cases for which the current condition is FALSE but the decision is positive).
#'
#' \item \code{cr} Color representing \emph{correct rejections} or true negatives in \code{\link{cr}}
#' (i.e., correct cases for which the current condition is FALSE and the decision is negative).
#'
#'
#' \item \code{ppv} Color representing \emph{positive predictive values} \code{\link{PPV}} (i.e., the conditional probability that
#' the condition is TRUE, provided that the decision is positive).
#'
#' \item \code{npv} Color representing \emph{negative predictive values} \code{\link{NPV}} (i.e., the conditional probability that
#' the condition is FALSE, provided that the decision is negative).
#'
#'
#' \item \code{txt} Color used for text labels.
#'
#' \item \code{brd} Color used for borders.
#'
#' }
#'
#'
#' @examples
#' pal        # shows all current color names and values
#' pal["hi"]  # shows the current color for hits (true positives)
#' pal["hi"] <- "green3"  # defines a new color for hits (true positives, TP)
#'
#'
#' @family lists containing current scenario information
#'
#'
#' @seealso
#' \code{\link{init_pal}} initializes color information;
#' \code{\link{num}} contains basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric parameters;
#' \code{\link{txt}} contains current text information;
#' \code{\link{init_txt}} initializes text information;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes current frequency information;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information.
#'
#' @export

## pal: Definition ------

pal <- init_pal()  ## apply

## Check:
# pal
# length(pal) # => 15 colors
# pal[2] == pal["true"]

## pal_bw: Define a black-and-white (b/w) palette: --------

#' Alternative color palette for black-and-white graphs.
#'
#' \code{pal_bw} is initialized to a vector of named elements (colors)
#' to define an alternative (black-and-white, b/w) scenario color scheme.
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_bw} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_bw        # shows all current color names and values
#' pal_bw["hi"]  # shows the current color for hits (true positives)
#' pal_bw["hi"] <- "grey" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_bw <- init_pal(col.N = grey(1.0, .99),     # white
                   col.true =  grey(.95, .99), # nearly white
                   col.false = grey(.95, .99), # nearly white
                   col.pos = grey(.90, .99),   # darker white
                   col.neg = grey(.90, .99),   # darker white
                   col.cor = grey(.85, .99),   # darker white
                   col.err = grey(.85, .99),   # darker white
                   col.hi = grey(.85, .99),    # white
                   col.mi = grey(.65, .99),
                   col.fa = grey(.65, .99),
                   col.cr = grey(.85, .99),
                   col.ppv = grey(.60, .99),   # lighter grey
                   col.npv = grey(.45, .99),   # darker grey
                   col.txt = "black",          # black
                   col.brd = "black"           # black
)

## Check:
# pal_bw
# pal_bw["hi"]

## Use bw color scheme (as default):
# pal <- pal_bw


## pal_4c: Define a reduced color palette: --------

#' Alternative color palette for graphs with only 4 colors.
#'
#' \code{pal_4c} is initialized to a vector of named elements (colors)
#' to define an alternative (reduced) scenario color scheme.
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_4c} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_4c        # shows all current color names and values
#' pal_4c["hi"]  # shows the current color for hits (true positives)
#' pal_4c["hi"] <- "grey" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_4c <- init_pal(col.N = grey(.85, .99),     # middle grey
                   col.true =  grey(.95, .99), # lighter grey
                   col.false = grey(.75, .99), # darker grey
                   col.pos = grey(.95, .99),   # lighter grey
                   col.neg = grey(.75, .99),   # darker grey
                   col.cor = grey(.95, .99),   # lighter grey
                   col.err = grey(.75, .99),   # darker grey
                   col.hi = pal["hi"],
                   col.mi = pal["mi"],
                   col.fa = pal["fa"],
                   col.cr = pal["cr"],
                   col.ppv = grey(.60, .99),   # darker grey
                   col.npv = grey(.45, .99),   # even darker grey
                   col.txt = "black",          # black
                   col.brd = "black"           # black
)

## Check:
# pal_4c
# pal_4c["N"]  # => "#D9D9D9FC"

## Use reduced color scheme (as default):
# pal <- pal_4c

## (D) Legacy code: ggplot themes: ----------

#   library("ggplot2")
#
#   my.theme <-  theme_bw() +
#     theme(plot.title = element_text(face = "bold", size = 12, color = col.grey.4, hjust = 0.0),
#           axis.title = element_text(face = "plain", size = 11, color = col.sand.dark),
#           axis.text = element_text(face = "plain", size = 10, color = col.sand.dark),
#           # axis.line = element_line(size = 0.75, color = "black", linetype = 1),
#           axis.ticks = element_line(size = 0.75, color = col.sand.mid, linetype = 1),
#           panel.background = element_rect(fill = "grey99", color = col.sand.dark),
#           panel.grid.major.x = element_line(color = col.sand.light, linetype = 1, size = .2),
#           panel.grid.major.y = element_line(color = col.sand.light, linetype = 1, size = .2),
#           # panel.grid.minor.x = element_blank(),
#           # panel.grid.minor.y = element_blank(),
#           legend.position = "none"
#     )
#
#   my.theme.legend <- theme_bw() +
#     theme(plot.title = element_text(face = "bold", size = 12, color = col.grey.4, hjust = 0.0),
#           axis.title = element_text(face = "plain", size = 11, color = col.sand.dark),
#           axis.text = element_text(face = "plain", size = 10, color = col.sand.dark),
#           # axis.line = element_line(size = 0.75, color = "black", linetype = 1),
#           axis.ticks = element_line(size = 0.75, color = col.sand.mid, linetype = 1),
#           panel.background = element_rect(fill = "grey99", color = col.sand.dark),
#           panel.grid.major.x = element_line(color = col.sand.light, linetype = 1, size = .2),
#           panel.grid.major.y = element_line(color = col.sand.light, linetype = 1, size = .2)#,
#           # panel.grid.minor.x = element_blank(),
#           # panel.grid.minor.y = element_blank()#,
#           # legend.position = "none"
#     )


## (*) Done: ----------

## - Clean up code.  [2018 08 26]

## (+) ToDo: ----------

## - Add pre-defined color palettes. For instance,
##   - b/w palette
##   - b/w-color high-lighting palettes (e.g., for 3 perspectives)
## - Make colors user-customizable

## eof. ------------------------------------------

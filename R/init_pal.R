## init_pal.R | riskyR
## 2018 01 28
## -----------------------------------------------
## Define and initialize the current set
## of custom colors (pal):

## pal contains defaults for user inputs.

## -----------------------------------------------
## (A) Define colors:

## -----------------------------------------------
## (1) Choose and name some colors:

{
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

  my.whitish <- "antiquewhite"
  my.bluish <- "aliceblue"

}

## -----------------------------------------------
## (2) Assign some colors to recurring elements/roles
##     (to set default colors for plots and app display):

{
  ## (a) Define base color (for population N):
  col.N <- grey(.95, .99)       # "white", col.grey.1

  ## (b) by condition: Define 2 colors for condition cases:
  col.true <- my.yellow         # "lightgoldenrod1" "gold1", col.orange.1, "yellow2"
  col.false <- "lightskyblue2"  #, my.blue, "deepskyblue1" # "lightskyblue2" # col.blue.1
  ## Combine in a named vector:
  cond.colors <- setNames(c(col.true, col.false),
                          c("true", "false")
                          )

  ## (c) by decision: Define 3 colors for decision cases:
  col.pos <- "rosybrown3" # "khaki", my.whitish
  col.neg <- "lightsteelblue2" #  "wheat", "lightsteelblue1", my.bluish
  ## Combine in a named vector:
  dec.colors <- setNames(c(col.pos, col.neg),
                         c("pos", "neg")
                         )

  ## (c) by condition x decision: Define 4 colors for SDT cases:
  col.hi <- my.green        # "olivedrab4", "palegreen4", col.green.2
  col.mi <- my.red          # "tomato3", "orangered3", "firebrick3", col.red.2
  col.fa <- "lightsalmon2"  # lightcoral" # "tomato1" # "orangered1" # "firebrick1", col.red.1
  col.cr <- "olivedrab3"    # "springgreen2" # "palegreen3" # col.green.1
  ## Combine in a named vector:
  sdt.colors <- setNames(c(col.hi, col.mi, col.fa, col.cr),
                         c("hi", "mi", "fa", "cr")
                         )

  ## (d) Define 2 colors for PVs:
  col.ppv <- my.orange  # "sienna1" # col.orange.2 # "orange3" "firebrick" "red3"
  col.npv <- my.blue    # "steelblue3", col.blue.3, "green4" "gray50" "brown4" "chartreuse4"
}

## -----------------------------------------------
## (3) Define corresponding default palette:

pal.def <- c(col.N, cond.colors, dec.colors, sdt.colors, col.ppv, col.npv)  # vector of colors
pal.def <- setNames(object = pal.def,
                    nm = c("N", names(cond.colors), names(dec.colors), names(sdt.colors), "ppv", "npv")
                    )
n.colors <- length(pal.def)  # number of colors for which defaults are defined

## -----------------------------------------------
## (B) Initialization function for all color
##     elements (all titles and labels):

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
#' @examples
#' init_pal()          # => define and return a vector of current (default) colors
#' length(init_pal())  # => 11 colors
#' pal <- init_pal(col.false = "firebrick2")  # => change current color (stored in pal)
#'
#' @family functions to initialize scenario settings
#'
#' @seealso
#' \code{\link{pal}} for current color settings;
#' \code{\link{txt}} for current text settings;
#' \code{\link{num}} for basic numeric parameters

init_pal <- function(col.N = pal.def["N"],          # population N
                     ## by condition:
                     col.true  = pal.def["true"],   # condition true
                     col.false = pal.def["false"],  # condition false
                     ## by decision:
                     col.pos  = pal.def["pos"],     # decision positive
                     col.neg = pal.def["neg"],      # decision negative
                     ## Combinations:
                     col.hi = pal.def["hi"],        # hits / true positives
                     col.mi = pal.def["mi"],        # misses / false negatives
                     col.fa = pal.def["fa"],        # false alarms / false positives
                     col.cr = pal.def["cr"],        # correct rejections / true negatives
                     ## Derived conditional probabilities:
                     col.ppv = pal.def["ppv"],      # positive predictive values
                     col.npv = pal.def["npv"]       # negative predictive values
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
           ## Combinations:
           col.hi,     # hits / true positives
           col.mi,     # misses / false negatives
           col.fa,     # false alarms / false positives
           col.cr,     # correct rejections / true negatives
           ## Derived conditional probabilities:
           col.ppv,    # positive predictive values
           col.npv     # negative predictive values
  )

  ## 3. Name vector elements:
  pal <- setNames(object = pal,
                  nm = c("N", names(cond.colors), names(dec.colors), names(sdt.colors), "ppv", "npv")
                  )

  ## 4. Return vector:
  return(pal)

}

## Check:
{
  # init_pal()          # => returns vector of current colors
  # length(init_pal())  # => 11
  # pal <- init_pal(col.false = "firebrick2")  # => change current color (stored in pal)
}

## -----------------------------------------------
## (C) Initialize a vector pal to contain
##     all current color information:

#' List current values of basic color information.
#'
#' \code{pal} is initialized to a vector of named elements (colors)
#' to define the color scheme for the current scenario that is
#' used throughout the \code{riskyr} package.
#'
#' All color information corresponding to the current scenario
#' is stored as named colors in a vector \code{pal}.
#' To change a color, assign a new color to an existing element name.
#'
#'
#' @param N Color representing the \emph{population} of \code{\link{N}} cases or individuals.
#'
#' @param true Color representing cases of \code{\link{cond.true}}, for which the current condition is \code{TRUE}.
#' @param false Color representing cases of in \code{\link{cond.false}}, for which the current condition is \code{FALSE}.
#'
#' @param pos Color representing cases of \code{\link{dec.pos}}, for which the current decision is \code{positive}.
#' @param neg Color representing cases in \code{\link{dec.neg}}, for which the current decision is \code{negative}.
#'
#' @param hi Color representing \emph{hits} or true positives in \code{\link{hi}}
#' (i.e., correct cases for which the current condition is TRUE and the decision is positive).
#' @param mi Color representing \emph{misses} or false negatives in \code{\link{mi}}
#' (i.e., incorrect cases for which the current condition is TRUE but the decision is negative).
#' @param fa Color representing \emph{false alarms} or false positives in \code{\link{fa}}
#' (i.e., incorrect cases for which the current condition is FALSE but the decision is positive).
#' @param cr Color representing \emph{correct rejections} or true negatives in \code{\link{cr}}
#' (i.e., correct cases for which the current condition is FALSE and the decision is negative).
#'
#' @param ppv Color representing \emph{positive predictive values} \code{\link{PPV}} (i.e., the conditional probability that
#' the condition is TRUE, provided that the decision is positive).
#' @param npv Color representing \emph{negative predictive values} \code{\link{NPV}} (i.e., the conditional probability that
#' the condition is FALSE, provided that the decision is negative).
#'
#' @examples
#' pal       # displays the vector of all current color names and values
#' pal["hi"] # displays the current color for hits (true positives)
#' pal["hi"] <- "green3" # defines a new color for hits (true positives)
#'
#'
#' @family lists containing basic scenario settings
#'
#' @seealso
#' \code{\link{init_pal}} to initialize color information

## Apply:
pal <- init_pal()

## Check:
{
  # pal
  # length(pal)
  # pal[2] == pal["true"]
}

## -----------------------------------------------
## ggplot themes:

{
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
}

## -----------------------------------------------
## (+) ToDo:

## - Add pre-defined color palettes & transparency
## - Make colors user-customizable

## -----------------------------------------------
## eof.

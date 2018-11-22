## init_pal.R | riskyr
## 2018 11 22
## Define custom color palettes (pal):
## -----------------------------------------------
## pal contains defaults for user inputs.

## (A) Define colors: ----------

## (1) Select and name some colors: ---------

## (1) from uni.kn:
seeblau <- rgb(0, 169, 224, max = 255) # seeblau.4 (non-transparent)

pal_uni_kn = data.frame( ## in one df:
  "seeblau1" = rgb(204, 238, 249, maxColorValue = 255), #  1. seeblau1 (non-transparent)
  "seeblau2" = rgb(166, 225, 244, maxColorValue = 255), #  2. seeblau2 (non-transparent)
  "seeblau3" = rgb( 89, 199, 235, maxColorValue = 255), #  3. seeblau3 (non-transparent)
  "seeblau4" = rgb(  0, 169, 224, maxColorValue = 255), #  4. seeblau4 (= seeblau base color)
  "black"    = rgb(  0,   0,   0, maxColorValue = 255), #  5. black
  "seegrau4" = rgb(102, 102, 102, maxColorValue = 255), #  6. grey40 (non-transparent)
  "seegrau3" = rgb(153, 153, 153, maxColorValue = 255), #  7. grey60 (non-transparent)
  "seegrau2" = rgb(204, 204, 204, maxColorValue = 255), #  8. grey80 (non-transparent)
  "seegrau1" = rgb(229, 229, 229, maxColorValue = 255), #  9. grey90 (non-transparent)
  "white"    = rgb(255, 255, 255, maxColorValue = 255), # 10. white
  stringsAsFactors = FALSE)

## (2) from https://bootswatch.com/sandstone/:
col_sand_light <- rgb(248, 245, 240, max = 255)
col_sand_mid   <- rgb(142, 140, 132, max = 255)
col_sand_dark  <- rgb(62, 63, 58, max = 255)

col_grey_1 <- rgb(181, 179, 174, max = 255)
col_grey_2 <- rgb(123, 121, 113, max = 255)
col_grey_3 <- "grey25"
col_grey_4 <- "grey10"

col_green_1 <- rgb(184, 217, 137, max = 255)
col_green_2 <- rgb(128, 177, 57, max = 255)

col_red_1 <- rgb(230, 142, 140, max = 255)
col_red_2 <- rgb(210, 52, 48, max = 255)

col_blue_1 <- rgb(115, 200, 234, max = 255)
col_blue_2 <- rgb(121, 149, 177, max = 255)
col_blue_3 <- rgb(29, 149, 198, max = 255)
col_blue_4 <- rgb(40, 74, 108, max = 255)

col_orange_1 <- rgb(247, 169, 127, max = 255)
col_orange_2 <- rgb(242, 100, 24, max = 255)

## (3) basic colors + transparency:

my_red   <- "tomato3"
my_blue  <- "steelblue3"
my_green <- "olivedrab4"

my_yellow <- "lightgoldenrod1"
my_orange <- "sienna1"

green_1 <- makeTransparent(my_green, alpha = .50)
green_2 <- makeTransparent(my_green, alpha = 1.0)

red_1 <- makeTransparent(my_red, alpha = .50)
red_2 <- makeTransparent(my_red, alpha = 1.0)

blue_1 <- makeTransparent(my_blue, alpha = .50)
blue_2 <- makeTransparent(my_blue, alpha = 1.0)

yellow_1 <- makeTransparent(my_yellow, alpha = .50)
yellow_2 <- makeTransparent(my_yellow, alpha = 1.0)

orange_1 <- makeTransparent(my_orange, alpha = .50)
orange_2 <- makeTransparent(my_orange, alpha = 1.0)

my_whitish <- "antiquewhite" # "whitesmoke"
my_bluish <- "aliceblue"


## (2) Assign colors to recurring elements/roles ----------
##     (to set default colors for plots and app display):

## (a) Define base color (for population N):

N_col <- grey(.90, .99)  # "white", col_grey_1

## (b) by condition: Define 2 colors for condition cases:

cond.true_col  <- my_yellow        # "lightgoldenrod1" "gold1", col_orange_1, "yellow2"
cond.false_col <- "lightskyblue2"  #, my_blue, "deepskyblue1" # "lightskyblue2" # col_blue_1
## Combine in a named vector:
cond.colors <- setNames(c(cond.true_col, cond.false_col),
                        c("true", "false"))

## (c) by decision: Define 3 colors for decision cases:

dec.pos_col <- "rosybrown3" # "khaki", my_whitish
dec.neg_col <- "lightsteelblue3" # "lightsteelblue2", "wheat", "lightsteelblue1", my_bluish, "chartreuse4"
## Combine in a named vector:
dec.colors <- setNames(c(dec.pos_col, dec.neg_col),
                       c("pos", "neg"))

## (d) Accuracy colors:

dec.cor_col <- "palegreen3" # "forestgreen" # correct/accurate decisions
dec.err_col <- "firebrick3" # erroneous/inaccurate decisions

acc.colors <- setNames(c(dec.cor_col, dec.err_col),
                       c("cor", "err"))

## (e) by condition x decision: Define 4 colors for SDT cases:

hi_col <- my_green        # "olivedrab4", "palegreen4", col_green_2, "chartreuse4"
mi_col <- my_red          # "tomato3", "orangered3", "firebrick3", col_red_2
fa_col <- "lightsalmon2"  # lightcoral" # "tomato1" # "orangered1" # "firebrick1", col_red_1
cr_col <- "olivedrab3"    # "springgreen2" # "palegreen3" # col_green_1
## Combine in a named vector:
sdt.colors <- setNames(c(hi_col, mi_col, fa_col, cr_col),
                       c("hi", "mi", "fa", "cr")
)

## (f) Define 2 colors for PVs:

PPV_col <- my_orange  # "sienna1" # col_orange_2 # "orange3" "firebrick" "red3"
NPV_col <- my_blue    # "steelblue3", col_blue_3, "green4" "gray50" "brown4"

## (g) Other colors:

## Text and borders:

txt_col <- grey(.01, .99)  # near "black" text labels, NA removes text?
brd_col <- grey(.20, .99)  # greyish borders, NA removes borders

## Probability lines (within Mosaic/area plots):

prev.li <- "gold"       # "aliceblue"
sens.li <- "cornsilk"   # "darkseagreen1" "olivedrab1"
spec.li <- "whitesmoke" # "lemonchiffon" "antiquewhite"

col_p <- c(prev.li, sens.li, spec.li)  # 3 bright colors (visible on SDT rectangles)


## (3) Define corresponding default palette: ----------

pal_def <- c(N_col,
             cond.colors, dec.colors, acc.colors,
             sdt.colors,
             PPV_col, NPV_col,
             txt_col, brd_col)  # vector of colors

pal_def <- setNames(object = pal_def,
                    nm = c("N",
                           names(cond.colors), names(dec.colors), names(acc.colors),
                           names(sdt.colors),
                           "ppv", "npv",
                           "txt", "brd")
                    )

n_colors <- length(pal_def)  # number of colors for which defaults are currently defined
# n_colors

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
#' @param N_col Color representing the \emph{population} of \code{\link{N}} cases or individuals.
#'
#' @param cond.true_col Color representing cases of \code{\link{cond.true}}, for which the current condition is \code{TRUE}.
#' @param cond.false_col Color representing cases of in \code{\link{cond.false}}, for which the current condition is \code{FALSE}.
#'
#' @param dec.pos_col Color representing cases of \code{\link{dec.pos}}, for which the current decision is \code{positive}.
#' @param dec.neg_col Color representing cases in \code{\link{dec.neg}}, for which the current decision is \code{negative}.
#'
#' @param dec.cor_col Color representing cases of correct decisions \code{\link{dec.cor}}, for which the current decision is \code{accurate}.
#' @param dec.err_col Color representing cases in erroneous decisions \code{\link{dec.err}}, for which the current decision is \code{inaccurate}.
#'
#' @param hi_col Color representing \emph{hits} or true positives in \code{\link{hi}}
#' (i.e., correct cases for which the current condition is TRUE and the decision is positive).
#' @param mi_col Color representing \emph{misses} or false negatives in \code{\link{mi}}
#' (i.e., incorrect cases for which the current condition is TRUE but the decision is negative).
#' @param fa_col Color representing \emph{false alarms} or false positives in \code{\link{fa}}
#' (i.e., incorrect cases for which the current condition is FALSE but the decision is positive).
#' @param cr_col Color representing \emph{correct rejections} or true negatives in \code{\link{cr}}
#' (i.e., correct cases for which the current condition is FALSE and the decision is negative).
#'
#' @param PPV_col Color representing \emph{positive predictive values} \code{\link{PPV}} (i.e., the conditional probability that
#' the condition is TRUE, provided that the decision is positive).
#' @param NPV_col Color representing \emph{negative predictive values} \code{\link{NPV}} (i.e., the conditional probability that
#' the condition is FALSE, provided that the decision is negative).
#'
#' @param txt_col Color used for text labels.
#' @param brd_col Color used for borders (e.g., around bars or boxes).
#'
#'
#' @examples
#' init_pal()          # => define and return a vector of current (default) colors
#' length(init_pal())  # => 15 named colors
#' pal <- init_pal(N_col = "steelblue4")  # => change a color (stored in pal)
#' pal <- init_pal(brd_col = NA)          # => remove a color
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

init_pal <- function(N_col = pal_def["N"],          # population N
                     ## by condition:
                     cond.true_col  = pal_def["true"],   # condition true
                     cond.false_col = pal_def["false"],  # condition false
                     ## by decision:
                     dec.pos_col  = pal_def["pos"],     # decision positive
                     dec.neg_col = pal_def["neg"],      # decision negative
                     ## by accuracy:
                     dec.cor_col  = pal_def["cor"],     # decision correct / accurate
                     dec.err_col = pal_def["err"],      # decision erroneous / inaccurate
                     ## Combinations:
                     hi_col = pal_def["hi"],        # hits / true positives
                     mi_col = pal_def["mi"],        # misses / false negatives
                     fa_col = pal_def["fa"],        # false alarms / false positives
                     cr_col = pal_def["cr"],        # correct rejections / true negatives
                     ## Derived conditional probabilities:
                     PPV_col = pal_def["ppv"],      # positive predictive values
                     NPV_col = pal_def["npv"],      # negative predictive values
                     ## Text labels and borders:
                     txt_col = pal_def["txt"],      # text labels
                     brd_col = pal_def["brd"]       # borders
) {

  ## 1. Initialize pal as a VECTOR:
  pal <- rep(NA, n_colors)

  ## 2. Pass arguments to VECTOR:
  pal <- c(N_col,      # population N
           ## by condition:
           cond.true_col,   # condition true
           cond.false_col,  # condition false
           ## by decision:
           dec.pos_col,    # decision positive
           dec.neg_col,    # decision negative
           ## by accuracy:
           dec.cor_col,    # decision correct / accurate
           dec.err_col,    # decision erroneous / inaccurate
           ## Combinations:
           hi_col,     # hits / true positives
           mi_col,     # misses / false negatives
           fa_col,     # false alarms / false positives
           cr_col,     # correct rejections / true negatives
           ## Derived conditional probabilities:
           PPV_col,    # positive predictive values
           NPV_col,    # negative predictive values
           ## Text labels and borders:
           txt_col,    # text labels
           brd_col     # borders
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
# pal <- init_pal(N_col = "steelblue4")  # => change a color (stored in pal)
# pal <- init_pal(brd_col = NA)          # => remove a color

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
#' \item \code{true} Color representing cases of \code{\link{cond.true}}, for which the current condition is \code{TRUE}.
#'
#' \item \code{false} Color representing cases of in \code{\link{cond.false}}, for which the current condition is \code{FALSE}.
#'
#' \item \code{pos} Color representing cases of \code{\link{dec.pos}}, for which the current decision is \code{positive}.
#'
#' \item \code{neg} Color representing cases in \code{\link{dec.neg}}, for which the current decision is \code{negative}.
#'
#' \item \code{cor} Color representing cases of correct decisions \code{\link{dec.cor}}, for which the current decision is \code{accurate}.
#'
#' \item \code{err} Color representing cases of erroneous decisions \code{\link{dec.err}}, for which the current decision is \code{inaccurate}.
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
#' \item \code{ppv} Color representing \emph{positive predictive values} \code{\link{PPV}} (i.e., the conditional probability that
#' the condition is TRUE, provided that the decision is positive).
#'
#' \item \code{npv} Color representing \emph{negative predictive values} \code{\link{NPV}} (i.e., the conditional probability that
#' the condition is FALSE, provided that the decision is negative).
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
#' pal["hi"] <- "gold"  # defines a new color for hits (true positives, TP)
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


## pal_org: Keep a copy of pal in pal_org: --------

#' Original color palette.
#'
#' \code{pal_org} is a copy of \code{\link{pal}}
#' (to retrieve original set of colors in case
#' \code{\link{pal}} is changed).
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_org} to re-set default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_org        # shows all current color names and values
#' pal_org["hi"]  # shows the current color for hits (true positives)
#' pal_org["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_org <- pal


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
#' pal_bw["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_bw <- init_pal(N_col = grey(.95, .99),     # nearly white
                   cond.true_col =  grey(.90, .99), # darker white
                   cond.false_col = grey(.80, .99), # darker white
                   dec.pos_col = grey(.85, .99),   # darker white
                   dec.neg_col = grey(.70, .99),   # darker white
                   dec.cor_col = grey(.75, .99),   # darker white
                   dec.err_col = grey(.60, .99),   # darker white
                   hi_col = grey(.80, .99),    # brighter 1
                   mi_col = grey(.60, .99),    # darker 1
                   fa_col = grey(.50, .99),    # darker 2
                   cr_col = grey(.70, .99),    # brighter 2
                   PPV_col = grey(.60, .99),   # medium grey
                   NPV_col = grey(.45, .99),   # darker grey
                   txt_col = grey(0, .99),     # black
                   brd_col = grey(.10, .99)    # almost black
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
#' pal_4c["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_4c <- init_pal(N_col = grey(.95, .99),     # nearly white
                   cond.true_col =  grey(.90, .99), # darker white
                   cond.false_col = grey(.75, .99), # lighter white
                   dec.pos_col = grey(.85, .99),   # lighter grey
                   dec.neg_col = grey(.70, .99),   # darker grey
                   dec.cor_col = grey(.75, .99),   # lighter grey
                   dec.err_col = grey(.60, .99),   # darker grey
                   # hi_col = pal["hi"],         # from pal
                   # mi_col = pal["mi"],         # from pal
                   # fa_col = pal["fa"],         # from pal
                   # cr_col = pal["cr"],         # from pal
                   PPV_col = grey(.60, .99),   # medium grey
                   NPV_col = grey(.45, .99),   # darker grey
                   txt_col = grey(0, .99),     # black
                   brd_col = grey(.10, .99)    # almost black
)

## Check:
# pal_4c
# pal_4c["N"]  # => "#D9D9D9FC"

## Use reduced color scheme (as default):
# pal <- pal_4c

## Note: Check http://colorbrewer2.org for alternatives:
## http://colorbrewer2.org/?type=qualitative&scheme=Paired&n=4

## pal_gbs: Define a gbs (green/blue/sand) color palette: --------

#' Alternative color palette with gbs (green/blue/sand) colors.
#'
#' \code{pal_gbs} is initialized to a vector of named elements (colors)
#' to define an alternative gbs (green/blue/sand) scenario color scheme.
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_gbs} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_gbs        # shows all current color names and values
#' pal_gbs["hi"]  # shows the current color for hits (true positives)
#' pal_gbs["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_gbs <- init_pal(N_col = grey(.90, .99),  # "wheat3", nearly white
                    cond.true_col =  grey(.80, .99), # col_sand_light, # "wheat3", #
                    cond.false_col = grey(.60, .99), # col_sand_mid, # "wheat4"
                    dec.pos_col = col_grey_1,
                    dec.neg_col = col_grey_2,
                    dec.cor_col = my_green,
                    dec.err_col = my_blue,
                    hi_col = col_green_1,
                    mi_col = col_blue_1,
                    fa_col = col_blue_3,
                    cr_col = col_green_2,
                    PPV_col = col_orange_2,
                    NPV_col = col_blue_3,
                    txt_col = grey(0, .99),  # black
                    brd_col = col_sand_dark
)

## Check:
# pal_gbs
# pal_gbs["hi"]  # => "#B8D989"

## Use gbs color scheme (as default):
pal <- pal_gbs

## pal_kn: Define a uni.kn palette: --------

#' Alternative color palette for uni.kn.
#'
#' \code{pal_kn} is initialized to a vector of named elements (colors)
#' to define an alternative (uni.kn) scenario color scheme.
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_kn} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_kn      # shows all current color names and values
#' pal_kn["hi"]  # shows the current color for hits (true positives)
#' pal_kn["hi"] <- "grey" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_kn <- init_pal(N_col = grey(.95, .99),     # nearly white
                   cond.true_col =  grey(.70, .99), # darker white
                   cond.false_col = grey(.85, .99), # darker white
                   dec.pos_col = grey(.60, .99),   # darker white
                   dec.neg_col = grey(.75, .99),   # darker white
                   dec.cor_col = grey(.50, .99),   # grey
                   dec.err_col = grey(.65, .99),   # darker white
                   hi_col = pal_uni_kn$seeblau4,
                   mi_col = pal_uni_kn$seeblau2,
                   fa_col = pal_uni_kn$seeblau1,
                   cr_col = pal_uni_kn$seeblau3,
                   PPV_col = col_orange_2,
                   NPV_col = pal_uni_kn$seeblau4,
                   txt_col = grey(0, .99),     # black
                   brd_col = grey(.10, .99)    # almost black
)

## Check:
# pal_kn
# pal_kn["hi"]

## Use uni.kn color scheme (as default):
# pal <- pal_kn


## pal_vir: Define a viridis color scale: ----------

## Using viridisLite (version 0.3.0):
## URL: https://github.com/sjmgarnier/viridisLite
## Maintainer: Simon Garnier <garnier@njit.edu>

# viridisLite::viridis(4)
vir_04 <- c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")

# viridisLite::viridis(8)
vir_08 <- c("#440154FF", "#46337EFF", "#365C8DFF", "#277F8EFF",
            "#1FA187FF", "#4AC16DFF", "#9FDA3AFF", "#FDE725FF")

# viridisLite::viridis(12)
vir_12 <- c("#440154FF", "#482173FF", "#433E85FF", "#38598CFF",
            "#2D708EFF", "#25858EFF", "#1E9B8AFF", "#2BB07FFF",
            "#51C56AFF", "#85D54AFF", "#C2DF23FF", "#FDE725FF")

#' Alternative color palette using viridis colors.
#'
#' \code{pal_vir} is initialized to a vector of named elements (colors)
#' to define a scenario color scheme modeled on the \code{viridis} color scale.
#'
#' These colors are select by the Matplotlib \code{viridis} color map
#' created by Stéfan van der Walt and Nathaniel Smith.
#' See the \code{viridisLite} package (maintained by Simon Garnier)
#' for further information.
#'
#' Assign \code{pal <- pal_vir} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_vir        # shows all current color names and values
#' pal_vir["hi"]  # shows the current color for hits (true positives)
#' pal_vir["hi"] <- "green3" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_vir <- init_pal(N_col = grey(.70, .99),     # mid-grey
                    cond.true_col =  grey(.85, .99), # brighter by 15
                    cond.false_col = grey(.55, .99), # darker
                    dec.pos_col = grey(.80, .99),   # brighter by 10
                    dec.neg_col = grey(.60, .99),   # darker
                    dec.cor_col = grey(.90, .99),   # brighter by 20
                    dec.err_col = grey(.50, .99),   # darker
                    hi_col = vir_12[10],        # hi: bright green
                    mi_col = vir_12[12],        # mi: yellow
                    fa_col = vir_12[5],         # fa: bluish
                    cr_col = vir_12[8],         # cr: darker green
                    PPV_col = vir_12[9],        # between hi and cr
                    NPV_col = vir_12[3],        # beyond fa
                    txt_col = grey(0, .99),     # black
                    brd_col = grey(.10, .99)    # almost black
)

## Check:
# pal_vir
# pal_vir["hi"]

## Use viridis color scheme (as default):
# pal <- pal_vir

## (*) Done: ----------

## - Clean up code.  [2018 09 01]
## - Colors are user-customizable
## - Add some pre-defined color palettes.

## (+) ToDo: ----------

## - Add 2x2-dimensional (+/-, T/F) color palette.
## - Add dark (inverted) color palette (bright text on dark boxes).
## - Add RGB, true b+w, and corrected palettes (for color-blind persons).
## - Add b/w-color high-lighting palettes (e.g., for 3 perspectives).

## eof. ------------------------------------------

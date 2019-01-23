## init_pal.R | riskyr
## 2019 01 23
## Define custom color palettes (pal):
## -----------------------------------------------
## pal contains defaults for user inputs.

## (A) Define colors: ----------

## (1) Select and name some colors: ---------

## (1) from uni.kn:
seeblau <- rgb(0, 169, 224, max = 255)  # seeblau.4 (non-transparent)

pal_uni_kn = data.frame( ## in one df:
  "seeblau1" = rgb(204, 238, 249, maxColorValue = 255),  #  1. seeblau1 (non-transparent)
  "seeblau2" = rgb(166, 225, 244, maxColorValue = 255),  #  2. seeblau2 (non-transparent)
  "seeblau3" = rgb( 89, 199, 235, maxColorValue = 255),  #  3. seeblau3 (non-transparent)
  "seeblau4" = rgb(  0, 169, 224, maxColorValue = 255),  #  4. seeblau4 (= seeblau base color)
  "black"    = rgb(  0,   0,   0, maxColorValue = 255),  #  5. black
  "seegrau4" = rgb(102, 102, 102, maxColorValue = 255),  #  6. grey40 (non-transparent)
  "seegrau3" = rgb(153, 153, 153, maxColorValue = 255),  #  7. grey60 (non-transparent)
  "seegrau2" = rgb(204, 204, 204, maxColorValue = 255),  #  8. grey80 (non-transparent)
  "seegrau1" = rgb(229, 229, 229, maxColorValue = 255),  #  9. grey90 (non-transparent)
  "white"    = rgb(255, 255, 255, maxColorValue = 255),  # 10. white
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

green_1 <- make_transparent(my_green, alpha = .50)
green_2 <- make_transparent(my_green, alpha = 1.0)

red_1 <- make_transparent(my_red, alpha = .50)
red_2 <- make_transparent(my_red, alpha = 1.0)

blue_1 <- make_transparent(my_blue, alpha = .50)
blue_2 <- make_transparent(my_blue, alpha = 1.0)

yellow_1 <- make_transparent(my_yellow, alpha = .50)
yellow_2 <- make_transparent(my_yellow, alpha = 1.0)

orange_1 <- make_transparent(my_orange, alpha = .50)
orange_2 <- make_transparent(my_orange, alpha = 1.0)

my_white <- "white"
my_grey  <- "grey75"
my_black <- "black"

my_whitish <- "antiquewhite" # "whitesmoke"
my_bluish <- "aliceblue"


## (2) Assign colors to recurring elements/roles ----------
##     (to set default colors for plots and app display):

## (a) Define base color (for population N):

N_col <- grey(.90, .99)  # "white", col_grey_1

## (b) by condition: Define 2 colors for condition cases:

cond_true_col  <- my_yellow        # "lightgoldenrod1" "gold1", col_orange_1, "yellow2"
cond_false_col <- "lightskyblue2"  #, my_blue, "deepskyblue1", "lightskyblue2", col_blue_1

## Combine in a named vector:
cond_colors <- setNames(c(cond_true_col, cond_false_col),
                        c("cond_true", "cond_false"))

## (c) by decision: Define 3 colors for decision cases:

dec_pos_col <- "rosybrown3" # "khaki", my_whitish
dec_neg_col <- "lightsteelblue3" # "lightsteelblue2", "wheat", "lightsteelblue1", my_bluish, "chartreuse4"

## Combine in a named vector:
dec_colors <- setNames(c(dec_pos_col, dec_neg_col),
                       c("dec_pos", "dec_neg"))

## (d) Accuracy colors:

dec_cor_col <- "palegreen3" # "forestgreen" # correct/accurate decisions
dec_err_col <- "firebrick3" # erroneous/inaccurate decisions

acc_colors <- setNames(c(dec_cor_col, dec_err_col),
                       c("dec_cor", "dec_err"))

## (e) by condition x decision: Define 4 colors for SDT cases:

hi_col <- my_green        # "olivedrab4", "palegreen4", col_green_2, "chartreuse4"
mi_col <- my_red          # "tomato3", "orangered3", "firebrick3", col_red_2
fa_col <- "lightsalmon2"  # lightcoral", "tomato1", "orangered1", "firebrick1", col_red_1
cr_col <- "olivedrab3"    # "springgreen2", "palegreen3", col_green_1

## Combine in a named vector:
sdt.colors <- setNames(c(hi_col, mi_col, fa_col, cr_col),
                       c("hi", "mi", "fa", "cr")
)

## (f) Define 2 colors for main PVs:

PPV_col <- my_orange  # "sienna1" # col_orange_2 # "orange3" "firebrick" "red3"
NPV_col <- my_blue    # "steelblue3", col_blue_3, "green4" "gray50" "brown4"

## (g) Other colors:

## Text and borders:

txt_col <- grey(.01, .99)  # near "black" text labels, NA removes text?
brd_col <- grey(.20, .99)  # greyish borders, NA removes borders

## Plot background:

bg_col <- "white"  # background color, NA creates transparent background [set ONLY here]


## Probability lines (within Mosaic/area plots):
# prev.li <- "gold"       # "aliceblue"
# sens.li <- "cornsilk"   # "darkseagreen1" "olivedrab1"
# spec.li <- "whitesmoke" # "lemonchiffon" "antiquewhite"

# col_p <- c(prev.li, sens.li, spec.li)  # 3 bright colors (visible on SDT rectangles)


## (3) Define corresponding default palette: ----------

# vector of colors:
pal_def <- c(N_col,
             cond_colors, dec_colors, acc_colors,
             sdt.colors,
             PPV_col, NPV_col,
             txt_col, brd_col,
             bg_col)

# names of colors:
pal_def <- setNames(object = pal_def,
                    nm = c("N",
                           names(cond_colors), names(dec_colors), names(acc_colors),
                           names(sdt.colors),
                           "ppv", "npv",
                           "txt", "brd", "bg")
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
#' @param cond_true_col Color representing cases of \code{\link{cond_true}}, for which the current condition is \code{TRUE}.
#' @param cond_false_col Color representing cases of in \code{\link{cond_false}}, for which the current condition is \code{FALSE}.
#'
#' @param dec_pos_col Color representing cases of \code{\link{dec_pos}}, for which the current decision is \code{positive}.
#' @param dec_neg_col Color representing cases in \code{\link{dec_neg}}, for which the current decision is \code{negative}.
#'
#' @param dec_cor_col Color representing cases of correct decisions \code{\link{dec_cor}}, for which the current decision is \code{accurate}.
#' @param dec_err_col Color representing cases in erroneous decisions \code{\link{dec_err}}, for which the current decision is \code{inaccurate}.
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
#' @param bg_col  Background color of plot (used to set \code{par(bg = bg_col)}).
#'
#' @examples
#' init_pal()          # => define and return a vector of current (default) colors
#' length(init_pal())  # => 15 named colors
#' pal <- init_pal(N_col = "steelblue4")  # => change a color (stored in pal)
#' pal <- init_pal(brd_col = NA)          # => remove a color
#'
#' @family functions initializing scenario information
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
                     cond_true_col  = pal_def["cond_true"],   # condition true
                     cond_false_col = pal_def["cond_false"],  # condition false
                     ## by decision:
                     dec_pos_col  = pal_def["dec_pos"],  # decision positive
                     dec_neg_col = pal_def["dec_neg"],   # decision negative
                     ## by accuracy:
                     dec_cor_col  = pal_def["dec_cor"],  # decision correct / accurate
                     dec_err_col = pal_def["dec_err"],   # decision erroneous / inaccurate
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
                     brd_col = pal_def["brd"],      # borders
                     bg_col = pal_def["bg"]         # background
) {

  ## 1. Initialize pal as a VECTOR:
  pal <- rep(NA, n_colors)

  ## 2. Pass arguments to VECTOR:
  pal <- c(N_col,      # population N
           ## by condition:
           cond_true_col,   # condition true
           cond_false_col,  # condition false
           ## by decision:
           dec_pos_col,    # decision positive
           dec_neg_col,    # decision negative
           ## by accuracy:
           dec_cor_col,    # decision correct / accurate
           dec_err_col,    # decision erroneous / inaccurate
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
           brd_col,    # borders
           ## Background:
           bg_col
  )

  ## 3. Name vector elements:
  pal <- setNames(object = pal,
                  nm = c("N",
                         names(cond_colors), names(dec_colors), names(acc_colors),
                         names(sdt.colors),
                         "ppv", "npv", "txt", "brd", "bg")
  )

  ## 4. Return vector:
  return(pal)

}

## Check:

# init_pal()          # => define and return a vector of current (default) colors
# length(init_pal())  # => 16 named colors
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
#' \item \code{cond_true} Color representing cases of \code{\link{cond_true}}, for which the current condition is \code{TRUE}.
#'
#' \item \code{cond_false} Color representing cases of in \code{\link{cond_false}}, for which the current condition is \code{FALSE}.
#'
#' \item \code{dec_pos} Color representing cases of \code{\link{dec_pos}}, for which the current decision is \code{positive}.
#'
#' \item \code{dec_neg} Color representing cases in \code{\link{dec_neg}}, for which the current decision is \code{negative}.
#'
#' \item \code{dec_cor} Color representing cases of correct decisions \code{\link{dec_cor}}, for which the current decision is \code{accurate}.
#'
#' \item \code{dec_err} Color representing cases of erroneous decisions \code{\link{dec_err}}, for which the current decision is \code{inaccurate}.
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
#' \item \code{bg} Color used for plot background (used to set \code{par(bg = bg_col)}).
#'
#' }
#'
#' Note that color names for frequencies correspond to frequency names,
#' but are different for probabilities (which are written in lowercase
#' and only \code{\link{PPV}} and \code{\link{NPV}} have assigned colors).
#'
#' @examples
#' pal        # shows all color names and current values
#' pal["hi"]  # shows the current color for hits (true positives, TP)
#' pal["hi"] <- "gold"  # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
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


## pal_org: A copy of original color palette: --------

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
#' pal_org        # shows all color names and current values
#' pal_org["hi"]  # shows the current color for hits (true positives, TP)
#' pal_org["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_org <- pal  # copy pal


## pal_bw:  A black-and-white (b/w/grey) color palette: --------

#' Alternative color palette for black-and-white (greyscale) graphs.
#'
#' \code{pal_bw} is initialized to a vector of named elements (colors)
#' to define an alternative (black-and-white, b/w) scenario color scheme.
#'
#' Note that \code{pal_bw} uses various shades of grey for frequency boxes
#' so that their bounds remain visible on a white background
#' when \code{f_lwd = 0} (as per default for most graphs).
#'
#' See \code{\link{pal_bwp}} for a stricter version that enforces
#' black text and lines on white boxes (e.g., for printing purposes).
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_bw} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_bw        # shows all color names and current values
#' pal_bw["hi"]  # shows the current color for hits (true positives, TP)
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
                   cond_true_col =  grey(.90, .99), # darker white
                   cond_false_col = grey(.80, .99), # darker white
                   dec_pos_col = grey(.85, .99),  # darker white
                   dec_neg_col = grey(.70, .99),  # darker white
                   dec_cor_col = grey(.75, .99),  # darker white
                   dec_err_col = grey(.60, .99),  # darker white
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


## pal_bwp:  A strict black-and-white (b/w) color palette for printing purposes: --------

#' Alternative color palette for black-and-white graphs (for printing purposes).
#'
#' \code{pal_bwp} is initialized to a vector of named elements (colors)
#' to define a strict (black-and-white, b/w) scenario color scheme
#' that is suited for printing graphs in black-and-white.
#'
#' \code{pal_bwp} is a stricter version of the greyscale
#' palette \code{\link{pal_bw}} that enforces
#' black text and lines on white boxes. Thus, the bounds of frequency boxes
#' are invisible on white backgrounds unless the default of
#' \code{f_lwd = 0} is changed (e.g., to \code{f_lwd = 1}).
#'
#' Some background colors (of frequencies) are also used as
#' foreground colors (of probabilities, e.g.,
#' in \code{\link{plot_curve}} and \code{\link{plot_plane}}).
#' For this reason, the plotting functions detect and
#' adjust colors and/or line settings when \code{pal_bwp}
#' is used.
#'
#' See \code{\link{pal_bw}} for a more permissible black-and-white
#' palette that uses various shades of grey for frequency boxes
#' so that their bounds remain visible on a white background
#' when \code{f_lwd = 0} (as per default for most graphs).
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_bwp} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_bwp        # shows all color names and current values
#' pal_bwp["hi"]  # shows the current color for hits (true positives, TP)
#' pal_bwp["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_bwp <- init_pal(N_col = my_white,          # grey(.95, .99), # nearly white
                    cond_true_col =  my_white,  # grey(.90, .99), # darker white
                    cond_false_col = my_white,  # grey(.80, .99), # darker white
                    dec_pos_col = my_white,  # grey(.85, .99),  # darker white
                    dec_neg_col = my_white,  # grey(.70, .99),  # darker white
                    dec_cor_col = my_white,  # grey(.75, .99),  # darker white
                    dec_err_col = my_white,  # grey(.60, .99),  # darker white
                    hi_col = my_white,  # grey(.80, .99),    # brighter 1
                    mi_col = my_white,  # grey(.60, .99),    # darker 1
                    fa_col = my_white,  # grey(.50, .99),    # darker 2
                    cr_col = my_white,  # grey(.70, .99),    # brighter 2
                    PPV_col = my_black,  # grey(.60, .99),   # medium grey
                    NPV_col = my_black,  # grey(.45, .99),   # darker grey
                    txt_col = my_black,  # grey(0, .99),     # black
                    brd_col = my_black   # grey(.10, .99)    # almost black
)

## Check:
# pal_bwp
# pal_bwp["hi"]


## pal_rgb: A reduced RGB color palette: --------

#' Alternative color palette for graphs (with RGB colors).
#'
#' \code{pal_rgb} is initialized to a vector of named elements (colors)
#' to define an alternative (reduced) scenario color scheme
#' (using red, green, and blue colors).
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_rgb} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_rgb        # shows all color names and current values
#' pal_rgb["hi"]  # shows the current color for hits (true positives, TP)
#' pal_rgb["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_rgb <- init_pal(N_col = grey(.95, .99),     # nearly white
                    cond_true_col =  grey(.90, .99), # darker white
                    cond_false_col = grey(.75, .99), # lighter white
                    dec_pos_col = grey(.85, .99),   # lighter grey
                    dec_neg_col = grey(.70, .99),   # darker grey
                    dec_cor_col = grey(.75, .99),   # lighter grey
                    dec_err_col = grey(.60, .99),   # darker grey
                    # hi_col = pal["hi"],         # from pal
                    # mi_col = pal["mi"],         # from pal
                    # fa_col = pal["fa"],         # from pal
                    # cr_col = pal["cr"],         # from pal
                    # PPV_col = grey(.60, .99),   # medium grey
                    # NPV_col = grey(.45, .99),   # darker grey
                    txt_col = grey(0, .99),     # black
                    brd_col = grey(.10, .99)    # almost black
)

## Check:
# pal_rgb
# pal_rgb["N"]  # => "#D9D9D9FC"

## Use reduced RGB color scheme (as default):
# pal <- pal_rgb

## Note: Check http://colorbrewer2.org for alternatives:
## http://colorbrewer2.org/?type=qualitative&scheme=Paired&n=4

## pal_mod: A modern (green/blue/orange) color palette: --------

#' Modern color palette (in green/blue/orange).
#'
#' \code{pal_mod} is initialized to a vector of named colors
#' to define a modern scenario color scheme (in green/blue/orange).
#'
#' See \code{\link{pal}} for default color information.
#'
#' Assign \code{pal <- pal_mod} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_mod        # shows all color names and current values
#' pal_mod["hi"]  # shows the current color for hits (true positives, TP)
#' pal_mod["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information.
#'
#' @export

pal_mod <- init_pal(N_col = grey(.90, .99),  # "wheat3", nearly white
                    cond_true_col = my_yellow, # col_sand_mid, # grey(.80, .99), # "wheat3", #
                    cond_false_col = grey(.75, .99), # col_sand_mid, # "wheat4"
                    dec_pos_col = col_orange_1, # my_orange, # col_red_1, # grey(.85, .99), # col_grey_1,
                    dec_neg_col = grey(.65, .99), # col_grey_2,
                    dec_cor_col = my_green,
                    dec_err_col = my_blue,
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
# pal_mod
# pal_mod["hi"]  # => "#B8D989"

## Use pal_mod color scheme (as default):
# pal <- pal_mod

## pal_mbw: A reduced modern (green/blue/bw) color palette: --------

#' Modern and reduced color palette (in green/blue/bw).
#'
#' \code{pal_mod} is initialized to a vector of named colors
#' to define a reduced modern scenario color scheme (in green/blue/bw).
#'
#' See \code{\link{pal_org}} for original color information;
#' \code{\link{pal_mod}} for a richer modern color palette; and
#' \code{\link{pal_bw}} for a more reduced black-and-white color palette.
#'
#' Assign \code{pal <- pal_mbw} to use as default color scheme
#' throughout the \code{riskyr} package.
#'
#' @examples
#' pal_mbw        # shows all color names and current values
#' pal_mbw["hi"]  # shows the current color for hits (true positives, TP)
#' pal_mbw["hi"] <- "gold" # defines a new color for hits (true positives, TP)
#'
#' @family lists containing current scenario information
#'
#' @seealso
#' \code{\link{pal}} contains current color information;
#' \code{\link{init_pal}} initializes color information;
#' \code{\link{pal_org}} for original color palette;
#' \code{\link{pal_mod}} for a richer modern color palette;
#' \code{\link{pal_bw}} for a more reduced black-and-white color palette.
#'
#' @export

pal_mbw <- init_pal(N_col = grey(.90, .99),  # "wheat3", nearly white
                    cond_true_col =  grey(.80, .99), # my_yellow, # col_sand_mid, # # "wheat3", #
                    cond_false_col = grey(.75, .99), # col_sand_mid, # "wheat4"
                    dec_pos_col =    grey(.85, .99), # col_orange_1, # my_orange, # col_red_1, # col_grey_1,
                    dec_neg_col =    grey(.65, .99), # col_grey_2,
                    dec_cor_col = my_green,
                    dec_err_col = my_blue,
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
# pal_mbw
# pal_mbw["cond_true"]  # => "#CCCCCCFC"

## Use pal_mbw color scheme (as default):
# pal <- pal_mbw


## pal_kn:  A uni.kn palette: --------

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
#' pal_kn      # shows all color names and current values
#' pal_kn["hi"]  # shows the current color for hits (true positives, TP)
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
                   cond_true_col =  grey(.70, .99), # darker white
                   cond_false_col = grey(.85, .99), # darker white
                   dec_pos_col = grey(.60, .99),   # darker white
                   dec_neg_col = grey(.75, .99),   # darker white
                   dec_cor_col = grey(.50, .99),   # grey
                   dec_err_col = grey(.65, .99),   # darker white
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


## pal_vir: A viridis color palette: ----------

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
#' pal_vir        # shows all color names and current values
#' pal_vir["hi"]  # shows the current color for hits (true positives, TP)
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
                    cond_true_col =  grey(.85, .99), # brighter by 15
                    cond_false_col = grey(.55, .99), # darker
                    dec_pos_col = grey(.80, .99),   # brighter by 10
                    dec_neg_col = grey(.60, .99),   # darker
                    dec_cor_col = grey(.90, .99),   # brighter by 20
                    dec_err_col = grey(.50, .99),   # darker
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

## Set default color palette: -------

## Use pal_mbw by default (riskyr 0.2.0):
# pal <- pal_mbw

## Use pal_mod by default (riskyr 0.2.0.9000+):
pal <- pal_mod

## Test:
# pal <- pal_bwp

## (*) Done: ----------

## - Clean up code.        [2018 09 01]
## - Add some pre-defined color palettes.

## - Addressed limitation: [2019 01 23]
##   Lack of an explicit background color (e.g., "white" or NA by default).
##   Solution implemented:
#    1. bg_col <- "white"           # in all color palettes
#    2. par(bg = col_pal[["bg"]])   # in all plot functions

## (+) ToDo: ----------

## - Lack of most probability colors:
##   Only PPV and NPV are assigned dedicated colors.
##   Plotting ppod and acc in plot_curve and plot_plane recruits frequency colors.
##   This can lead to conflicts (e.g., with pal_bwp when par("bg") == "white").

## - Lack of ability to invert boxes (e.g., text bright, fill color dark).

## ToDos:

## - Add more 2x2-dimensional (+/-, T/F) color palettes.
## - Add dark (inverted) color palette (bright text on dark boxes).

## eof. ------------------------------------------

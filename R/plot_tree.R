## plot_tree.R | riskyR
## 2018 01 22
## -----------------------------------------------
## Plot a tree diagram of natural frequencies
## -----------------------------------------------
## 4 different versions of plot_tree():

## 1. Basic nftree (area = "no"):
##    Default box sizes (identical areas) carry no meaning.

## 2. Sum tree (area = "sq"):
##    Frequencies as squares: Areas of each level add up to N.

## 3. Area tree (area = "hr" and "vr"):
##    - Constraint: Areas on each level must sum to area of N
##      But levels 2 and 3 contain rectangles that visually add up to area on next higher level:
##    - Condition areas (on level 2): 2 rectangles that add up to the population square
##    - SDT areas (on level 3): 4 rectangles that correspond to both the condition areas (level 2) and population area (level 1)
##      Note that "hr" dissects the condition areas more clearly, whereas "vr" corresponds to the rectangles in the mosaic plot.

## -----------------------------------------------
## Dependencies:

# library("diagram") # moved to "Imports:" in in DESCRIPTION!

## -----------------------------------------------
## plot_tree: Plot tree diagram of natural frequencies
## (using only necessary arguments with good defaults):

## Assuming that freq$N (+ num txt pal) is known!

#' Plot a tree diagram of natural frequencies.
#'
#' \code{plot_tree} computes and visualizes a tree diagram of
#' natural frequencies (typically rounded integers) given basic
#' probabilities -- \code{\link{prev}} and \code{\link{sens}},
#' and \code{\link{spec}} or \code{\link{fart}} (\code{spec = 1 - fart})
#' -- and a population size of \code{\link{N}} individuals.
#'
#' The option \code{area} (as 2 characters) allows specifying
#' 4 different box shapes and sizes:
#'
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
#' \code{plot_tree} requires and uses the R package "diagram"
#' (\code{library("diagram")}).
#'
#' @param prev The condition's prevalence value \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#' @param sens The decision's sensitivity value \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' @param spec The decision's specificity value \code{\link{spec}}
#' (i.e., the conditional probability
#' of a negative decision provided that the condition is \code{FALSE}).
#' \code{spec} is optional when is complement \code{fart} is provided.
#' @param fart The decision's false alarm rate \code{\link{fart}}
#' (i.e., the conditional probability
#' of a positive decision provided that the condition is \code{FALSE}).
#' \code{fart} is optional when its complement \code{spec} is provided.
#'
#' @param N The number of individuals in the population:
#' a suitable value of \code{\link{N}} is computed, if not provided.
#'
#' @param area  A character option for 4 different box sizes:
#' - \code{"no"} ... all boxes are shown with the same size (default);
#' - \code{"sq"} ... boxes as squares of area sizes proportional to frequencies;
#' - \code{"hr"} ... boxes as horizontal rectangles of area sizes proportional to frequencies;
#' - \code{"vr"} ... boxes as vertical rectangles of area sizes proportional to frequencies.
#'
#' @param round A Boolean option that determines whether frequencies are
#' rounded to the nearest integer. Default: \code{round = TRUE}.
#'
#' @param title.lbl Text label to set plot title.
#'
#' @param col.boxes Colors of boxes (a single color or vector of 7 colors).
#'
#' @return Nothing (NULL).
#'
#' @examples
#' # Plotting existing freq:
#' plot_tree()
#' plot_tree(title.lbl = "")
#' plot_tree(N = 33)
#' plot_tree(N = NA)
#'
#' ## Computing and plotting new frequencies:
#' plot_tree(prev = 1/3)
#' plot_tree(prev = 1/3, N = 55)
#' plot_tree(prev = 1/3, N = NA)
#' plot_tree(prev = 1/3, round = FALSE)
#' plot_tree(prev = .10, sens = .90, spec = 1/3, N = 100)
#' plot_tree(prev = .10, sens = .90, spec = NA, fart = 1/3, N = 33)
#' plot_tree(prev = .10, sens = .90, spec = 1/3, fart = NA, N = NA)
#' plot_tree(prev = .10, sens = .90, spec = NA, fart = 1/3, N = NA)
#'
#' ## Area trees:
#' plot_tree(area = "sq")
#' plot_tree(area = "sq", round = FALSE)
#' plot_tree(area = "hr")
#' plot_tree(area = "vr", round = FALSE)
#'
#' ## Custom colors and shadows:
#' plot_tree(prev = .08, sens = .92, spec = .95, N = 10000, area = "hr")
#' plot_tree(area = "sq", col.boxes = "gold", col.border = "steelblue4", col.shadow = "steelblue4", cex.shadow = .008)
#' plot_tree(N = NA, area = "vr", col.txt = "steelblue4", col.boxes = "lightyellow", col.border = grey(.3, .7), cex.shadow = .008, col.shadow = grey(.1, .9))
#'
#' @family visualization functions
#'
#' @seealso
#' \code{\link{num}} for basic numeric parameters;
#' \code{\link{init_num}} initializes basic numeric variables;
#' \code{\link{freq}} contains current frequency information;
#' \code{\link{comp_freq}} computes frequencies from probabilities;
#' \code{\link{prob}} contains current probability information;
#' \code{\link{comp_prob}} computes current probability information;
#' \code{\link{pal}} for current color settings;
#' \code{\link{txt}} for current text settings;
#' \code{\link{comp_min_N}} computes a suitable population size \code{\link{N}} (if missing)

plot_tree <- function(prev = num$prev,  # probabilities
                      sens = num$sens,
                      spec = num$spec,
                      fart = NA,        # was: num$fart,
                      N = freq$N,       # only freq used (so far)
                      ## Options:
                      area = "no",      # "no"...none (default), "sq"...square, "hr"...horizontal rectangles, "vr"...vertical rectangles
                      round = TRUE,     # round freq (if computed)
                      ## Labels:
                      title.lbl = txt$scen.lbl, # custom labels
                      popu.lbl = txt$popu.lbl,
                      cond.lbl = txt$cond.lbl,  # condition labels
                      cond.true.lbl = txt$cond.true.lbl,
                      cond.false.lbl = txt$cond.false.lbl,
                      dec.lbl = txt$dec.lbl,    # decision labels
                      dec.true.lbl = txt$dec.true.lbl,
                      dec.false.lbl = txt$dec.false.lbl,
                      sdt.hi.lbl = txt$sdt.hi.lbl, # SDT combinations
                      sdt.mi.lbl = txt$sdt.mi.lbl,
                      sdt.fa.lbl = txt$sdt.fa.lbl,
                      sdt.cr.lbl = txt$sdt.cr.lbl,
                      ## Colors:
                      col.boxes = pal[1:7],
                      # col.N = col.sand.light,
                      # col.true = col.N, col.false = col.N,
                      # col.hi = pal["hi"], col.mi = pal["mi"], col.fa = pal["fa"], col.cr = pal["cr"],
                      col.txt = grey(.01, alpha = .99), # black
                      col.border = col.grey.4,
                      ## Shadows:
                      col.shadow = col.sand.dark,
                      cex.shadow = 0 # [allow using shadows]
                      ){

  ## (1) Compute or gather natural frequencies:
  if (is_valid(prev, sens, spec, fart)) { # probabilities are provided:

    ## (a) Compute cur.freq based on current parameters (N and probabilities):
    cur.freq <- comp_freq(prev, sens, spec, fart, N, round)  # compute freq

    ## Assign elements of cur.freq:
    N <- cur.freq$N
    n.true  <- cur.freq$cond.true
    n.true  <- cur.freq$cond.true
    n.false <- cur.freq$cond.false
    n.hi <- cur.freq$hi
    n.mi <- cur.freq$mi
    n.fa <- cur.freq$fa
    n.cr <- cur.freq$cr

    ## Compute missing fart or spec (4th argument) value (if applicable):
    cur.spec.fart <- comp_comp_pair(spec, fart)
    spec <- cur.spec.fart[1] # 1st argument
    fart <- cur.spec.fart[2] # 2nd argument

  } else { # prev is NA:

    ## (b) Plot current values of freq:
    N <- freq$N
    n.true  <- freq$cond.true
    n.true  <- freq$cond.true
    n.false <- freq$cond.false
    n.hi <- freq$hi
    n.mi <- freq$mi
    n.fa <- freq$fa
    n.cr <- freq$cr

  }

  ## (2) Text/labels in 7 boxes:                      # NOT used yet:
  ## Defaults:
  names <- c(paste0("Population", ":\n", "N = ", N),  # popu.lbl
             paste0(cond.true.lbl, ":\n",  round(n.true, 2)),
             paste0(cond.false.lbl, ":\n", round(n.false, 2)),
             paste0(sdt.hi.lbl, ":\n", round(n.hi, 2)),
             paste0(sdt.mi.lbl, ":\n", round(n.mi, 2)),
             paste0(sdt.fa.lbl, ":\n", round(n.fa, 2)),
             paste0(sdt.cr.lbl, ":\n", round(n.cr, 2))
             )

  ## Alternative: Shorter labels
  if (area != "no") {
    ## Reduced names (as areas get quite small):
    names <- c(paste0("N = ", N),  # popu.lbl
               paste0("true:\n",  round(n.true, 2)),
               paste0("false:\n", round(n.false, 2)),
               paste0("hi:\n", round(n.hi, 2)),
               paste0("mi:\n", round(n.mi, 2)),
               paste0("fa:\n", round(n.fa, 2)),
               paste0("cr:\n", round(n.cr, 2))
               )
  }

  ## (3) Make matrix M:
  M <- matrix(nrow = 7, ncol = 8, byrow = TRUE, data = 0)

  ## (4) Arrow/edge labels:
  ## ToDo: Use more informative arrow/edge labels:
  prev.lbl <- paste0("prev = ", as_pc(prev), "%")

  ## Defaults:
  M[2, 1] <- "prevalence"  # ERROR: WHY does prev.lbl not work with spaces???
  M[3, 1] <- "(1 - prev)"  # "(N - n.true)"
  M[4, 2] <- "sensitivity"
  M[5, 2] <- "(1 - sens)"  # "n(true - hi)"  # mirt = missing rate
  M[6, 3] <- "(1 - spec)"  # "n(false - cr)" # fart = false alarm rate
  M[7, 3] <- "specificity"

  ## Alternative: Shorter labels
  if (area != "no") {
    ## Reduced arrow/edge labels:
    M[2, 1] <- "prev"  # ERROR: WHY does prev.lbl not work with spaces???
    M[3, 1] <- "(1 - prev)"    # "n(N - true)"
    M[4, 2] <- "sens"
    M[5, 2] <- "(1 - sens)"    # mirt = "n(true - hi)"
    M[6, 3] <- "(1 - spec)"    # fart = "n(false - cr)"
    M[7, 3] <- "spec"
  }

  ## (5) Distinguish between 4 different plot types (based on area setting):
  ## 5a. Default case: Rectangles of same width and height (non-proportional)
  if (area == "no") {

    x.pop <- .11   # basic width of population box
    x.y.pop <- 2/3 # basic proportion is rectangular (width > height)

    ## Collect all sizes and proportions:
    x.boxes <- rep(x.pop, 7)    # all boxes have the same width
    x.y.prop <- rep(x.y.pop, 7) # all boxes have the same proportion
  }

  ## 5b. Squares that sum to the area of the next higher level:
  if (area == "sq") {

    ## Level 1: Population square
    x.pop <- .10 # basic width of population box: Area N = x.pop^2
    x.y.pop <- 1/1 # square

    ## Determine other box widths by proportions in freq:
    ## Level 2: Condition squares
    x.true <- sqrt(n.true/N * x.pop^2)
    x.false <- sqrt(n.false/N * x.pop^2)

    if (!all.equal(x.pop^2, sum(x.true^2, x.false^2))) {
      warning("sumtree 1: Sum of True and False area differs from Population area.")
    }

    ## Level 3: 4 SDT squares
    x.hi <- sqrt(n.hi/N * x.pop^2)
    x.mi <- sqrt(n.mi/N * x.pop^2)
    x.fa <- sqrt(n.fa/N * x.pop^2)
    x.cr <- sqrt(n.cr/N * x.pop^2)

    if (!all.equal(x.true^2, sum(x.hi^2, x.mi^2))) {
      warning("sumtree 2: Sum of HI and MI area differs from cond True area.")
    }
    if (!all.equal(x.false^2, sum(x.fa^2, x.cr^2))) {
      warning("sumtree 3: Sum of FA and CR area differs from cond False area.")
    }
    if (!all.equal(x.pop^2, sum(x.hi^2, x.mi^2, x.fa^2, x.cr^2))) {
      warning("sumtree 4: Population area differs from the area sum of all 4 SDT cases.")
    }

    ## Collect all sizes and proportions:
    x.boxes <- c(x.pop, x.true, x.false, x.hi, x.mi, x.fa, x.cr)
    x.y.prop <- rep(x.y.pop, 7) # all boxes have the same proportion (squares)
  }

  ## 5c. Rectangles that sum to the area of the next higher level:
  if (area == "hr") {

    ## Level 1: Population square
    x.pop <- .10   # basic width x of population box: Area N = x.pop^2
    x.y.pop <- 1/1 # square

    ## Determine other box widths by proportions in freq:
    ## Level 2: 2 vertical rectangles
    x.true <- (n.true/N) * x.pop # scale x.pop by proportion
    x.y.true <- x.pop/x.true

    x.false <- n.false/N * x.pop # scale x.pop by proportion
    x.y.false <- x.pop/x.false

    if (!all.equal(x.pop^2, sum((x.true * x.pop), (x.false * x.pop)))) {
      warning("hrectree 1: Sum of True + False areas differs from Population area.")
    }

    ## Level 3: 4 horizontal rectangles
    x.hi <- (n.hi/n.true) * x.pop # scale x.pop by sens
    x.y.hi <- x.true/x.hi

    x.mi <-  (1 - (n.hi/n.true)) * x.pop # scale x.pop by (1 - sens)
    x.y.mi <- x.true/x.mi

    if (!all.equal((x.true * x.pop),
                   sum((x.hi * x.true), (x.mi * x.true)))) {
      warning("hrectree 2: Sum of HI + MI area differs from Cond TRUE area.")
    }

    x.cr <- (n.cr/n.false) * x.pop # scale x.pop by spec
    x.y.cr <- x.false/x.cr

    x.fa <- (1 - (n.cr/n.false)) * x.pop # scale x.pop by (1 - spec)
    x.y.fa <- x.false/x.fa

    if (!all.equal((x.false * x.pop),
                   sum((x.fa * x.false), (x.cr * x.false)))) {
      warning("hrectree 3: Sum of FA + CR area differs from Cond FALSE area.")
    }

    if (!all.equal((x.pop^2),
                   sum((x.hi * x.true), (x.mi * x.true), (x.fa * x.false), (x.cr * x.false)))) {
      warning("hrectree 4: Population area differs from the area sum of all 4 SDT cases.")
    }

    ## Collect all sizes and proportions:
    x.boxes <- c(x.pop, x.true, x.false, x.hi, x.mi, x.fa, x.cr) # specific widths
    x.y.prop <- c(x.y.pop, x.y.true, x.y.false, x.y.hi, x.y.mi, x.y.fa, x.y.cr) # specific proportions
  }

  ## 5d. Rectangles that sum to the area of the next higher level
  ##     (= 3. flipped by 90 degrees on Level 3 to correspond to 4 SDT areas of mosaic plot):
  if (area == "vr") {

    ## Level 1: Population square
    x.pop <- .10   # basic width x of population box: Area N = x.pop^2
    x.y.pop <- 1/1 # square

    ## Determine other box widths by proportions in freq:
    ## Level 2: 2 vertical rectangles
    x.true <- (n.true/N) * x.pop # scale x.pop by proportion true
    x.y.true <- x.pop/x.true

    x.false <- n.false/N * x.pop # scale x.pop by proportion false
    x.y.false <- x.pop/x.false

    if (!all.equal(x.pop^2, sum((x.true * x.pop), (x.false * x.pop)))) {
      warning("vrectree 1: Sum of True + False areas differs from Population area.")
    }

    ## Level 3: 4 horizontal rectangles
    x.hi <- x.true # keep constant
    x.y.hi <- x.y.true * (n.hi/n.true) # scale previous prop by prop hi

    x.mi <- x.true # keep constant
    x.y.mi <- x.y.true * (n.mi/n.true) # scale previous prop by prop mi

    if (!all.equal((x.true * x.pop),
                   sum((x.hi * (x.hi * x.y.hi)), (x.mi * (x.mi * x.y.mi))))) {
      warning("vrectree 2: Sum of HI + MI area differs from Cond TRUE area.")
    }

    x.fa <- x.false # keep constant
    x.y.fa <- x.y.false * (n.fa/n.false) # scale previous prop by prop fa

    x.cr <- x.false # keep constant
    x.y.cr <- x.y.false * (n.cr/n.false) # scale previous prop by prop cr

    if (!all.equal((x.false * x.pop),
                   sum((x.fa * (x.fa * x.y.fa)), (x.cr * (x.cr * x.y.cr))))) {
      warning("vrectree 3: Sum of FA + CR area differs from Cond FALSE area.")
    }

    if (!all.equal((x.pop^2),
                   sum((x.hi * (x.hi * x.y.hi)),
                       (x.mi * (x.mi * x.y.mi)),
                       (x.fa * (x.fa * x.y.fa)),
                       (x.cr * (x.cr * x.y.cr))))) {
      warning("vrectree 4: Population area differs from the area sum of all 4 SDT cases.")
    }

    ## Collect all sizes and proportions:
    x.boxes <- c(x.pop, x.true, x.false, x.hi, x.mi, x.fa, x.cr) # specific widths
    x.y.prop <- c(x.y.pop, x.y.true, x.y.false, x.y.hi, x.y.mi, x.y.fa, x.y.cr) # specific proportions
  }

  ## (6) Plot matrix M (from diagram package):
  pp <- diagram::plotmat(M, # square coefficient matrix, specifying the links (rows = to, cols = from)
                         pos = c(1, 2, 4),
                         curve = 0.0, # no curve (> 0 curve left, < 0 curve right)
                         name = names,
                         relsize	= .98, # a scaling factor for the size of the graph
                         lwd = 1.5,
                         ## Boxes:
                         box.size = x.boxes,  # widths of boxes
                         box.prop = x.y.prop, # proportionality (length/width) ratio of boxes
                         box.type = "rect", # "ellipse", "diamond", "circle", "hexa", "multi", "none"
                         box.col = col.boxes, # scalar or vector of length 7.
                         # c(col.N, col.true, col.false, col.hi, col.mi, col.fa, col.cr), # WAS: "lightyellow"
                         box.lcol = col.border,
                         box.lwd = 2.0,
                         lcol = col.border, # default color for box and arrow lines
                         ## Text in Boxes:
                         txt.col = col.txt,
                         box.cex = .85, # relative size of text in boxes
                         txt.font = 1, # 1 = plain, 2 = bold, ...
                         ## Arrows:
                         cex.txt = .80, # relative size of arrow text
                         arr.pos = .50, # relative position of arrowhead on arrow segment/curve
                         arr.type = "triangle", # one of "curved", "triangle", "circle", "ellipse", "T", "simple"
                         arr.length = .20,
                         arr.width = .15,
                         arr.col = col.border,
                         shadow.size = cex.shadow, # .005
                         shadow.col = col.shadow #,
                         # main = paste0(title.lbl, ":\n", "Sum tree of natural frequencies (N = ", N, ")")
  )

  ## (7) Title:
  if (area == "no") {type.lbl <- "Tree"}
  if (area == "sq") {type.lbl <- "Area (square) tree"}
  if (area == "hr") {type.lbl <- "Area (horizontal rectangle) tree"}
  if (area == "vr") {type.lbl <- "Area (vertical rectangle) tree"}
  if (nchar(title.lbl) > 0) { title.lbl <- paste0(title.lbl, ":\n") }  # put on top (in separate line)
  cur.title.lbl <- paste0(title.lbl, type.lbl, " of natural frequencies") # , "(N = ", N, ")")
  title(cur.title.lbl, adj = 0.5, line = 1.0, font.main = 1) # (centered, raised, normal font)

  ## (8) Margin text:
  cur.par.lbl <-  paste0("(", "prev = ", as_pc(prev), "%, ", "sens = ", as_pc(sens), "%, ", "spec = ", as_pc(spec), "%)")
  mtext(cur.par.lbl, side = 1, line = 1, adj = 1, col = grey(.33, .99), cex = .90)

  ## (9)
  # return(pp)  # returns diagram object
  return()      # returns nothing

}

## Check:
{
  # plot_tree()
  # plot_tree(title.lbl = "")
  # plot_tree(N = 33)
  # plot_tree(N = NA)
  # plot_tree(prev = 1/3)
  # plot_tree(prev = 1/3, N = 55)
  # plot_tree(prev = 1/3, N = NA)
  # plot_tree(prev = 1/3, round = FALSE)
  # plot_tree(prev = .10, sens = .90, spec = 1/3, N = 100)
  # plot_tree(prev = .10, sens = .90, spec = NA, fart = 1/3, N = 33)
  # plot_tree(prev = .10, sens = .90, spec = 1/3, fart = NA, N = NA)
  # plot_tree(prev = .10, sens = .90, spec = NA, fart = 1/3, N = NA)
  #
  # plot_tree(area = "sq")
  # plot_tree(area = "sq", round = FALSE)
  # plot_tree(area = "hr")
  # plot_tree(area = "vr", round = FALSE)
  #
  # plot_tree(prev = .08, sens = .92, spec = .95, N = 10000, area = "hr")
  # plot_tree(area = "sq", col.boxes = "gold", col.border = "steelblue4", col.shadow = "steelblue4", cex.shadow = .008)
  # plot_tree(N = NA, area = "vr", col.txt = "steelblue4", col.boxes = "lightyellow", col.border = grey(.3, .7), cex.shadow = .008, col.shadow = grey(.1, .9))
}

## -----------------------------------------------
## (+) ToDo:

## - Allow shorter edge labels:
##   prev;  sens, mirt,  fart, spec

## - 0. Add complementary tree(s) that split(s)
##      level 1 by Decision (rather than by condition)!

## - 1. provide more info on current numeric inputs (prev, sens, spec, fart) on edges
## - 2. Make version with options for
##         a - providing fart rather than spec
##         b - freq rather than prev, sens, spec
## - 3. make text color adjustable (using col.txt)
## - 4. pimp plot (labels, colors, transparency)

## -----------------------------------------------
## eof.

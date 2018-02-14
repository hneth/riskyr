## riskyr_class.R | riskyr
## 2018 02 14
## -----------------------------------------------
## Define riskyr class and corresponding methods
## and re-define df.scenarios as a list of
## riskyr objects scenarios:
## -----------------------------------------------


## -----------------------------------------------
## (0) Get some exemplary scenarios and
##     save them with "riskyr" class attribute:

# scenario2 <- df.scenarios[2, ]  # get scenario 2 of df.scenarios
# class(scenario2) <- "riskyr"

# scenario3 <- df.scenarios[3, ]  # get scenario 3 of df.scenarios
# class(scenario3) <- "riskyr"


## -----------------------------------------------
## (1) Function to create riskyr scenarios: ------

#' Create riskyr scenarios.
#'
#' The instantiation function \code{riskyr} is used to create
#' scenarios of class "riskyr",
#' which can then be visualized by the \code{plot} method \code{\link{plot.riskyr}}
#' and summarized by the \code{summary} method \code{\link{summary.riskyr}}.
#'
#' Beyond basic scenario information
#' only the population size \code{\link{N}} and the essential probabilities
#' \code{\link{prev}}, \code{\link{sens}}, \code{\link{spec}}, and \code{\link{fart}}
#' are used and returned.
#'
#' @format An object of class "riskyr"
#' with 21 entries on textual and numeric information on
#' a riskyr scenario.
#'
#' @return A list \code{object} of class "riskyr"
#' containing information on a risky scenario.
#'
#' Text elements (all elements of \code{\link{txt}}:
#'
#' @param scen.lbl The current scenario title (sometimes in Title Caps).
#' @param scen.txt A longer text description of the current scenario
#' (which may extend over several lines).
#'
#' @param scen.lng Language of the current scenario (as character code).
#' Options: \code{"en"} for English, \code{"de"} for  German.
#'
#' @param popu.lbl A brief description of the current target population \code{\link{popu}} or sample.
#'
#' @param cond.lbl A name for the \emph{condition} or feature (e.g., some disease) currently considered.
#' @param cond.true.lbl A label for the \emph{presence} of the current condition
#' or \code{\link{cond.true}} cases (the condition's true state of TRUE).
#' @param cond.false.lbl A label for the \emph{absence} of the current condition
#' or \code{\link{cond.false}} cases (the condition's true state of FALSE).
#'
#' @param dec.lbl A name for the \emph{decision} or judgment (e.g., some diagnostic test) currently made.
#' @param dec.pos.lbl A label for \emph{positive} decisions
#' or \code{\link{dec.pos}} cases (e.g., predicting the presence of the condition).
#' @param dec.neg.lbl A label for \emph{negative} decisions
#' or \code{\link{dec.neg}} cases (e.g., predicting the absence of the condition).
#'
#' @param hi.lbl A label for \emph{hits} or \emph{true positives} \code{\link{hi}}
#' (i.e., correct decisions of the presence of the condition, when the condition is actually present).
#' @param mi.lbl A label for \emph{misses} or \emph{false negatives} \code{\link{mi}}
#' (i.e., incorrect decisions of the absence of the condition when the condition is actually present).
#' @param fa.lbl A label for \emph{false alarms} or \emph{false positives} \code{\link{fa}}
#' (i.e., incorrect decisions of the presence of the condition when the condition is actually absent).
#' @param cr.lbl A label for \emph{correct rejections} or \emph{true negatives} \code{\link{cr}}
#' (i.e., a correct decision of the absence of the condition, when the condition is actually absent).
#'
#' Numeric elements:
#'
#' @param N The number of individuals in the scenario's population.
#' A suitable value of \code{\link{N}} is computed, if not provided.
#'
#' @param prev The condition's prevalence \code{\link{prev}}
#' (i.e., the probability of condition being \code{TRUE}).
#'
#' @param sens The decision's sensitivity \code{\link{sens}}
#' (i.e., the conditional probability of a positive decision
#' provided that the condition is \code{TRUE}).
#' \code{sens} is optional when its complement \code{mirt} is provided.
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
#' Source information:
#'
#' @param scen.src Source information for the current scenario.
#'
#' @param scen.apa Source information for the current scenario
#' in the style of the American Psychological Association (APA style).
#'
#'
#' @examples
#' # Defining a scenario:
#' custom.scenario <- riskyr(scen.lbl = "Identify reoffenders",
#'   cond.lbl = "Being a reoffender", popu.lbl = "Prisoners",
#'   cond.true.lbl = "Has reoffended", cond.false.lbl = "Has not reoffended",
#'   dec.lbl = "Test result",
#'   dec.pos.lbl = "will reoffend", dec.neg.lbl = "will not reoffend",
#'   hi.lbl = "Reoffender found", mi.lbl = "Reoffender missed",
#'   fa.lbl = "False accusation", cr.lbl = "Correct release",
#'   prev = .45,  # prevalence of being a reoffender.
#'   sens = .98, spec = .46, fart = NA, N = 753,
#'   scen.src = "Ficticious example scenario")
#'
#' # Using a scenario:
#' summary(custom.scenario)
#' plot(custom.scenario)
#'
#' @export

riskyr <- function(scen.lbl = txt$scen.lbl, scen.lng = txt$scen.lng,
                   scen.txt = txt$scen.txt, popu.lbl = txt$popu.lbl,
                   cond.lbl = txt$cond.lbl,
                   cond.true.lbl = txt$cond.true.lbl, cond.false.lbl = txt$cond.false.lbl,
                   dec.lbl = txt$dec.lbl,
                   dec.pos.lbl = txt$dec.pos.lbl, dec.neg.lbl = txt$dec.neg.lbl,
                   hi.lbl = txt$hi.lbl, mi.lbl = txt$mi.lbl,
                   fa.lbl = txt$fa.lbl, cr.lbl = txt$cr.lbl,
                   prev = num$prev,
                   sens = num$sens,
                   spec = num$spec, fart = NA,
                   N = freq$N,
                   scen.src = txt$scen.src, scen.apa = txt$scen.apa) {

  # Create object (scenario):
  if (is_valid_prob_set(prev = prev, sens = sens, mirt = NA, spec = spec, fart = fart,
                        tol = .01)) {

    ## (a) Compute the complete quintet of probabilities:
    prob_quintet <- comp_complete_prob_set(prev, sens, mirt = NA, spec, fart)
    sens <- prob_quintet[2] # gets sens (if not provided)
    mirt <- prob_quintet[3] # gets mirt (if not provided)
    spec <- prob_quintet[4] # gets spec (if not provided)
    fart <- prob_quintet[5] # gets fart (if not provided)

  }

  object <- list(scen.lbl = scen.lbl, scen.lng = scen.lng, scen.txt = scen.txt,
                 popu.lbl = popu.lbl, cond.lbl = cond.lbl,
                 cond.true.lbl = cond.true.lbl, cond.false.lbl = cond.false.lbl,
                 dec.lbl = dec.lbl, dec.pos.lbl = dec.pos.lbl, dec.neg.lbl = dec.neg.lbl,
                 hi.lbl = hi.lbl, mi.lbl = mi.lbl, fa.lbl = fa.lbl, cr.lbl = cr.lbl,
                 prev = prev,
                 sens = sens,
                 spec = spec, fart = fart,
                 N = N,
                 scen.src = scen.src, scen.apa = scen.apa)

  # add class riskyr:
  class(object) <- "riskyr"

  return(object)
}


## Check:
{
  # test.obj <- riskyr()  # initialize with default parameters
  # names(test.obj)

  ## Compare with df.scenarios:
  # names(df.scenarios)
  # all.equal(names(test.obj), names(df.scenarios))

  # # cat(
  # #   paste0(
  # #     paste0(names(scenarios$scen1), " = ", names(scenarios$scen1)),
  # #     collapse = ", "))

}

## -----------------------------------------------
## (2) Define an object with a list of riskyr objects:
##     - Convert the data frame df.scenarios into
##       a list "scenarios" of riskyr objects:

scenarios <- NULL # initialize

## Helper stuff:
# cat(paste0("#'   \\item ", df.scenarios$scen.lbl[-1], "\n#'\n"))


#' A collection of riskyr scenarios from various sources.
#'
#' \code{scenarios} is a list  that
#' contains a collection of scenarios of class "riskyr" from the
#' scientific literature and other sources that can be used directly
#' in the visualization and summary functions.
#'
#' \code{scenarios} currently contains the following scenarios:
#'
#' \enumerate{
#'
#'   \item Mammografie 1
#'
#'   \item Nackenfaltentest (NFT)
#'
#'   \item HIV 1 (f)
#'   \item HIV 2 (f)
#'
#'   \item Mammography 2
#'
#'   \item Sepsis
#'
#'   \item Cab problem
#'
#'   \item Sigmoidoskopie 1
#'   \item Sigmoidoskopie 2
#'
#'   \item Brustkrebs 1
#'   \item Brustkrebs 2 (BRCA1)
#'   \item Brustkrebs 3 (BRCA1 + pos. Mam.)
#'   \item HIV 3 (m)
#'   \item HIV 4 (m)
#'   \item Nackenfaltentest 2 (NFT)
#'   \item Amniozentese (pos. NFT)
#'
#'   \item Musical town
#'   \item Mushrooms
#'
#'   \item Bowel cancer (FOB screening)
#'
#'   \item PSA test 1 (high prev)
#'   \item PSA test 2 (low prev)
#'
#'   \item Colorectal cancer
#'
#'   \item Psylicraptis screening
#'
#'   \item Mammography 6 (prob)
#'   \item Mammography 6 (freq)
#'
#' }
#'
#' Variables describing each scenario:
#'
#' \enumerate{
#'
#'   \item \code{scen.lbl} Text label for current scenario.
#'   \item \code{scen.lng} Language of current scenario.
#'   \item \code{scen.txt} Description text of current scenario.
#'
#'   \item \code{popu.lbl} Text label for current population.
#'
#'   \item \code{cond.lbl} Text label for current condition.
#'   \item \code{cond.true.lbl} Text label for \code{\link{cond.true}} cases.
#'   \item \code{cond.false.lbl} Text label for \code{\link{cond.false}} cases.
#'
#'   \item \code{dec.lbl} Text label for current decision.
#'   \item \code{dec.pos.lbl} Text label for \code{\link{dec.pos}} cases.
#'   \item \code{dec.neg.lbl} Text label for \code{\link{dec.neg}} cases.
#'
#'   \item \code{hi.lbl} Text label for cases of hits \code{\link{hi}}.
#'   \item \code{mi.lbl} Text label for cases of misses \code{\link{mi}}.
#'   \item \code{fa.lbl} Text label for cases of false alarms \code{\link{fa}}.
#'   \item \code{cr.lbl} Text label for cases of correct rejections \code{\link{cr}}.
#'
#'   \item \code{prev} Value of current prevalence \code{\link{prev}}.
#'   \item \code{sens} Value of current sensitivity \code{\link{sens}}.
#'   \item \code{spec} Value of current specificity \code{\link{spec}}.
#'   \item \code{fart} Value of current false alarm rate \code{\link{fart}}.
#'
#'   \item \code{N} Current population size \code{\link{N}}.
#'
#'   \item \code{scen.src} Source information for current scenario.
#'   \item \code{scen.apa} Source information in APA format.
#'
#' }
#'
#' Note that names of variables (columns)
#' correspond to \code{\link{init_txt}} (to initialize \code{\link{txt}})
#' and \code{\link{init_num}} (to initialize \code{\link{num}}).
#'
#' See columns \code{scen.src} and \code{scen.apa}
#' for a scenario's source information.
#'
#' The information of \code{scenarios} is also contained in an
#' R data frame \code{\link{df.scenarios}} (and generated from
#' the corresponding \code{.rda} file in \code{/data/}).
#'
#' @format A list with currently 26 objects of class "riskyr" (i.e., scenarios)
#' which are each described by 21 variables:
#'
#' @export
#'

scenarios <- vector("list", nrow(df.scenarios))  # initialize scenarios as a list (from df.scenarios)
names(scenarios) <- paste0("n", 1:nrow(df.scenarios))

for (i in 1:nrow(df.scenarios)) {  # for each scenario i in df.scenarios:

  ## (1) define scenario s:
  s <- df.scenarios[i, ]

  ## (2) pass scenario s to riskyr function:
  cur.scen <- riskyr(scen.lbl = s$scen.lbl, scen.lng = s$scen.lng, scen.txt = s$scen.txt,
                     popu.lbl = s$popu.lbl, cond.lbl = s$cond.lbl,
                     cond.true.lbl = s$cond.true.lbl, cond.false.lbl = s$cond.false.lbl,
                     dec.lbl = s$dec.lbl, dec.pos.lbl = s$dec.pos.lbl, dec.neg.lbl = s$dec.neg.lbl,
                     hi.lbl = s$hi.lbl, mi.lbl = s$mi.lbl, fa.lbl = s$fa.lbl, cr.lbl = s$cr.lbl,
                     prev = s$prev,
                     sens = s$sens,
                     spec = s$spec, fart = s$fart,
                     N = s$N,
                     scen.src = s$scen.src, scen.apa = s$scen.apa)  # use initialization function.

  # (3) Add cur.scen (riskyr object) as i-th element of scenarios
  scenarios[[i]] <- cur.scen

} # end for

## -----------------------------------------------
## (3) Define scenarios as the list scenarios.lst
##     (of riskyr scenario objects):

# scenarios <- NULL
# scenarios <- scenarios.lst

## Check:
# length(scenarios)
# scenarios$n25  # => shows elements of a scenario


## -----------------------------------------------
## Dealing with riskyr objects:
## -----------------------------------------------
## (4) plot.riskyr function:

#' Plot information of riskyr object.
#'
#' \code{plot.riskyr} is a method that allows to generate
#' different plot types from a \code{riskyr} object.
#'
#' \code{plot.riskyr} also uses the text settings
#' specified in the "riskyr" object.
#'
#' @param object  An object of class "riskyr", usually a result of a call to \code{riskyr}.
#' Pre-defined \code{\link{scenarios}} are also of type "riskyr".
#'
#' @param plot.type The type of plot to be generated by \code{plot.riskyr}.
#'
#' \enumerate{
#'
#'   \item \code{plot.type = "fnet"} or \code{plot.type = "network"}:
#'   Risk information is plotted in a network diagram of frequencies and probabilities (default).
#'   See \code{\link{plot_fnet}} for further options.
#'
#'   \item \code{plot.type = "ftree"} or \code{plot.type = "ftree"}:
#'   Risk information is plotted in a frequency tree.
#'   See \code{\link{plot_tree}} for further options.
#'
#'   \item \code{plot.type = "icons"} or \code{plot.type = "iconarray"}:
#'   The underlying population is plotted as icons.
#'   See \code{\link{plot_icons}} for further options.
#'
#'   \item \code{plot.type = "mosaic"} or \code{plot.type = "mosaicplot"}:
#'   Risk information is plotted as a mosaicplot.
#'   See \code{\link{plot_mosaic}} for further options.
#'
#'   \item \code{plot.type = "curve"} or \code{plot.type = "curves"}:
#'   Draws curves of selected values (including \code{\link{PPV}}, \code{\link{NPV}})
#'   See \code{\link{plot_curve}} for further options.
#'
#'   \item \code{plot.type = "plane"} or \code{plot.type = "planes"}:
#'   Draws a 3D-plane of selected values (e.g., predictive values \code{\link{PPV}} or \code{\link{NPV}})
#'   See \code{\link{plot_plane}} for further options.
#' }
#'
#' @param ... Additional parameters to be passed to the
#' underlying plotting functions.
#'
#' @examples
#' # Select a scenario from list of scenarios:
#' s25 <- scenarios$n25  # select scenario 25 from scenarios
#'
#' # Plot different types:
#' plot(s25)  # => default plot (fnet)
#' plot(s25, plot.type = "fnet")  # => network diagram (same as default)
#' plot(s25, plot.type = "tree", area = "vr") # => tree diagram (with vertical rectangles)
#' plot(s25, plot.type = "curve", what = "all")
#' plot(s25, plot.type = "icons")
#' plot(s25, plot.type = "icons", type = "mosaic")  # passing on additional parameter to create.
#' plot(s25, plot.type = "mosaic")
#' plot(s25, plot.type = "plane", what = "NPV")
#'
#'
#' @family visualization functions
#'
#'
#' @export

plot.riskyr <- function(object,
                        plot.type = "network",  # plot.type parameter for type of plot.
                        # type = "array",  # type parameter for plot subtypes.
                        ...  # ellipsis for additional type parameters for the plotting functions.
) {

  ## Test plot.type argument:
  if (!plot.type %in% c("fnet", "network",
                        "tree", "ftree",
                        "icons", "iconarray",
                        "mosaic", "mosaicplot",
                        "curve", "curves",
                        "plane", "planes")) {
    stop("Invalid plot.type specified in plot.riskyr.")
  }

  # Recovering additional parameters:
  arguments <- list(...)
  arg.names <- names(arguments)

  # all:
  if ("show.accu" %in% arg.names) show.accu <- arguments$show.accu else show.accu = TRUE
  if ("area" %in% arg.names) area <- arguments$area else area <- "no"  # tree and net.


  ## Plotting functions: ----------


  ## A. Frequency net (default):
  if ((plot.type == "fnet") || (plot.type == "network")) {

    if ("by" %in% arg.names) by <- arguments$by else by <- "cddc"

    plot_fnet(prev = object$prev,
              sens = object$sens, mirt = NA,
              spec = object$spec, fart = NA,
              N = object$N,
              round = TRUE,
              by = "cddc",
              area = "sq",
              p.lbl = "num",
              show.accu = TRUE,
              w.acc = 0.5,
              title.lbl = object$scen.lbl,
              popu.lbl = object$popu.lbl,
              cond.true.lbl = object$cond.true.lbl,
              cond.false.lbl = object$cond.false.lbl,
              dec.pos.lbl = object$dec.pos.lbl,
              dec.neg.lbl = object$dec.neg.lbl,
              hi.lbl = object$hi.lbl, mi.lbl = object$mi.lbl, fa.lbl = object$fa.lbl,
              cr.lbl = object$cr.lbl, col.txt = grey(0.01, alpha = 0.99), box.cex = 0.85,
              col.boxes = pal, col.border = grey(0.33, alpha = 0.99), lwd = 1.5,
              box.lwd = 1.5, col.shadow = grey(0.11, alpha = 0.99), cex.shadow = 0)

  } # if (plot.type == "network")


  ## B. Frequency tree:
  if ((plot.type == "tree") || (plot.type == "ftree")) {

    if ("by" %in% arg.names) by <- arguments$by else by <- "cd"
    if ("p.lbl" %in% arg.names) p.lbl <- arguments$p.lbl else p.lbl <- "mix"
    # if ("box.cex" %in% arg.names) box.cex <- arguments$box.cex else box.cex <- 0.90

    plot_tree(prev = object$prev,             # probabilities
              sens = object$sens, mirt = NA,
              spec = object$spec, fart = NA,  # was: num$fart,
              N = object$N,    # ONLY freq used (so far)
              ## Options:
              round = TRUE,  # Boolean: round freq (if computed), default: round = TRUE.
              by = by,     # 4 perspectives: "cd" by condition, "dc" by decision.
              area = area,   # 4 area types: "no" none (default), "sq" square, "hr" horizontal rectangles, "vr" vertical rectangles.
              p.lbl = p.lbl, # 4 probability (edge) label types: "nam" names, "num" numeric, "mix" essential names + complement values (default), "min" minimal.
              ## Compute and show accuracy info:
              show.accu = TRUE,  # compute and show accuracy metrics
              w.acc = .50,       # weight w for wacc (from 0 to 1)
              ## Labels:
              title.lbl = object$scen.lbl,     # custom text labels
              popu.lbl = object$popu.lbl,
              ## Condition labels:
              cond.true.lbl = object$cond.true.lbl,
              cond.false.lbl = object$cond.false.lbl,
              ## Decision labels:
              dec.pos.lbl = object$dec.pos.lbl,
              dec.neg.lbl = object$dec.neg.lbl,
              ## SDT combinations:
              hi.lbl = object$hi.lbl,
              mi.lbl = object$mi.lbl,
              fa.lbl = object$fa.lbl,
              cr.lbl = object$cr.lbl,
              ## Box settings:
              col.txt = grey(.01, alpha = .99),  # black
              box.cex = 0.90,                     # relative text size
              col.boxes = pal, # pal[c(1:9)],    # box colors (9 frequencies/boxes/colors)
              col.border = grey(.33, alpha = .99),  # grey
              ## Widths of arrows and box borders:
              lwd = 1.6,      # width of arrows
              box.lwd = 1.8,  # set to 0.001 to show boxes without borders (but =0 yields ERROR)
              ## Shadows:
              col.shadow = grey(.11, alpha = .99),  # dark grey
              cex.shadow = 0  # [values > 0 show shadows]
    )

  } #  if (plot.type == "tree")


  ## C. Mosaicplot:
  if ((plot.type == "mosaic") || (plot.type == "mosaicplot")) {
    plot_mosaic(prev = object$prev,
                sens = object$sens, mirt = NA,
                spec = object$spec, fart = NA,
                N = object$N,
                vsplit = TRUE,
                show.accu = TRUE, w.acc = 0.5,
                title.lbl = object$scen.lbl,
                col.sdt = c(pal["hi"], pal["mi"], pal["fa"], pal["cr"]))

  } # if (plot.type == "mosaicplot")


  ## D. Iconarrays
  if ((plot.type == "icons") || (plot.type == "iconarray")) {

    if ("ident.order" %in% arg.names) ident.order <- arguments$ident.order else ident.order <- c("hi", "mi", "fa", "cr")
    if ("type" %in% arg.names) type <- arguments$type else type <- "array"
    if ("cex.lbl" %in% arg.names) cex.lbl <- arguments$cex.lbl else cex.lbl <- 1.0

    plot_icons(prev = object$prev,             # probabilities
               sens = object$sens, mirt = NA,
               spec = object$spec, fart = NA,  # was: num$fart,
               N = object$N,    # ONLY freq used (so far)
               ## Key options: ##
               type = type,  # needs to be given if random position but nonrandom ident.
               # Types include: array, shuffled array, mosaic, equal, fillleft, filltop, scatter.
               ident.order = ident.order,
               icon.colors = pal[c("hi", "mi", "fa", "cr")],  # use one color for each usual type.
               icon.types = 22,  # plotting characters; default square with border
               icon.border.col = grey(.10, .50),  # border color of icons
               icon.border.lwd = 1.5, # line width of icons
               transparency = .50,
               icon.size = NULL,
               block.d = NULL,  # distance between blocks (where applicable).
               border.d = 0.1,  # distance of icons to border.

               # for classic icon arrays only:
               block.size.row = 10,
               block.size.col = 10,
               nblocks.row = NULL,
               nblocks.col = NULL,

               fill.array = "left",
               fill.blocks = "rowwise",

               show.accu = TRUE, # Option for showing current accuracy metrics.
               w.acc = 0.50,

               # labelling:
               title.lbl = object$scen.lbl,
               type.lbls = object[c("hi.lbl", "mi.lbl", "fa.lbl", "cr.lbl")],
               cex.lbl = cex.lbl
    )

  } #  if (plot.type == "iconarray")


  ## E. Curve:
  if ((plot.type == "curve") || (plot.type == "curves")) {

    if ("what" %in% arg.names) what <- arguments$what else what <- c("prev", "PPV", "NPV")
    if ("show.points" %in% arg.names) show.points <- arguments$show.points else show.points <- TRUE

    plot_curve(prev = object$prev,             # probabilities (3 essential, 2 optional)
               sens = object$sens, mirt = NA,
               spec = object$spec, fart = NA,
               ## DVs:
               what = what,  # what curves?  Options: "acc", "ppod"
               what.col = pal,                  # colors for what.
               ## Options:
               show.points = show.points,  # show points at current prev?
               log.scale = FALSE,   # x-axis on log scale?
               title.lbl = object$scen.lbl
    )
  } # if (plot.type == "curve")


  ## F. Plane:
  if ((plot.type == "plane") || (plot.type == "planes")) {

    if ("what" %in% arg.names) what <- arguments$what else what <- "PPV"
    if ("theta" %in% arg.names) theta <- arguments$theta else theta <- -45
    if ("phi" %in% arg.names) phi <- arguments$phi else phi <- 0
    if ("cex.lbl" %in% arg.names) cex.lbl <- arguments$cex.lbl else cex.lbl <- 0.85

    plot_plane(prev = object$prev,             # probabilities (3 essential, 2 optional)
               sens = object$sens, mirt = NA,
               spec = object$spec, fart = NA,
               ## DVs:
               what = what, # what plane?  Options: "PPV", "NPV", "acc", "ppod".
               ## Options:
               what.col = pal,     # color for what.
               step.size = .05,    # resolution of matrix (sens.range and spec.range)
               show.point = TRUE,  # show point on plane?
               ## Main persp() options [adjustable]:
               theta = theta,
               phi = phi,
               ## Text:
               title.lbl = object$scen.lbl, # plot title label
               cex.lbl = cex.lbl # scale size of text labels (e.g., on axes, legend, margin text)
    )
  } # if (plot.type == "plane")

  ## Add other plot.types:
  ## ( )

}


## Check:

{
  ## (A) with example scenarios (defined above):
  # plot(scenario2, plot.type = "icons")
  # plot(scenario3, plot.type = "tree")

  ## (B) with scenarios from scenarios (defined BELOW):
  #
  # s25 <- scenarios$n25  # select scenario 25 from scenarios
  #
  # plot(s25)  # => default plot (fnet)
  # plot(s25, plot.type = "fnet")  # => network diagram (same as default)
  # plot(s25, plot.type = "tree", area = "vr") # => tree diagram (with vertical rectangles)
  # plot(s25, plot.type = "curve", what = "all")
  # plot(s25, plot.type = "icons")
  # plot(s25, plot.type = "icons", type = "mosaic")  # passing on additional parameters.
  # plot(s25, plot.type = "mosaic")
  # plot(s25, plot.type = "plane", what = "NPV")
  # # plot(s25, plot.type = "wetwork")
}


## -----------------------------------------------
## (5) summary.riskyr function:

## (A) Create a summary objectect: ---------------

#' Summarizing risk information.
#'
#' \code{summary.riskyr} provides a \code{summary} method for objects of class "riskyr".
#'
#'
#' @format An object of class \code{summary.riskyr}
#' with up to 9 entries.
#'
#' @return A summary list \code{obj.sum}
#' with up to 9 entries, dependent on which information is requested by \code{summarize}.
#'
#' Scenario name, relevant condition \code{}, and \code{N}
#' are summarized by default.
#'
#' @param object  An object of class "riskyr", usually a result of a call to \code{riskyr}.
#' Inbuilt scenarios are also of type "riskyr".
#'
#' @param summarize What is summarized as a vector consisting of \code{c("freq", "prob", "accu")}
#' for frequencies, probabilities, and accuracy respectively.
#' The default "all" is an alias to all three.
#'
#' @examples
#' summary(scenarios$n4)
#'
#'
#' @family summary functions
#'
#' @export

summary.riskyr <- function(object, summarize = "all") {

  obj.sum <- list()  # initialize as list

  obj.sum$scen.lbl <- object$scen.lbl

  obj.sum$cond.lbl <- object$cond.lbl  # condition
  obj.sum$dec.lbl <- object$dec.lbl    # decision
  obj.sum$popu.lbl <- object$popu.lbl  # population
  obj.sum$N <- object$N                # N
  obj.sum$scen.src <- object$scen.src  # source (short)

  ## (0) If all should be summarized: ----------

  if (summarize == "all") summarize <- c("prob", "freq", "accu")



  ## (A) Probability information: ----------

  if (("prob" %in% summarize) || ("probs" %in% summarize) || ("probabilities" %in% summarize)) {

    # calculate all probabilities:
    probs <- comp_prob_prob(prev = object$prev, sens = object$sens, spec = object$spec)

    probs.ess <- unlist(probs[c("prev", "sens", "mirt", "spec", "fart")])  # essential probabilities.

    probs.ness <- unlist(probs[c("ppod", "PPV", "NPV", "FDR", "FOR")])  # non-essential probabilities.

    obj.sum$probs <- list(probs.ess = probs.ess, probs.ness = probs.ness)

  } # if "prob"



  ## (B) Frequency information: ----------

  if (("freq" %in% summarize) || ("freqs" %in% summarize) || ("frequencies" %in% summarize)) {

    # calculate frequencies:
    freqs <- comp_freq(prev = object$prev, sens = object$sens, spec = object$spec,
                       N = object$N)

    ## (a) Frequencies by condition:
    cond.freqs <- unlist(freqs[c("cond.true", "cond.false")])

    ## (b) Frequencies by decision:
    dec.freqs <- unlist(freqs[c("dec.pos", "dec.neg")])

    ## (c) Frequencies by accuracy (i.e., correspondence of decision to condition):
    acc.freqs <- unlist(freqs[c("dec.cor", "dec.err")])

    ## (d) SDT frequencies:
    sdt.freqs <- unlist(freqs[c("hi", "mi", "fa", "cr")])  # == "essential" frequencies.

    ## (+) Add to summary object:
    obj.sum$freqs <- list(cond.freqs = cond.freqs,
                          dec.freqs = dec.freqs,
                          acc.freqs = acc.freqs,
                          sdt.freqs = sdt.freqs)

  } # if "freq"


  ## (C) Accuracy information: ----------

  if (("acc" %in% summarize) || ("accu" %in% summarize) || ("accuracy" %in% summarize)) {

    ## Overall accuracy acc:
    obj.sum$acc <- comp_acc(prev = object$prev, sens = object$sens, spec = object$spec)

    ## ToDo: ALL accuracy metrics:
    # accu <- comp_accu(prev = obj$prev, sens = obj$sens, spec = obj$spec)

  } # if "acc"



  ## Add class to summary object: ----------

  class(obj.sum) <- c("summary.riskyr")

  return(obj.sum)

}


## (B) Create corresponding print function: ----------


#' Printing summarized risk information.
#'
#' \code{print.summary.riskyr} provides a \code{print} method for objects of class "summary.riskyr".
#'
#'
#' @format Printed output of a "summary.riskyr" object.
#'
#' @param object  An object of class "summaryriskyr", usually a result of a call to \code{summary.riskyr}.
#'
#' @param ... Additional parameters to be passed to the
#' generic print function.
#'
#' @examples
#' summary(scenarios$n4)
#'
#'
#' @family print functions
#'
#' @export

print.summary.riskyr <- function(object, ...) {

  ## 1. Always print header: ----------

  cat("Scenario: ",   object$scen.lbl, "\n\n")  # always show scenario name.

  cat("Condition: ",  object$cond.lbl, "\n")  # always show current condition.
  cat("Decision: ",   object$dec.lbl,  "\n")  # always show current decision.
  cat("Population: ", object$popu.lbl, "\n")  # always show current condition.
  cat("N = ", object$N, "\n")                 # always show population size N.
  cat("Source: ", object$scen.src, "\n")      # always show (short) source info

  ## 2. Print only on demand: ----------

  n <- names(object)  # save names.

  ## (A) Probabilities: ----------

  if ("probs" %in% n) {

    cat("\nProbabilities:\n\n")

    cat(" Essential probabilities:\n")
    # names(object$probs$probs.ess) <- c("Prevalence", "Sensitivity", "Miss rate", "Specificity", "False alarm rate")  # explicit
    names(object$probs$probs.ess) <- c("prev", "sens", "mirt", "spec", "fart")  # shorter
    print(object$probs$probs.ess)

    cat("\n Other probabilities:\n")
    print(round(object$probs$probs.ness, 3))  # no naming for non-essential probs.
  }

  ## (B) Frequencies: ----------

  if ("freqs" %in% n) {

    cat("\nFrequencies:\n")

    cat("\n by conditions:\n")
    # names(object$freqs$cond.freqs) <- c("True", "False")  # explicit
    names(object$freqs$cond.freqs) <- c("cond.true", "cond.false")  # more explicit
    print(object$freqs$cond.freqs)


    cat("\n by decision:\n")
    names(object$freqs$dec.freqs) <- c("Positive", "Negative")  # explicit
    names(object$freqs$dec.freqs) <- c("dec.pos", "dec.neg")  # more explicit
    print(object$freqs$dec.freqs)

    cat("\n by correspondence (of decision to condition):\n")
    # names(object$freqs$acc.freqs) <- c("Correct cases", "Incorrect cases")  # explicit
    names(object$freqs$acc.freqs) <- c("dec.cor", "dec.err")  # implicit
    print(object$freqs$acc.freqs)

    cat("\n 4 essential (SDT) frequencies:\n")
    # names(object$freqs$sdt.freqs) <- c("Hits", "Misses", "False alarms", "Correct rejections")  # explicit
    names(object$freqs$sdt.freqs) <- c("hi", "mi", "fa", "cr")  # implicit
    print(object$freqs$sdt.freqs)

  }

  ## (C) Accuracy: ----------

  if (("acc" %in% n) || ("accu" %in% n)) {

    cat("\nAccuracy:\n\n")

    cat(" acc:\n")
    cat(object$acc)  # overall accuracy acc only!

    ## ToDo: Include ALL other accuracy metrics (accu).

  }

}


## Check:
{
  # summary(scenario2)  # => all summaries
  # summary(scenario2, summarize = "freq")
  # summary(scenario2, summarize = "prob")
  # summary(scenario2, summarize = "accu")
}



## -----------------------------------------------
## (6) Typical user interaction / session:
## -----------------------------------------------
## (A) Defining and viewing your own scenario:

## ToDo: ( )

## -----------------------------------------------
## (B) Exporing pre-defined scenarios
## -----------------------------------------------
## Standard example: Mammography screening
## Source: Hoffrage et al. (2015), p. 3

# ## (a) Choosing a scenario
# s25 <- scenarios[[25]] # select by number: [[dd]]
# s25 <- scenarios$n25   # select by name:   $ndd

# ## (b) Summary info:
# summary(s25)

# ## (c) Visualization:
# plot(s25)  # => default plot (fnet)
# plot(s25, plot.type = "icons")
# plot(s25, plot.type = "curve")

## -----------------------------------------------
## Example 2: PSA screening
## Source: Arkes & Gaissmaier (2012), p. 550

## Overview:
# summary(scenarios$n21)

## Visualization:
# plot(scenarios$n21, plot.type = "tree", area = "sq")
# plot(scenarios$n21, plot.type = "icons")
# plot(scenarios$n21, plot.type = "curves", what = "all")
# plot(scenarios$n21, plot.type = "planes", what = "PPV")

## Contrast with lower prevalence version:

## Overview:
# summary(scenarios$n22)

## Visualization:
# plot(scenarios$n22, plot.type = "tree", area = "sq")
# plot(scenarios$n22, plot.type = "icons")
# plot(scenarios$n22, plot.type = "curves", what = "all")
# plot(scenarios$n22, plot.type = "planes", what = "PPV")

## -----------------------------------------------
## Example 3: Bowel cancer (FOB screening)
## Source: https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Worked_example

# s20 <- scenarios$n20   # select by name:   $ndd

## Overview:
# summary(s20)
# summary(s20, summarize = "freq")

## Visualization:
# plot(s20, plot.type = "tree", area = "vr") # => tree diagram (with vertical rectangles)
# plot(s20, plot.type = "curve", what = "all")
# plot(s20, plot.type = "icons")
# plot(s20, plot.type = "icons", type = "mosaic")  # passing on additional parameters.
# plot(s20, plot.type = "mosaic")
# plot(s20, plot.type = "plane", what = "NPV")
## plot(s20, plot.type = "wetwork")


## -----------------------------------------------
## (+) ToDo:

## - Summary: Extend to other types of accuracy in accu
##
## - allow riskyr() to take all kinds of inputs,
##   so that a full object is created.

## -----------------------------------------------
## eof.

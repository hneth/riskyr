## init_pal.R | riskyR
## 2018 01 16
## -----------------------------------------------
## Define and initialize current set of
## custom colors (pal):

## Note that pal contains defaults for user inputs.

## -----------------------------------------------
## Set defaults for all color inputs (pal):

## Utility function:

makeTransparent = function(..., alpha = .50) {

  if (alpha < 0 | alpha > 1) {
    stop("alpha must be between 0 and 1")
  }

  alpha <- floor(255 * alpha)
  newColor <- col2rgb(col = unlist(list(...)), alpha = FALSE)

  .makeTransparent <- function(col, alpha) {
    rgb(red = col[1],
        green = col[2],
        blue=col[3],
        alpha = alpha, maxColorValue = 255)
  }

  newColor <- apply(newColor, 2, .makeTransparent, alpha = alpha)

  return(newColor)

}



## -----------------------------------------------
## (1) Define some named colors:

{
  ## (1) from uni.kn:
  seeblau <- rgb(0, 169, 224, max = 255) # seeblau.4 (non-transparent)

  ## (2) from https://bootswatch.com/sandstone/:
  col.sand.light = rgb(248, 245, 240, max = 255)
  col.sand.mid   = rgb(142, 140, 132, max = 255)
  col.sand.dark  = rgb(62, 63, 58, max = 255)

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
  my.red  <- "tomato3"
  my.blue <- "steelblue3"
  my.green <- "olivedrab4"

  my.yellow <- "lightgoldenrod1"
  my.orange <- "sienna1"

  green.1 <- makeTransparent(my.green, alpha = .50)
  green.2 <- makeTransparent(my.green, alpha = 1.0)

  red.1 <- makeTransparent(my.red, alpha = .50)
  red.2 <- makeTransparent(my.red, alpha = 1.0)

  blue.1 <- makeTransparent(my.blue, alpha = .50)
  blue.2 <- makeTransparent(my.blue, alpha = 1.0)

  yellow.1  <- makeTransparent(my.yellow, alpha = .50)
  yellow.2  <- makeTransparent(my.yellow, alpha = 1.0)

  orange.1  <- makeTransparent(my.orange, alpha = .50)
  orange.2  <- makeTransparent(my.orange, alpha = 1.0)

}



## -----------------------------------------------
## (2) Select and name some colors by their function
##     (to set default colors for plots and app display):

{
  ## (a) Define base color (for population N):
  col.N <- grey(.95, .99) # "white", col.grey.1

  ## (b) Define 2 colors for condition cases:
  col.true <- my.yellow # "lightgoldenrod1" "gold1", col.orange.1, "yellow2"
  col.false <- "lightskyblue2" #, my.blue, "deepskyblue1" # "lightskyblue2" # col.blue.1
  ## Combine:
  cond.colors <- setNames(c(col.true, col.false),
                         c("true", "false")
  )

  ## (c) Define 4 colors for SDT cases:
  col.hi <- my.green # "olivedrab4", "palegreen4", col.green.2
  col.mi <- my.red   # "tomato3", "orangered3", "firebrick3", col.red.2
  col.fa <- "lightsalmon2" # lightcoral" # "tomato1" # "orangered1" # "firebrick1", col.red.1
  col.cr <- "olivedrab3"   # "springgreen2" # "palegreen3" # col.green.1
  ## Combine:
  sdt.colors <- setNames(c(col.hi, col.mi, col.fa, col.cr),
                         c("hi", "mi", "fa", "cr")
                         )

  ## (d) Define 2 colors for PVs:
  col.ppv <- my.orange # "sienna1" # col.orange.2 # "orange3" "firebrick" "red3"
  col.npv <- my.blue # "steelblue3", col.blue.3, "green4" "gray50" "brown4" "chartreuse4"
}

## -----------------------------------------------
## (3) Define corresponding color palette:

pal <- c(col.N, cond.colors, sdt.colors, col.ppv, col.npv) # vector of all colors
pal <- setNames(object = pal,
                nm = c("N", names(cond.colors), names(sdt.colors), "ppv", "npv")
                )

## Check:
# pal
# length(pal)
# pal[2] == pal["true"]

## -----------------------------------------------
## ggplot themes:

{
  library("ggplot2")

  my.theme <-  theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 12, color = col.grey.4, hjust = 0.0),
          axis.title = element_text(face = "plain", size = 11, color = col.sand.dark),
          axis.text = element_text(face = "plain", size = 10, color = col.sand.dark),
          # axis.line = element_line(size = 0.75, color = "black", linetype = 1),
          axis.ticks = element_line(size = 0.75, color = col.sand.mid, linetype = 1),
          panel.background = element_rect(fill = "grey99", color = col.sand.dark),
          panel.grid.major.x = element_line(color = col.sand.light, linetype = 1, size = .2),
          panel.grid.major.y = element_line(color = col.sand.light, linetype = 1, size = .2),
          # panel.grid.minor.x = element_blank(),
          # panel.grid.minor.y = element_blank(),
          legend.position = "none"
    )

  my.theme.legend <- theme_bw() +
    theme(plot.title = element_text(face = "bold", size = 12, color = col.grey.4, hjust = 0.0),
          axis.title = element_text(face = "plain", size = 11, color = col.sand.dark),
          axis.text = element_text(face = "plain", size = 10, color = col.sand.dark),
          # axis.line = element_line(size = 0.75, color = "black", linetype = 1),
          axis.ticks = element_line(size = 0.75, color = col.sand.mid, linetype = 1),
          panel.background = element_rect(fill = "grey99", color = col.sand.dark),
          panel.grid.major.x = element_line(color = col.sand.light, linetype = 1, size = .2),
          panel.grid.major.y = element_line(color = col.sand.light, linetype = 1, size = .2)#,
          # panel.grid.minor.x = element_blank(),
          # panel.grid.minor.y = element_blank()#,
          # legend.position = "none"
    )
}

## -----------------------------------------------
## (+) ToDo:

## - use standard colors as default
## - add pre-defined color palettes & transparency
## - make colors user-customizable

## -----------------------------------------------
## eof.

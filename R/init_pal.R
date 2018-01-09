## init_pal.R | riskyR
## 2018 01 08
## -----------------------------------------------
## Define and initialize current set of
## custom colors (pal):

## Note that pal contains defaults for user inputs.

## -----------------------------------------------
## Set defaults for all color inputs (pal):

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

}

## -----------------------------------------------
## (2) Select and name some colors by their function
##     (to set default colors for plots and app display):

{
  ## Define 4 colors for SDT cases:
  col.hi <- col.green.2
  col.mi <- col.red.2
  col.fa <- col.red.1
  col.cr <- col.green.1

  ## Combine:
  sdt.colors <- setNames(c(col.hi, col.mi, col.fa, col.cr),
                         c("hi", "mi", "fa", "cr")
                         )

  ## Define 2 colors for PVs:
  col.ppv <- col.orange.2 # "orange3" "firebrick" "red3"
  col.npv <- col.blue.3   # "steelblue3" "green4" "gray50" "brown4" "chartreuse4"
}

## -----------------------------------------------
## (3) Define corresponding color palette:

pal <- c(sdt.colors, col.ppv, col.npv) # vector of colors
pal <- setNames(object = pal,
                nm = c(names(sdt.colors), "ppv", "npv")
                )

## Check:
# pal
# length(pal)
# pal[4] == pal["cr"]

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

## - add color pal cus objects (inputs)
## - add pre-defined color palettes
## - make colors user-customizable

## -----------------------------------------------
## eof.

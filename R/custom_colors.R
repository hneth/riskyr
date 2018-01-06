## riskyR
## 2018 01 06
## -----------------------------------------------
## Define and initialize current color information:

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

## (2) Select and name colors by their function
##     (to set default colors for plots and app display):

{
  col.hi <- col.green.2
  col.mi <- col.red.2
  col.cr <- col.green.1
  col.fa <- col.red.1

  sdt.colors <- setNames(c(col.hi, col.mi, col.cr, col.fa),
                         c("hi", "mi", "cr", "fa")
  )

  col.ppv <- col.orange.2 # "orange3" "firebrick" "red3"
  col.npv <- col.blue.3   # "steelblue3" "green4" "gray50" "brown4" "chartreuse4"
}

## (+) ToDo:
## - Add colors to cus object?
## - Add pre-defined color palettes and
## - make colors user-customizable.

## -----------------------------------------------
## eof.

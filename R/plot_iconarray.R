## temp_code.R | riskyR
## 2018 01 08
## -----------------------------------------------

##  This function plots an iconarray flexibly, dependent on population size

## code

## -----------------------------------------------
## (+) ToDo:

## - ...

dev.new(width = 5, height = 5)  # create device with known aspect ratio.

# define parameters:
pop <- 73 # define population size.
ncols <- 10  # define number of columns (to be filled by row)

nrows <- ceiling(pop / ncols)

# create raw plot to accomodate the icons:
plot(1, 1, type = "n", xlim = c(0.5, 10 + 0.5), ylim = c(0.5, 10 + 0.5))

# rect(0.55, 0.45, 1.45, 1.45, asp = 1)  # lower left.
#
# rect(1.55, 0.45, 2.45, 1.45, asp = 1)  # to the right of first.
#
# rect(2.55, 0.45, 3.45, 1.45, asp = 1)
#
# rect(3.55, 0.45, 4.45, 1.45, asp = 1)
#
# rect(0.45, 1.55, 1.45, 2.55, asp = 1)  # above first.


design.matrix <- expand.grid(1:ncols, 10:(1 + ncols - nrows))  # create a matrix for positions.
design.matrix <- design.matrix[1:pop, ]  # truncate the matrix to population size.

# TODO: Integrate color in design matrix?

for(i in 1:nrow(design.matrix)){

  xleft <- design.matrix[i,1] - 0.45
  xright <- design.matrix[i,1] + 0.45

  ybottom <- design.matrix[i,2] - 0.45
  ytop <- design.matrix[i,2] + 0.45

  rect(xleft, ybottom, xright, ytop, asp = 1)
}

# points(3, 3, pch = 15)  # does not really work with cex...; maybe try later.

lines(par("usr")[1], par("usr")[2], col = "red")

lines(x = c(1, 5), y = c(5, 5), col = "red")

## -----------------------------------------------
## eof.

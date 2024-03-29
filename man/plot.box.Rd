% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_util.R
\name{plot.box}
\alias{plot.box}
\title{Plot a frequency box object}
\usage{
\method{plot}{box}(x, cur_freq = freq, lbl_txt = txt, col_pal = pal, ...)
}
\arguments{
\item{x}{The box (i.e., an object of class \code{box}) to be plotted.}

\item{cur_freq}{Current frequency information
(see \code{\link{freq}} for details).}

\item{lbl_txt}{Current text information
(see \code{\link{txt}} for details).}

\item{col_pal}{Current color palette
(see \code{\link{pal}} for details).}

\item{...}{Additional (graphical) parameters to be passed to the
underlying plotting functions.}
}
\description{
\code{plot.box} is a utility method that allows to plot low level boxes for \code{riskyr} plots.
}
\details{
\code{plot.riskyr} also uses the text settings
specified in the "riskyr" object.
}
\seealso{
Other utility functions: 
\code{\link{as_pb}()},
\code{\link{as_pc}()},
\code{\link{print.box}()}
}
\concept{utility functions}

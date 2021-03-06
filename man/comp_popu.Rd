% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_popu.R
\name{comp_popu}
\alias{comp_popu}
\title{Compute a population table (data) from frequencies (description).}
\format{
An object of class \code{data.frame}
with \code{\link{N}} rows and 3 columns
(e.g., \code{"X/truth/cd", "Y/test/dc", "SDT/cell/class"}).
}
\usage{
comp_popu(
  hi = freq$hi,
  mi = freq$mi,
  fa = freq$fa,
  cr = freq$cr,
  cond_lbl = txt$cond_lbl,
  cond_true_lbl = txt$cond_true_lbl,
  cond_false_lbl = txt$cond_false_lbl,
  dec_lbl = txt$dec_lbl,
  dec_pos_lbl = txt$dec_pos_lbl,
  dec_neg_lbl = txt$dec_neg_lbl,
  sdt_lbl = txt$sdt_lbl,
  hi_lbl = txt$hi_lbl,
  mi_lbl = txt$mi_lbl,
  fa_lbl = txt$fa_lbl,
  cr_lbl = txt$cr_lbl
)
}
\arguments{
\item{hi}{The number of hits \code{\link{hi}} (or true positives).}

\item{mi}{The number of misses \code{\link{mi}} (or false negatives).}

\item{fa}{The number of false alarms \code{\link{fa}} (or false positives).}

\item{cr}{The number of correct rejections \code{\link{cr}} (or true negatives).}

\item{cond_lbl}{Text label for condition dimension ("by cd" perspective).}

\item{cond_true_lbl}{Text label for \code{\link{cond_true}} cases.}

\item{cond_false_lbl}{Text label for \code{\link{cond_false}} cases.}

\item{dec_lbl}{Text label for decision dimension ("by dc" perspective).}

\item{dec_pos_lbl}{Text label for \code{\link{dec_pos}} cases.}

\item{dec_neg_lbl}{Text label for \code{\link{dec_neg}} cases.}

\item{sdt_lbl}{Text label for 4 cases/combinations (SDT classifications).}

\item{hi_lbl}{Text label for \code{\link{hi}} cases.}

\item{mi_lbl}{Text label for \code{\link{mi}} cases.}

\item{fa_lbl}{Text label for \code{\link{fa}} cases.}

\item{cr_lbl}{Text label for \code{\link{cr}} cases.}
}
\value{
A data frame \code{popu}
containing \code{\link{N}} rows (individual cases)
and 3 columns (e.g., \code{"X/truth/cd", "Y/test/dc", "SDT/cell/class"}).
encoded as ordered factors (with 2, 2, and 4 levels, respectively).
}
\description{
\code{comp_popu} computes a table \code{\link{popu}} (as an R data frame)
from the current frequency information (contained in \code{\link{freq}}).
}
\details{
By default, \code{comp_popu} uses the text settings
contained in \code{\link{txt}}.

A visualization of the current population
\code{popu} is provided by \code{\link{plot_icons}}.
}
\examples{
popu <- comp_popu()  # => initializes popu (with current values of freq and txt)
dim(popu)            # => N x 3
head(popu)

# (A) Diagnostic/screening scenario (using default labels):
comp_popu(hi = 4, mi = 1, fa = 2, cr = 3)  # => computes a table of N = 10 cases.

# (B) Intervention/treatment scenario:
comp_popu(hi = 3, mi = 2, fa = 1, cr = 4,
          cond_lbl = "Treatment", cond_true_lbl = "pill", cond_false_lbl = "placebo",
          dec_lbl = "Health status", dec_pos_lbl = "healthy", dec_neg_lbl = "sick")

# (C) Prevention scenario (e.g., vaccination):
comp_popu(hi = 3, mi = 2, fa = 1, cr = 4,
          cond_lbl = "Vaccination", cond_true_lbl = "yes", cond_false_lbl = "no",
          dec_lbl = "Disease", dec_pos_lbl = "no flu", dec_neg_lbl = "flu")

}
\seealso{
\code{\link{read_popu}} creates a scenario (description) from data (as df);
\code{\link{write_popu}} creates data (as df) from a riskyr scenario (description);
\code{\link{popu}} for data format;
\code{\link{num}} for basic numeric parameters;
\code{\link{freq}} for current frequency information;
\code{\link{txt}} for current text settings;
\code{\link{pal}} for current color settings.

Other functions converting data/descriptions: 
\code{\link{read_popu}()},
\code{\link{write_popu}()}
}
\concept{functions converting data/descriptions}

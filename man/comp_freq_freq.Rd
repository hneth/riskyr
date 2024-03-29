% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_freq_freq.R
\name{comp_freq_freq}
\alias{comp_freq_freq}
\title{Compute frequencies from (4 essential) frequencies.}
\usage{
comp_freq_freq(hi = freq$hi, mi = freq$mi, fa = freq$fa, cr = freq$cr)
}
\arguments{
\item{hi}{The number of hits \code{\link{hi}} (or true positives).}

\item{mi}{The number of misses \code{\link{mi}} (or false negatives).}

\item{fa}{The number of false alarms \code{\link{fa}} (or false positives).}

\item{cr}{The number of correct rejections \code{\link{cr}} (or true negatives).}
}
\description{
\code{comp_freq_freq} computes current frequency information
from 4 essential frequencies
(\code{\link{hi}}, \code{\link{mi}}, \code{\link{fa}}, \code{\link{cr}}).
It returns a list of 11 frequencies \code{\link{freq}}
for a population of \code{\link{N}} individuals
as its output.
}
\details{
Key relationships between frequencies and probabilities
(see documentation of \code{\link{comp_freq}} or \code{\link{comp_prob}} for details):

\itemize{

  \item Three perspectives on a population:

  by condition / by decision / by accuracy.

  \item Defining probabilities in terms of frequencies:

  Probabilities can be computed as ratios between frequencies, but beware of rounding issues.

}


Functions translating between representational formats:
\code{\link{comp_prob_prob}}, \code{\link{comp_prob_freq}},
\code{\link{comp_freq_prob}}, \code{comp_freq_freq}
(see documentation of \code{\link{comp_prob_prob}} for details).
}
\examples{
## Basics:
comp_freq_freq()
all.equal(freq, comp_freq_freq())  # => should be TRUE

## Circular chain:
# 1. Current numeric parameters:
num

# 2. Compute all 10 probabilities in prob (from essential probabilities):
prob <- comp_prob()
prob

# 3. Compute 9 frequencies in freq from probabilities:
freq <- comp_freq(round = FALSE)   # no rounding (to obtain same probabilities later)
freq

# 4. Compute 9 frequencies AGAIN (but now from frequencies):
freq_freq <- comp_freq_freq()

# 5. Check equality of results (steps 2. and 4.):
all.equal(freq, freq_freq)  # => should be TRUE!

}
\seealso{
\code{\link{comp_freq_prob}} computes current frequency information from (3 essential) probabilities;
\code{\link{comp_prob_freq}} computes current probability information from (4 essential) frequencies;
\code{\link{comp_prob_prob}} computes current probability information from (3 essential) probabilities;
\code{\link{num}} contains basic numeric parameters;
\code{\link{init_num}} initializes basic numeric parameters;
\code{\link{prob}} contains current probability information;
\code{\link{comp_prob}} computes current probability information;
\code{\link{freq}} contains current frequency information;
\code{\link{comp_freq}} computes current frequency information;
\code{\link{is_prob}} verifies probability inputs;
\code{\link{is_freq}} verifies frequency inputs.

Other functions computing frequencies: 
\code{\link{comp_freq}()},
\code{\link{comp_freq_prob}()},
\code{\link{comp_min_N}()},
\code{\link{comp_prob_prob}()}

Other format conversion functions: 
\code{\link{comp_freq_prob}()},
\code{\link{comp_prob_freq}()},
\code{\link{comp_prob_prob}()}
}
\concept{format conversion functions}
\concept{functions computing frequencies}

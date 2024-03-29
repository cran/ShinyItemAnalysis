% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMultinomial.R
\name{plotMultinomial}
\alias{plotMultinomial}
\title{Plot category probabilities of multinomial model}
\usage{
plotMultinomial(x, matching, matching.name = "matching")
}
\arguments{
\item{x}{object of class \code{multinom}}

\item{matching}{numeric: vector of matching criterion used for estimation in
\code{x}.}

\item{matching.name}{character: name of matching criterion used for
estimation in \code{x}.}
}
\value{
An object of class \code{ggplot} and/or \code{gg}.
}
\description{
Plots category probabilities functions estimated by
\code{multinom()} from the \code{nnet} package using the \pkg{ggplot2}
package.
}
\examples{
# loading data
data(GMAT, GMATtest, GMATkey, package = "difNLR")

matching <- scale(rowSums(GMAT[, 1:20])) # Z-score

# multinomial model for item 1
fit <- nnet::multinom(relevel(GMATtest[, 1], ref = paste(GMATkey[1])) ~ matching)

# plotting category probabilities
plotMultinomial(fit, matching, matching.name = "Z-score")

}
\seealso{
\code{\link[nnet:multinom]{nnet::multinom()}}
}
\author{
Adela Hladka \cr
Institute of Computer Science of the Czech Academy of Sciences \cr
\email{hladka@cs.cas.cz}

Tomas Jurica \cr
Institute of Computer Science of the Czech Academy of Sciences

Patricia Martinkova \cr
Institute of Computer Science of the Czech Academy of Sciences \cr
\email{martinkova@cs.cas.cz}
}

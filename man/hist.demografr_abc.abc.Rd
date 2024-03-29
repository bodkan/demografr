% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hist.demografr_abc.R
\name{hist.demografr_abc.abc}
\alias{hist.demografr_abc.abc}
\title{Plot histogram of posterior distribution(s)}
\usage{
\method{hist}{demografr_abc.abc}(x, param = NULL, ...)
}
\arguments{
\item{x}{ABC object generated by \code{run_abc}}

\item{param}{A character vector containing either parameter names to summarize,
or a regex-like matches to be used for subsetting. If \code{NULL} (the default),
all parameters will be extracted.}

\item{...}{Formal ellipsis argument of the \code{plot} method (unused)}
}
\value{
A base R histogram plot
}
\description{
Plot histogram of posterior distribution(s)
}

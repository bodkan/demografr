% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_posterior.R
\name{plot_posterior}
\alias{plot_posterior}
\title{Plot posterior distribution of given parameters}
\usage{
plot_posterior(
  abc,
  param = NULL,
  posterior = c("adj", "unadj"),
  facets = FALSE,
  file = NULL,
  ...
)
}
\arguments{
\item{abc}{ABC object generated by \code{run_abc}}

\item{param}{A character vector containing either parameter names to summarize,
or a regex-like matches to be used for subsetting. If \code{NULL} (the default),
all parameters will be extracted.}

\item{posterior}{Should an "adj"-usted or "unadj"-usted posterior be extracted?
(Default is "adj").}

\item{facets}{Should individual distributions be plotted in separate facets? Default
is \code{FALSE}.}

\item{file}{Output file for a figure saved via \code{ggsave}}

\item{...}{Optional argument which will be passed to \code{ggsave}}
}
\value{
A ggplot2 plot object
}
\description{
Plot posterior distribution of given parameters
}

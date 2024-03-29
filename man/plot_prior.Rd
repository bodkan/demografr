% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_prior.R
\name{plot_prior}
\alias{plot_prior}
\title{Plot prior distribution(s)}
\usage{
plot_prior(
  x,
  param = NULL,
  facets = FALSE,
  file = NULL,
  replicates = 10000,
  geom = ggplot2::geom_density,
  ...
)
}
\arguments{
\item{x}{Either an object of the class \code{demografr_abc}, or a list of prior
sampling statements}

\item{param}{A character vector containing either parameter names to summarize,
or a regex-like matches to be used for subsetting. If \code{NULL} (the default),
all parameters will be extracted.}

\item{facets}{Should individual parameters be plotting on a facet each?}

\item{file}{Output file for a figure saved via \code{ggsave}}

\item{replicates}{How many samples to simulate from each prior for plotting?}

\item{geom}{Either \code{ggplot2::geom_histogram} or \code{ggplot2::geom_density}}

\item{...}{Optional argument which will be passed to \code{ggsave}}
}
\value{
A ggplot2 plot object
}
\description{
Plot prior distribution(s)
}

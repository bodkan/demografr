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
\examples{
priors <- list(
  Ne_A  ~ runif(1000, 3000),
  Ne_B  ~ runif(100,  1500),
  Ne_C  ~ runif(5000, 10000),
  Ne_D  ~ runif(2000, 7000),

  T_AB  ~ runif(1,    4000),
  T_BC  ~ runif(3000, 9000),
  T_CD  ~ runif(5000, 10000),

  gf_BC ~ runif(0, 0.3)
)

# as with many other distribution plotting functions, plotting everything
# at once doesn't make much sense
# plot_prior(priors)

# it's better to visualize together distributions of the same scale
# plot_prior(priors, "^Ne")
plot_prior(priors, "gf") + ggplot2::xlim(0, 1)

}

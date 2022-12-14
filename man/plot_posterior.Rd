% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viz.R
\name{plot_posterior}
\alias{plot_posterior}
\title{Plot posterior distribution(s)}
\usage{
plot_posterior(
  abc,
  param = NULL,
  type = NULL,
  posterior = c("adj", "unadj"),
  summary = c("mode", "mean", "median"),
  geom = ggplot2::geom_density,
  facets = TRUE,
  xlim = NULL,
  ...
)
}
\description{
Plot posterior distribution(s)
}

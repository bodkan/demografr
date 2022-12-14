% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abc.R
\name{validate_abc}
\alias{validate_abc}
\title{Validate the individual components of an ABC model}
\usage{
validate_abc(
  model,
  priors,
  functions,
  observed,
  model_args = NULL,
  sequence_length = 10000,
  recombination_rate = 0,
  mutation_rate = 0,
  ...
)
}
\arguments{
\item{model}{A compiled slendr model object}

\item{priors}{A list of prior distributions to use for sampling of model parameters}

\item{functions}{A named list of summary statistic functions to apply on simulated
tree sequences}

\item{observed}{A named list of observed summary statistics}
}
\value{
No return value. The function is ran for its terminal output.
}
\description{
Validates the ABC setup by checking that all priors can be correctly sampled from,
that a slendr model resulting from those priors can simulate a tree sequence,
and that the user-defined summary functions produce output compatible with
the provided empirical summary statistics.
}

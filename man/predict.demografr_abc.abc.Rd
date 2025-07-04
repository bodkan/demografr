% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.demografr_abc.abc.R
\name{predict.demografr_abc.abc}
\alias{predict.demografr_abc.abc}
\title{Generate summary statistics from the inferred posterior distribution of parameters}
\usage{
\method{predict}{demografr_abc.abc}(
  object,
  samples,
  stat = NULL,
  posterior = c("adj", "unadj"),
  strict = FALSE,
  functions = NULL,
  ...
)
}
\arguments{
\item{object}{ABC object generated by \code{run_abc}}

\item{samples}{Number of draws from the posterior distribution to simulate}

\item{stat}{Which tree-sequence summary statistic to compute? If \code{NULL}, all summary
statistics will be computed.}

\item{posterior}{Should 'unadj'usted or 'adj'usted parameters be sampled? Default
(recommended) value is 'unadj'usted.}

\item{strict}{Should parameter combinations leading to invalid slendr models be treated as
an error? Default is \code{TRUE}. If set to \code{FALSE}, invalid simulations will be
simply dropped, with an informative message. This parameter is internally passed to the
function \code{simulate_grid()} which performs the simulations across a sampled parameter
matrix.}

\item{functions}{A named list of summary statistic tree-sequence functions to be
applied to each simulated tree sequence. If \code{NULL} (the default), the same
summary statistics will be computed as those that were used in the ABC inference
itself. Otherwise, custom tree-sequence summary statistics can be provided.}

\item{...}{A formal argument of the \code{predict} generic method (unused)}
}
\value{
A data frame object with the results of posterior simulations, with values of
 each summary statistic stored in a list-column (i.e. the same format as used by
 the function \code{simulate_grid}, as the predictions are generated internally using
}
\description{
The function draws parameter values from the posterior distribution and then
internally runs the function \code{simulate_grid} to run simulations from
the sampled parameters and compute tree-sequence summary statistics
}
\examples{
\dontrun{
# read statistics computed from posterior sample simulations
# the `pred` object was produced by the following call:
#    pred <- predict(abc, samples = 1000, posterior = "unadj")
# (where `abc` is the product of the `run_abc()` function)
pred <- readRDS(url(paste0(
"https://raw.githubusercontent.com/bodkan/demografr/",
"refs/heads/main/inst/examples/downstream_predX.rds"
)))

# note the columns `diversity`, `divergence`, and `f4`
pred

# extract_prediction() is a convenience function which unpacks the given
# summary statistic in a normal data frame column (here `diversity`)
extract_prediction(pred, "diversity")

# we can also plot the posterior predictive distributions for all statistics
plot_prediction(stats, "diversity")
plot_prediction(stats, "divergence")
}

}

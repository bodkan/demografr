skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE))

library(slendr)
init_env(quiet = TRUE)

SEED <- 42

model <- function(Ne_p1, Ne_p2, Ne_p3, Ne_p4, T_p1_p2, T_p2_p3, T_p3_p4) {
  p1 <- population("p1", time = 1, N = 1000)
  p2 <- population("p2", time = T_p1_p2, N = 3000, parent = p1)
  p3 <- population("p3", time = T_p2_p3, N = 10000, parent = p2)
  p4 <- population("p4", time = T_p3_p4, N = 5000, parent = p3)

  model <- compile_model(
    populations = list(p1, p2, p3, p4),
    generation_time = 1,
    simulation_length = 10000, serialize = FALSE
  )

  return(model)
}

samples <- list(
  p1 = paste0("p1_", 1:10),
  p2 = paste0("p2_", 1:10),
  p3 = paste0("p3_", 1:10),
  p4 = paste0("p4_", 1:10)
)

m <- model(100, 2000, 5000, 800, 1000, 3000, 4000)

ts <- msprime(m, sequence_length = 1e6, recombination_rate = 0, random_seed = 42)

set.seed(SEED)

samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name") %>% lapply(sample, 5)

pi_df <- ts_diversity(ts, sample_sets = samples, mode = "branch")
d_df <- ts_divergence(ts, sample_sets = samples, mode = "branch")
observed <- list(diversity = pi_df, divergence = d_df)

priors <- list(
  Ne_p1 ~ runif(10, 10000),
  Ne_p2 ~ runif(10, 10000),
  Ne_p3 ~ runif(10, 10000),
  Ne_p4 ~ runif(10, 10000),

  T_p1_p2 ~ runif(100, 1500),
  T_p2_p3 ~ runif(1500, 3500),
  T_p3_p4 ~ runif(3000, 5000)
)

compute_diversity <- function(ts) {
  samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name") %>% lapply(sample, 5)
  ts_diversity(ts, sample_sets = samples, mode = "branch")
}
compute_divergence <- function(ts) {
  samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name") %>% lapply(sample, 5)
  ts_divergence(ts, sample_sets = samples, mode = "branch")
}
functions <- list(diversity = compute_diversity, divergence = compute_divergence)

test_that("correct ABC setups are validated", {
  expect_output(validate_abc(model, priors, functions, observed))
})

test_that("validation output can be silenced", {
  expect_silent(validate_abc(model, priors, functions, observed, quiet = TRUE))
})

test_that("consistent naming of summary functions and observed statistics is enforced", {
  error_msg <- "Elements of lists of summary functions and observed statistics must have the same names"

  xfunctions <- functions
  xfunctions$diversityyy <- xfunctions$diversity
  xfunctions$diversity <- NULL
  expect_error(validate_abc(model, priors, xfunctions, observed), error_msg)

  xobserved <- observed
  xobserved$diversityyy <- xobserved$diversity
  xobserved$diversity <- NULL
  expect_error(validate_abc(model, priors, functions, xobserved), error_msg)
})

test_that("errors in summary functions will be correctly reported", {
  error_msg <- "Computation of '.*' function on simulated tree sequence has failed"

  xfunctions <- functions
  xfunctions$diversity <- function(ts) stop("ERROR HERE!\n", call. = FALSE)
  expect_error(quiet(validate_abc(model, priors, xfunctions, observed), error_msg))
})

test_that("dimensions of simulated and observed summary statistics must be the same", {
  error_msg <- "Dimensions of observed and simulated statistics differ"

  xfunctions <- functions
  xfunctions$diversity <- function(ts) {
    samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
    ts_diversity(ts, sample_sets = samples) %>%
      mutate(stat = paste0("pi_", set)) %>%
      select(stat, value = diversity) %>%
      utils::head(1)
  }
  expect_error(quiet(validate_abc(model, priors, xfunctions, observed), error_msg))
})

test_that("names of observed and simulated statistics must be the same", {
  error_msg <- "Elements of lists of summary functions and observed statistics must have the same names"

  xobserved <- observed
  xobserved$diversityyy <- xobserved$diversity[1, ]
  xobserved$diversity <- NULL
  expect_error(validate_abc(model, priors, functions, xobserved), error_msg)
})

test_that("errors in prior sampling are correctly caught", {
  xpriors <- priors
  xpriors[[1]] <- Ne_popA ~ runif(1e6, 10000)
  error_msg <- "Sampling the prior Ne_popA resuted in the following problem"
  expect_error(qiet(validate_abc(model, xpriors, functions, observed), error_msg))
})

test_that("an error is raised with SLiM ABC on non-serialized models", {
  skip_if(Sys.which("slim") == "")
  expect_error(
    utils::capture.output(simulate_abc(model, priors, functions, observed, iterations = 1,
                 sequence_length = 1e6, recombination_rate = 0, engine = "slim")),
    "An unexpected error was raised when generating data from a slendr model
using the provided slendr function.

The error message received was:
It is not possible to simulate non-serialized models in SLiM"
  )
})

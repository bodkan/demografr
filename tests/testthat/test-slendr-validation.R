skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE))

library(slendr)
init_env(quiet = TRUE)

SEED <- 42
set.seed(SEED)

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

ts <- msprime(m, sequence_length = 1e6, recombination_rate = 0, random_seed = SEED)

samples <- ts_names(ts, split = "pop") %>% lapply(sample, 5)

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
  samples <- ts_names(ts, split = "pop") %>% lapply(sample, 5)
  ts_diversity(ts, sample_sets = samples, mode = "branch")
}
compute_divergence <- function(ts) {
  samples <- ts_names(ts, split = "pop") %>% lapply(sample, 5)
  ts_divergence(ts, sample_sets = samples, mode = "branch")
}
functions <- list(diversity = compute_diversity, divergence = compute_divergence)

test_that("correct ABC setups are validated", {
  expect_output(validate_abc(model, priors, functions, observed,
                             sequence_length = 1e6, recombination_rate = 0))
})

test_that("validation output can be silenced", {
  expect_silent(validate_abc(model, priors, functions, observed,
                             sequence_length = 1e6, recombination_rate = 0, quiet = TRUE))
})

test_that("incorrect priors are caught", {
  priors <- list(
    Ne_p1 ~ "hello",
    Ne_p2 ~ runif(10, 10000)
  )
  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "Sampling from the prior Ne_p1 resulted in the following problem:")

  priors <- list(
    Ne_p1 ~ runif(10, 10000),
    Ne_p2 ~ "hello"
  )
  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "Sampling from the prior Ne_p2 resulted in the following problem:")
})

test_that("only one return statement in a model function is allowed", {
  model <- function(Ne_p1, Ne_p2, Ne_p3, Ne_p4, T_p1_p2, T_p2_p3, T_p3_p4) {
    p1 <- population("p1", time = 1, N = 1000)

    return("another")

    model <- compile_model(populations = list(p1, p2, p3, p4), generation_time = 1, simulation_length = 10000)

    return(model)
  }
  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "A demografr model function must have exactly one return statement")
})

test_that("only `return(<model>)` and `return(list(<model>, <schedule>)` are allowed", {
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

    return(c(model, 123, 456))
  }
  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "A demografr model return statement must be:")
})

test_that("only priors corresponding to a function argument are allowed", {
  priors <- list(
    nonexistent ~ runif(1, 100),
    Ne_p2 ~ runif(10, 10000)
  )
  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "The following priors are not among the model function arguments")
})

test_that("consistent naming of summary functions and observed statistics is enforced", {
  error_msg <- "Elements of lists of summary functions and observed statistics must have the same names"

  xfunctions <- functions
  xfunctions$diversityyy <- xfunctions$diversity
  xfunctions$diversity <- NULL
  expect_error(validate_abc(model, priors, xfunctions, observed,
                            sequence_length = 1e6, recombination_rate = 0), error_msg)

  xobserved <- observed
  xobserved$diversityyy <- xobserved$diversity
  xobserved$diversity <- NULL
  expect_error(validate_abc(model, priors, functions, xobserved,
                            sequence_length = 1e6, recombination_rate = 0), error_msg)
})

test_that("errors in summary functions will be correctly reported", {
  error_msg <- "Computation of '.*' function on simulated tree sequence has failed"

  xfunctions <- functions
  xfunctions$diversity <- function(ts) stop("ERROR HERE!\n", call. = FALSE)
  expect_error(quiet(validate_abc(model, priors, xfunctions, observed,
                                  sequence_length = 1e6, recombination_rate = 0), error_msg))
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
  expect_error(validate_abc(model, priors, functions, xobserved,
                            sequence_length = 1e6, recombination_rate = 0), error_msg)
})

test_that("errors in prior sampling are correctly caught", {
  xpriors <- priors
  xpriors[[1]] <- Ne_popA ~ runif(1e6, 10000)
  error_msg <- "Sampling the prior Ne_popA resuted in the following problem"
  expect_error(qiet(validate_abc(model, xpriors, functions, observed,
                                 sequence_length = 1e6, recombination_rate = 0), error_msg))
})

test_that("an error is raised with SLiM ABC on non-serialized models", {
  skip_if(Sys.which("slim") == "")
  expect_error(
    suppressWarnings(utils::capture.output(simulate_abc(model, priors, functions, observed, iterations = 1,
                 sequence_length = 1e6, recombination_rate = 0, engine = "slim"))),
    "An unexpected error was raised when generating data from a slendr model
using the provided slendr function.

The error message received was:
It is not possible to simulate non-serialized models in SLiM"
  )
})

test_that("all model components must be present", {
  msg <- "A model to simulate from, priors, summary functions, and observed"

  model_ <- function(Ne_p1, Ne_p2, Ne_p3, Ne_p4, T_p1_p2, T_p2_p3, T_p3_p4) {
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
  pi_df <- ts_diversity(ts, sample_sets = samples, mode = "branch")
  d_df <- ts_divergence(ts, sample_sets = samples, mode = "branch")
  observed_ <- list(diversity = pi_df, divergence = d_df)

  priors_ <- list(
    Ne_p1 ~ runif(10, 10000),
    Ne_p2 ~ runif(10, 10000),
    Ne_p3 ~ runif(10, 10000),
    Ne_p4 ~ runif(10, 10000),

    T_p1_p2 ~ runif(100, 1500),
    T_p2_p3 ~ runif(1500, 3500),
    T_p3_p4 ~ runif(3000, 5000)
  )

  compute_diversity <- function(ts) {
    samples <- ts_names(ts, split = "pop") %>% lapply(sample, 5)
    ts_diversity(ts, sample_sets = samples, mode = "branch")
  }
  compute_divergence <- function(ts) {
    samples <- ts_names(ts, split = "pop") %>% lapply(sample, 5)
    ts_divergence(ts, sample_sets = samples, mode = "branch")
  }
  functions_ <- list(diversity = compute_diversity, divergence = compute_divergence)

  tmp <- model_
  rm(model_)
  expect_error(validate_abc(model_, priors, functions, observed, quiet = TRUE,
                            sequence_length = 1e6, recombination_rate = 0), msg)
  model_ <- tmp

  tmp <- priors_
  rm(priors_)
  expect_error(validate_abc(model, priors_, functions, observed, quiet = TRUE,
                            sequence_length = 1e6, recombination_rate = 0), msg)
  priors_ <- tmp

  tmp <- functions_
  rm(functions_)
  expect_error(validate_abc(model, priors, functions_, observed, quiet = TRUE,
                            sequence_length = 1e6, recombination_rate = 0), msg)
  functions_ <- tmp

  tmp <- observed_
  rm(observed_)
  expect_error(validate_abc(model, priors, functions, observed_, quiet = TRUE,
                            sequence_length = 1e6, recombination_rate = 0), msg)
  observed_ <- tmp

  expect_output(validate_abc(model_, priors_, functions_, observed_,
                             sequence_length = 1e6, recombination_rate = 0))
})

test_that("fully customized models must provide data-generating functions", {
  msg <- "Models which generate custom files require a list of data function\\(s\\)"
  quiet(expect_error(validate_abc(model, priors, functions, observed, format = "files"), msg))
})

test_that("pairs of summary statistics are enforced to be of the same format", {
  compute_diversity <- function(ts) { data.frame(a = "asdf", x = 123) }
  functions <- list(diversity = compute_diversity, divergence = compute_divergence)

  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "Dimensions of observed and simulated statistics differ")
})

test_that("all summary statistics must be either vectors or data frames (simulated)", {
  # simulated summary statistics
  compute_diversity <- function(ts) { list(list_is_an_incorrect_format = 123) }
  functions <- list(diversity = compute_diversity, divergence = compute_divergence)

  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "Observed and simulated statistics must be data frames or vectors")
})

test_that("all summary statistics must be either vectors or data frames (simulated)", {
  # observed summary statistics
  observed$diversity <- list(illegal_value = 123)

  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "Observed and simulated statistics must be data frames or vectors")
})

test_that("pairs of summary statistics are enforced to be of the same format", {
  compute_diversity <- function(ts) { data.frame(a = "asdf", x = 123) }
  functions <- list(diversity = compute_diversity, divergence = compute_divergence)

  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "Dimensions of observed and simulated statistics differ")
})

test_that("data-frame summary statistics must have the same columns", {
  # it doesn't seem to check the misformatted simulated statistics data frame
  # because that would be caught by an upstream validation step
  names(observed$diversity)[2] <- "different_diversity_name_like_pi"

  expect_error(validate_abc(model, priors, functions, observed,
                            sequence_length = 1e6, recombination_rate = 0, quiet = TRUE),
               "Columns of observed and simulated statistics must have the same names")
})


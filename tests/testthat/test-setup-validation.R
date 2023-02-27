skip_if(!slendr:::check_env_present())
init_env(quiet = TRUE)

model <- tree_model(tree = "(popA,(popB,(popC,popD)));", time_span = 10000)

priors <- list(
  Ne_popA ~ runif(1, 10000),
  Ne_popB ~ runif(1, 10000),
  Ne_popC ~ runif(1, 10000),
  Ne_popD ~ runif(1, 10000),

  Tsplit_popA_popB ~ runif(1, 3000),
  Tsplit_popB_popC ~ runif(3000, 6000),
  Tsplit_popC_popD ~ runif(6000, 9000)
)

compute_diversity <- function(ts) {
  samples <- slendr::ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
  slendr::ts_diversity(ts, sample_sets = samples) %>%
    dplyr::mutate(stat = paste0("pi_", set)) %>%
    dplyr::select(stat, value = diversity)
}
compute_divergence <- function(ts) {
  samples <- slendr::ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
  slendr::ts_divergence(ts, sample_sets = samples) %>%
    dplyr::mutate(stat = sprintf("d_%s_%s", x, y)) %>%
    dplyr::select(stat, value = divergence)
}
summary_funs <- list(diversity  = compute_diversity, divergence = compute_divergence)

ts <- slendr::msprime(model, sequence_length = 100000, recombination_rate = 0)
samples <- slendr::ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
diversity <- slendr::ts_diversity(ts, sample_sets = samples)  %>% dplyr::mutate(stat = paste0("pi_", set)) %>% dplyr::select(stat, value = diversity)
divergence <- slendr::ts_divergence(ts, sample_sets = samples)  %>% dplyr::mutate(stat = sprintf("d_%s_%s", x, y)) %>% dplyr::select(stat, value = divergence)
observed_stats <- list(diversity = diversity, divergence = divergence)

test_that("correct ABC setups are validated", {
  expect_output(validate_abc(model, priors, summary_funs, observed_stats))
})

test_that("consistent naming of summary functions and observed statistics is enforced", {
  error_msg <- "Lists of summary functions and observed statistics must have the same names"

  xsummary_funs <- summary_funs
  xsummary_funs$diversityyy <- xsummary_funs$diversity
  xsummary_funs$diversity <- NULL
  expect_error(validate_abc(model, priors, xsummary_funs, observed_stats), error_msg)

  xobserved_stats <- observed_stats
  xobserved_stats$diversityyy <- xobserved_stats$diversity
  xobserved_stats$diversity <- NULL
  expect_error(validate_abc(model, priors, summary_funs, xobserved_stats), error_msg)
})

test_that("errors in summary functions will be correctly reported", {
  error_msg <- "Computation of '.*' function on simulated tree sequence has failed"

  xsummary_funs <- summary_funs
  xsummary_funs$diversity <- function(ts) stop("ERROR HERE!\n", call. = FALSE)
  expect_error(quiet(validate_abc(model, priors, xsummary_funs, observed_stats), error_msg))
})

test_that("dimensions of simulated and observed summary statistics must be the same", {
  error_msg <- "Dimensions of observed and simulated statistics differ"

  xsummary_funs <- summary_funs
  xsummary_funs$diversity <- function(ts) {
    samples <- ts_samples(ts) %>% split(., .$pop) %>% lapply(`[[`, "name")
    ts_diversity(ts, sample_sets = samples) %>%
      mutate(stat = paste0("pi_", set)) %>%
      select(stat, value = diversity) %>%
      utils::head(1)
  }
  expect_error(quiet(validate_abc(model, priors, xsummary_funs, observed_stats), error_msg))
})

test_that("names of observed and simulated statistics must be the same", {
  error_msg <- "Lists of summary functions and observed statistics must have the same names"

  xobserved_stats <- observed_stats
  xobserved_stats$diversityyy <- xobserved_stats$diversity[1, ]
  xobserved_stats$diversity <- NULL
  expect_error(validate_abc(model, priors, summary_funs, xobserved_stats), error_msg)
})

test_that("errors in prior sampling are correctly caught", {
  xpriors <- priors
  xpriors[[1]] <- Ne_popA ~ runif(1e6, 10000)
  error_msg <- "Sampling the prior Ne_popA resuted in the following problem"
  expect_error(qiet(validate_abc(model, xpriors, summary_funs, observed_stats), error_msg))
})

test_that("an error is raised with SLiM ABC on non-serialized models", {
  expect_error(simulate_abc(model, priors, functions, observed, iterations = 1,
                            sequence_length = 1e6, recombination_rate = 0, engine = "SLiM"),
               "Non-serialized slendr model cannot be used")
})

test_that("a warning is given with msprime ABC on serialized models", {
  popA <- population("popA", time = 1, N = 1)
  popB <- population("popB", time = 2500, N = 1, parent = popA)
  popC <- population("popC", time = 5000, N = 1, parent = popB)
  popD <- population("popD", time = 7500, N = 1, parent = popC)

  xmodel <- compile_model(populations = list(popA, popB, popC, popD),
                          simulation_length = 10000, generation_time = 1, serialize = TRUE)

  expect_warning(simulate_abc(xmodel, priors, functions, observed, iterations = 1,
                              sequence_length = 1e6, recombination_rate = 0, engine = "msprime"),
                 "Model is serialized to disk which is unnecessary and inefficient")
})

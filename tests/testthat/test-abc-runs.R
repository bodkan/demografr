skip_if(!slendr::check_dependencies(python = TRUE))

library(slendr)
init_env(quiet = TRUE)

SEED <- 42

model <- function(Ne_p1, Ne_p2, Ne_p3, Ne_p4) {
  p1 <- population("p1", time = 1, N = 1000)
  p2 <- population("p2", time = 2000, N = 3000, parent = p1)
  p3 <- population("p3", time = 4000, N = 10000, parent = p2)
  p4 <- population("p4", time = 6000, N = 5000, parent = p3)

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

m <- model(100, 2000, 5000, 800)

ts <- msprime(m, sequence_length = 1e6, recombination_rate = 0, random_seed = 42)

set.seed(SEED)

samples <- ts_names(ts, split = "pop") %>% lapply(sample, 5)

pi_df <- ts_diversity(ts, sample_sets = samples, mode = "branch")

observed <- list(diversity = pi_df)

priors <- list(
  Ne_p1 ~ runif(10, 10000),
  Ne_p2 ~ runif(10, 10000),
  Ne_p3 ~ runif(10, 10000),
  Ne_p4 ~ runif(10, 10000)
)

compute_diversity <- function(ts) {
  samples <- ts_names(ts, split = "pop") %>% lapply(sample, 5)
  ts_diversity(ts, sample_sets = samples, mode = "branch")
}
functions <- list(diversity = compute_diversity)

test_that("validation passes", {
  expect_output(validate_abc(model, priors, functions, observed))
})

suppressPackageStartupMessages(run1 <- simulate_abc(model, priors, functions, observed, iterations = 2, sequence_length = 10000, recombination_rate = 0))
suppressPackageStartupMessages(run2 <- simulate_abc(model, priors, functions, observed, iterations = 2, sequence_length = 10000, recombination_rate = 0))
suppressPackageStartupMessages(run3 <- simulate_abc(model, priors, functions, observed, iterations = 2, sequence_length = 10000, recombination_rate = 0))

test_that("model functions must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same ABC setup but model functions differ"
  xrun1 <- run1
  xrun1$model <- ls
  expect_error(combine_data(xrun1, run2, run3), error_msg)
})

test_that("priors must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same ABC setup but priors differ"
  xrun1 <- run1
  xrun1$priors[[1]] <- NULL
  expect_error(combine_data(xrun1, run2, run3), error_msg)
})

test_that("summary functions must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same ABC setup but summary functions differ"
  xrun1 <- run1
  xrun1$functions[[1]] <- lm
  expect_error(combine_data(xrun1, run2, run3), error_msg)
})

test_that("observed statistics must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same ABC setup but observed statistics differ"
  xrun1 <- run1
  xrun1$observed[1] <- 42
  expect_error(combine_data(xrun1, run2, run3), error_msg)
})

test_that("both a list of runs and individual runs can be combined", {
  # individual runs as combine arguments
  expect_s3_class(runs_ind <- combine_data(run1, run2, run3), "demografr_abc_sims")

  expect_true(nrow(runs_ind$parameters) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run1$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run2$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run3$parameters))

  expect_true(nrow(runs_ind$simulated) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_ind$simulated) == ncol(run1$simulated))
  expect_true(ncol(runs_ind$simulated) == ncol(run2$simulated))
  expect_true(ncol(runs_ind$simulated) == ncol(run3$simulated))

  expect_true(all(runs_ind$observed$diversity == run1$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run2$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run3$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run1$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run2$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run3$observed$diversity))

  # list of runs
  expect_s3_class(runs_list <- combine_data(list(run1, run2, run3)), "demografr_abc_sims")

  expect_true(nrow(runs_list$parameters) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run1$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run2$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run3$parameters))

  expect_true(nrow(runs_list$simulated) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_list$simulated) == ncol(run1$simulated))
  expect_true(ncol(runs_list$simulated) == ncol(run2$simulated))
  expect_true(ncol(runs_list$simulated) == ncol(run3$simulated))

  expect_true(all(runs_list$observed$diversity == run1$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run2$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run3$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run1$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run2$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run3$observed$diversity))
})

test_that("simulation runs can be loaded in their serialized form", {
  f1 <- tempfile()
  f2 <- tempfile()
  f3 <- tempfile()
  saveRDS(run1, f1)
  saveRDS(run2, f2)
  saveRDS(run3, f3)

  # individual runs as combine arguments
  runs_ind <- combine_data(f1, f2, f3)
  expect_true(nrow(runs_ind$parameters) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run1$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run2$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run3$parameters))

  expect_true(nrow(runs_ind$simulated) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_ind$simulated) == ncol(run1$simulated))
  expect_true(ncol(runs_ind$simulated) == ncol(run2$simulated))
  expect_true(ncol(runs_ind$simulated) == ncol(run3$simulated))

  expect_true(all(runs_ind$observed$diversity == run1$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run2$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run3$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run1$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run2$observed$diversity))
  expect_true(all(runs_ind$observed$diversity == run3$observed$diversity))

  # list of runs
  expect_s3_class(runs_list <- combine_data(list(f1, f2, f3)), "demografr_abc_sims")

  expect_true(nrow(runs_list$parameters) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run1$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run2$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run3$parameters))

  expect_true(nrow(runs_list$simulated) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_list$simulated) == ncol(run1$simulated))
  expect_true(ncol(runs_list$simulated) == ncol(run2$simulated))
  expect_true(ncol(runs_list$simulated) == ncol(run3$simulated))

  expect_true(all(runs_list$observed$diversity == run1$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run2$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run3$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run1$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run2$observed$diversity))
  expect_true(all(runs_list$observed$diversity == run3$observed$diversity))
})

test_that("missing serialized files are correctly handled", {
  f1 <- tempfile()
  f2 <- tempfile()
  f3 <- tempfile()
  saveRDS(run1, f1)
  saveRDS(run2, f2)
  saveRDS(run3, f3)

  unlink(f1)
  expect_error(combine_data(f1, f2, f3), "File .* does not exist")
  expect_error(combine_data(list(f1, f2, f3)), "File .* does not exist")
})

test_that("'combining' a single ABC run does not break", {
  f1 <- tempfile()
  saveRDS(run1, f1)

  expect_s3_class(combine_data(f1), "demografr_abc_sims")
  expect_s3_class(combine_data(list(f1)), "demografr_abc_sims")
  expect_s3_class(combine_data(run1), "demografr_abc_sims")
  expect_s3_class(combine_data(list(run1)), "demografr_abc_sims")
})

test_that("abc::abc via run_abc() runs correctly", {
  data <- combine_data(run1, run2, run3)

  expect_error(run_abc(data, engine = "abc"), "The `tol` argument must be provided")
  expect_error(run_abc(data, engine = "abc", tol = 1), "The `method` argument must be provided")

  quiet(suppressWarnings(abc <- run_abc(data, engine = "abc", tol = 1, method = "neuralnet")))

  expect_s3_class(abc, "demografr_abc.abc")
})

test_that("abc predict() works correctly", {
  data <- combine_data(run1, run2, run3)

  quiet(suppressWarnings(abc <- run_abc(data, engine = "abc", tol = 0.3, method = "neuralnet")))

  expect_error(suppressWarnings(predict(abc)), "The number of samples to generate")
  expect_s3_class(suppressWarnings(predict(abc, samples = 3)), "data.frame")
  expect_true(suppressWarnings(nrow(predict(abc, samples = 3)) == 3))
})

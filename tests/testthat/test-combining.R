skip_if(!slendr:::check_env_present())
init_env(quiet = TRUE)

SEED <- 42

p1 <- slendr::population("popA", time = 1, N = 1000)
p2 <- slendr::population("popB", time = 2000, N = 3000, parent = p1)
p3 <- slendr::population("popC", time = 4000, N = 10000, parent = p2)
p4 <- slendr::population("popD", time = 6000, N = 5000, parent = p3)

model <- slendr::compile_model(
  populations = list(p1, p2, p3, p4),
  generation_time = 1,
  simulation_length = 10000, serialize = FALSE
)

ts <- slendr::msprime(model, sequence_length = 1e6, recombination_rate = 0, random_seed = SEED)

samples <- slendr::ts_samples(ts, split = TRUE)

pi_df <- slendr::ts_diversity(ts, sample_sets = samples) %>%
  dplyr::mutate(stat = paste0("pi_", set)) %>% dplyr::select(stat, value = diversity)

div_df <- slendr::ts_divergence(ts, sample_sets = samples) %>%
  dplyr::mutate(stat = sprintf("d_%s_%s", x, y)) %>% dplyr::select(stat, value = divergence)

observed <- list(diversity = pi_df, divergence = div_df)

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
  samples <- slendr::ts_samples(ts, split = TRUE)
  slendr::ts_diversity(ts, sample_sets = samples) %>%
    dplyr::mutate(stat = paste0("pi_", set)) %>%
    dplyr::select(stat, value = diversity)
}
compute_divergence <- function(ts) {
  samples <- slendr::ts_samples(ts, split = TRUE)
  slendr::ts_divergence(ts, sample_sets = samples) %>%
    dplyr::mutate(stat = sprintf("d_%s_%s", x, y)) %>%
    dplyr::select(stat, value = divergence)
}
functions <- list(diversity = compute_diversity, divergence = compute_divergence)

run1 <- simulate_abc(model, priors, functions, observed, iterations = 3, sequence_length = 10000, recombination_rate = 0)
run2 <- simulate_abc(model, priors, functions, observed, iterations = 3, sequence_length = 10000, recombination_rate = 0)
run3 <- simulate_abc(model, priors, functions, observed, iterations = 3, sequence_length = 10000, recombination_rate = 0)

test_that("model scaffolds must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same ABC setup but scaffolds\ndiffer"
  xrun1 <- run1
  xrun1$model$splits[1, ] <- -42
  expect_error(combine_abc(xrun1, run2, run3), error_msg)
})

test_that("priors must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same ABC setup but priors\ndiffer"
  xrun1 <- run1
  xrun1$priors[[1]] <- NULL
  expect_error(combine_abc(xrun1, run2, run3), error_msg)
})

test_that("summary functions must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same ABC setup but summary functions\ndiffer"
  xrun1 <- run1
  xrun1$functions[[1]] <- lm
  expect_error(combine_abc(xrun1, run2, run3), error_msg)
})

test_that("observed statistics must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same ABC setup but observed statistics\ndiffer"
  xrun1 <- run1
  xrun1$observed[1] <- 42
  expect_error(combine_abc(xrun1, run2, run3), error_msg)
})

test_that("both a list of runs and individual runs can be combined", {
  # individual runs as combine arguments
  expect_s3_class(runs_ind <- combine_abc(run1, run2, run3), "demografr_sims")

  expect_true(nrow(runs_ind$parameters) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run1$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run2$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run3$parameters))

  expect_true(nrow(runs_ind$simulated) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_ind$simulated) == ncol(run1$simulated))
  expect_true(ncol(runs_ind$simulated) == ncol(run2$simulated))
  expect_true(ncol(runs_ind$simulated) == ncol(run3$simulated))

  expect_true(nrow(runs_ind$observed) == nrow(run1$observed))
  expect_true(nrow(runs_ind$observed) == nrow(run2$observed))
  expect_true(nrow(runs_ind$observed) == nrow(run3$observed))
  expect_true(ncol(runs_ind$observed) == ncol(run1$observed))
  expect_true(ncol(runs_ind$observed) == ncol(run2$observed))
  expect_true(ncol(runs_ind$observed) == ncol(run3$observed))

  # list of runs
  expect_s3_class(runs_list <- combine_abc(run1, run2, run3), "demografr_sims")

  expect_true(nrow(runs_list$parameters) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run1$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run2$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run3$parameters))

  expect_true(nrow(runs_list$simulated) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_list$simulated) == ncol(run1$simulated))
  expect_true(ncol(runs_list$simulated) == ncol(run2$simulated))
  expect_true(ncol(runs_list$simulated) == ncol(run3$simulated))

  expect_true(nrow(runs_list$observed) == nrow(run1$observed))
  expect_true(nrow(runs_list$observed) == nrow(run2$observed))
  expect_true(nrow(runs_list$observed) == nrow(run3$observed))
  expect_true(ncol(runs_list$observed) == ncol(run1$observed))
  expect_true(ncol(runs_list$observed) == ncol(run2$observed))
  expect_true(ncol(runs_list$observed) == ncol(run3$observed))
})

test_that("simulation runs can be loaded in their serialized form", {
  f1 <- tempfile()
  f2 <- tempfile()
  f3 <- tempfile()
  saveRDS(run1, f1)
  saveRDS(run2, f2)
  saveRDS(run3, f3)

  # individual runs as combine arguments
  runs_ind <- combine_abc(f1, f2, f3)
  expect_true(nrow(runs_ind$parameters) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run1$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run2$parameters))
  expect_true(ncol(runs_ind$parameters) == ncol(run3$parameters))

  expect_true(nrow(runs_ind$simulated) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_ind$simulated) == ncol(run1$simulated))
  expect_true(ncol(runs_ind$simulated) == ncol(run2$simulated))
  expect_true(ncol(runs_ind$simulated) == ncol(run3$simulated))

  expect_true(nrow(runs_ind$observed) == nrow(run1$observed))
  expect_true(nrow(runs_ind$observed) == nrow(run2$observed))
  expect_true(nrow(runs_ind$observed) == nrow(run3$observed))
  expect_true(ncol(runs_ind$observed) == ncol(run1$observed))
  expect_true(ncol(runs_ind$observed) == ncol(run2$observed))
  expect_true(ncol(runs_ind$observed) == ncol(run3$observed))

  # list of runs
  expect_s3_class(runs_list <- combine_abc(f1, f2, f3), "demografr_sims")

  expect_true(nrow(runs_list$parameters) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run1$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run2$parameters))
  expect_true(ncol(runs_list$parameters) == ncol(run3$parameters))

  expect_true(nrow(runs_list$simulated) == nrow(run1$parameters) + nrow(run2$parameters) + nrow(run3$parameters))
  expect_true(ncol(runs_list$simulated) == ncol(run1$simulated))
  expect_true(ncol(runs_list$simulated) == ncol(run2$simulated))
  expect_true(ncol(runs_list$simulated) == ncol(run3$simulated))

  expect_true(nrow(runs_list$observed) == nrow(run1$observed))
  expect_true(nrow(runs_list$observed) == nrow(run2$observed))
  expect_true(nrow(runs_list$observed) == nrow(run3$observed))
  expect_true(ncol(runs_list$observed) == ncol(run1$observed))
  expect_true(ncol(runs_list$observed) == ncol(run2$observed))
  expect_true(ncol(runs_list$observed) == ncol(run3$observed))
})

test_that("missing serialized files are correctly handled", {
  f1 <- tempfile()
  f2 <- tempfile()
  f3 <- tempfile()
  saveRDS(run1, f1)
  saveRDS(run2, f2)
  saveRDS(run3, f3)

  unlink(f1)
  expect_error(combine_abc(f1, f2, f3), "File .* does not exist")
  expect_error(combine_abc(list(f1, f2, f3)), "File .* does not exist")
})

test_that("the function comparison hack works as intended", {
  funs1 <- list(mean, lm, print)
  funs2 <- list(mean, lm, print)
  expect_true(identical_functions(funs1, funs2))

  funs1 <- list(function() 1 + 2, function(x, y) print(x^y), function(z) hist(z))
  funs2 <- list(function() (1 + 2) - 1, function(x, y) print(x^y), function(z) hist(z))
  expect_false(identical_functions(funs1, funs2))
})

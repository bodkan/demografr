skip_if(!slendr:::is_slendr_env_present())
slendr::init_env(quiet = TRUE)

model <- function(Ne_p1, Ne_p2, Ne_p3, Ne_p4) {
  p1 <- slendr::population("p1", time = 1, N = 1000)
  p2 <- slendr::population("p2", time = 2000, N = 3000, parent = p1)

  model <- slendr::compile_model(
    populations = list(p1, p2),
    generation_time = 1,
    simulation_length = 10000, serialize = FALSE
  )

  return(model)
}

grid <- tidyr::crossing(
  Ne_p1 = c(10, 100),
  Ne_p2 = c(10, 100)
)

compute_diversity <- function(ts) {
  samples <- list(slendr::ts_samples(ts)$name)
  slendr::ts_diversity(ts, sample_sets = samples, mode = "branch")
}
functions <- list(diversity = compute_diversity)

nreps <- 2
run1 <- simulate_grid(model, grid, functions, replicates = nreps, sequence_length = 10000, recombination_rate = 0)
run2 <- simulate_grid(model, grid, functions, replicates = nreps, sequence_length = 10000, recombination_rate = 0)
run3 <- simulate_grid(model, grid, functions, replicates = nreps, sequence_length = 10000, recombination_rate = 0)

test_that("model functions must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same grid setup but model functions differ"
  xrun1 <- run1
  attr(xrun1, "model") <- ls
  expect_error(combine_data(xrun1, run2, run3), error_msg)
})

test_that("summary functions must be consistent between runs", {
  error_msg <- "Simulation runs must originate from the same grid setup but summary functions differ"
  xrun1 <- run1
  attr(xrun1, "functions")[[1]] <- lm
  expect_error(combine_data(xrun1, run2, run3), error_msg)
})

test_that("both a list of runs and individual runs can be combined", {
  # individual runs as combine arguments
  expect_s3_class(runs_ind <- combine_data(run1, run2, run3), "data.frame")

  expect_true(nrow(runs_ind) == nrow(run1) + nrow(run2) + nrow(run3))
  expect_true(min(runs_ind$rep) == 1)
  expect_true(max(runs_ind$rep) == 3 * nreps)

  # list of runs
  expect_s3_class(runs_list <- combine_data(list(run1, run2, run3)), "data.frame")

  expect_true(nrow(runs_list) == nrow(run1) + nrow(run2) + nrow(run3))
  expect_true(min(runs_list$rep) == 1)
  expect_true(max(runs_list$rep) == 3 * nreps)
})

test_that("simulation runs can be loaded in their serialized form", {
  f1 <- tempfile()
  f2 <- tempfile()
  f3 <- tempfile()
  saveRDS(run1, f1)
  saveRDS(run2, f2)
  saveRDS(run3, f3)

  # individual runs as combine arguments
  expect_s3_class(runs_ind <- combine_data(f1, f2, f3), "data.frame")

  expect_true(nrow(runs_ind) == nrow(run1) + nrow(run2) + nrow(run3))
  expect_true(min(runs_ind$rep) == 1)
  expect_true(max(runs_ind$rep) == 3 * nreps)

  # list of runs
  expect_s3_class(runs_list <- combine_data(list(f1, f2, f3)), "data.frame")

  expect_true(nrow(runs_list) == nrow(run1) + nrow(run2) + nrow(run3))
  expect_true(min(runs_list$rep) == 1)
  expect_true(max(runs_list$rep) == 3 * nreps)
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

test_that("'combining' a single grid run does not break", {
  f1 <- tempfile()
  saveRDS(run1, f1)

  expect_s3_class(combine_data(f1), "data.frame")
  expect_s3_class(combine_data(list(f1)), "data.frame")
  expect_s3_class(combine_data(run1), "data.frame")
  expect_s3_class(combine_data(list(run1)), "data.frame")
})

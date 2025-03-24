skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE))

library(slendr)
init_env(quiet = TRUE)

path_observed <- "custom_diversity.rds"
observed_diversity <- readRDS(path_observed)

priors <- list(Ne ~ runif(100, 5000))
observed <- list(diversity = observed_diversity)
compute_diversity_ts <- function(ts) {
  ts_diversity(ts, list(pop = seq(0, 99)))
}
functions <- list(diversity = compute_diversity_ts)
gens <- list(
  ts = function(path) ts_read(file.path(path, "result.trees"))
)

test_that("custom Python script can be used as a simulation engine", {
  python_script <- system.file("examples/custom.py", package = "demografr")
  model_run <- simulate_model(python_script, parameters = list(Ne = 123), format = "files", data = gens)

  expect_true(inherits(model_run, "list"))
  expect_s3_class(model_run$ts, "slendr_ts")
})

test_that("custom SLiM script can be used as a simulation engine", {
  slim_script <- system.file("examples/custom.slim", package = "demografr")
  quiet(model_run <- simulate_model(slim_script, parameters = list(Ne = 123), format = "files", data = gens))

  expect_true(inherits(model_run, "list"))
  expect_s3_class(model_run$ts, "slendr_ts")
})

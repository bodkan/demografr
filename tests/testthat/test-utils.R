test_that("sample_names() returns data in a correct format", {
  skip_if(!slendr:::check_env_present())

  p1 <- population("pop1", time = 1, N = 123)
  p2 <- population("pop2", parent = p1, time = 2, N = 456)

  model <- compile_model(
    populations = list(p1, p2),
    generation_time = 1, simulation_length = 300,
  )

  samples <- rbind(
    schedule_sampling(model, times = 10, list(p1, 2), list(p2, 2)),
    schedule_sampling(model, times = 250, list(p1, 10), list(p2, 10))
  )

  slim_ts <- tempfile(fileext = ".trees")
  msprime_ts <- tempfile(fileext = ".trees")

  slim(model, sequence_length = 100000, recombination_rate = 0, output = slim_ts,
      method = "batch", random_seed = 314159,
      samples = samples, verbose = FALSE)

  msprime(model, sequence_length = 100000, recombination_rate = 0, output = msprime_ts,
          random_seed = 314159, samples = samples, verbose = FALSE)

  ts_msprime <- ts_load(model, file = msprime_ts)
  ts_slim <- ts_load(model, file = slim_ts)

  expect_type(sample_names(ts_msprime), "character")
  expect_type(sample_names(ts_slim), "character")

  expect_type(sample_names(ts_msprime, split = TRUE), "list")
  expect_type(sample_names(ts_slim, split = TRUE), "list")

  expect_true(length(sample_names(ts_msprime, split = TRUE)) == length(unique(samples$pop)))
  expect_true(length(sample_names(ts_slim, split = TRUE)) == length(unique(samples$pop)))

  expect_true(length(sample_names(ts_msprime)) == sum(samples$n))
  expect_true(length(sample_names(ts_slim)) == sum(samples$n))
})

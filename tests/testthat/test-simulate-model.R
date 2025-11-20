skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE, quit = FALSE))

library(slendr)
init_env(quiet = TRUE)

priors <- list(N ~ 100)

model <- function(N) {
  model <- population("asdf", time = 100, N = N) %>%
    compile_model(simulation_length = 100, direction = "forward", generation_time = 1)
  return(model)
}

test_that("simulate_model produces files if instructed", {
  # data function list specified directly
  expect_true(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      format = "files",
      data = list(path = path)
    )$path %>% dir.exists()
  )

  # data function list given as a variable
  data_list <- list(path = function(path) path)
  expect_true(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      format = "files",
      data = data_list
    )$path %>% dir.exists()
  )
})

test_that("basic simulate_model() runs produce a tree-sequence file (SLiM)", {
  data_list <- list(path = function(path) path)
  result <- simulate_model(
    model, priors,
    sequence_length = 1e6, recombination_rate = 1e-8,
    engine = "slim",
    format = "files",
    data = data_list
  )
  norm_path <- normalizePath(file.path(result$path, "slim.trees"), winslash = "/", mustWork = FALSE)
  expect_s3_class(ts_read(norm_path, "slendr_ts"))
})

test_that("basic simulate_model() runs produce a tree-sequence file (msprime)", {
  data_list <- list(path = function(path) path)
  result <- simulate_model(
    model, priors,
    sequence_length = 1e6, recombination_rate = 1e-8,
    engine = "msprime",
    format = "files",
    data = data_list
  )
  norm_path <- normalizePath(file.path(result$path, "msprime.trees"), winslash = "/", mustWork = FALSE)
  expect_s3_class(ts_read(norm_path, "slendr_ts"))
})

test_that("for customized files, data-generating functions must be provided", {
  expect_error(
    simulate_model(model, priors, sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim", format = "files"),
    "Models which generate custom files require a list of data function\\(s\\)"
  )
})

test_that("format = 'ts' allows only 'ts' and 'model' to be available in data functions (SLiM)", {
  expect_error(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      format = "ts",
      data = list(
        ts = function(path, model) file.path(path, "slim.trees") %>% ts_read(model),
        gt = function(path, model) file.path(path, "slim.trees") %>% ts_read(model) %>% ts_mutate(1e-8) %>% ts_genotypes()
      )
    ),
    "The following function arguments are not valid: \"path\"."
  )

  expect_true(
    is.list(result <- simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      format = "files", data = list(
        ts = function(path, model) file.path(path, "slim.trees") %>% ts_read(model),
        gt = function(path, model) file.path(path, "slim.trees") %>% ts_read(model) %>% ts_mutate(1e-8) %>% ts_genotypes()
      )
    ))
  )

  expect_s3_class(result$ts, "slendr_ts")
  expect_s3_class(result$gt, "data.frame")
})

test_that("format = 'ts' allows only 'ts' and 'model' to be available in data functions (msprime)", {
  expect_error(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "msprime",
      format = "ts",
      data = list(
        ts = function(path, model) file.path(path, "slim.trees") %>% ts_read(model),
        gt = function(path, model) file.path(path, "slim.trees") %>% ts_read(model) %>% ts_mutate(1e-8) %>% ts_genotypes()
      )
    ),
    "The following function arguments are not valid: \"path\"."
  )

  expect_true(
    is.list(result <- simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "msprime",
      format = "files", data = list(
        ts = function(path, model) file.path(path, "msprime.trees") %>% ts_read(model),
        gt = function(path, model) file.path(path, "msprime.trees") %>% ts_read(model) %>% ts_mutate(1e-8) %>% ts_genotypes()
      )
    ))
  )

  expect_s3_class(result$ts, "slendr_ts")
  expect_s3_class(result$gt, "data.frame")
})

test_that("format = 'custom' allows only 'path' and 'model' to be available", {
  expect_error(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      format = "files",
      data = list(
        gt = function(ts) ts_mutate(ts, 1e-8) %>% ts_genotypes()
      )
    ),
    "The following function arguments are not valid: \"ts\""
  )

  expect_s3_class(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      format = "files",
      data = list(
        gt = function(path, model) file.path(path, "slim.trees") %>% ts_read(model) %>% ts_mutate(1e-8) %>% ts_genotypes()
      )
    )$gt,
    "data.frame"
  )
})

test_that("sampling from a grid works just as it does for priors", {
  grid <- data.frame(N = c(100, 1000, 10000))
  expect_s3_class(
    simulate_model(model, grid, sequence_length = 1e6, recombination_rate = 0),
    "slendr_ts"
  )
})

skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE, quit = FALSE))

library(slendr)
init_env(quiet = TRUE)

priors <- list(N ~ 100)

model <- function(N) {
  model <- population("asdf", time = 100, N = N) %>%
    compile_model(simulation_length = 100, direction = "forward", generation_time = 1)
  return(model)
}

test_that("simulate_model can also use non-functional data", {
  # data list directly
  expect_type(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      data_type = "custom",
      data = list(path = path)
    )$path,
    "character"
  )

  # data list as a variable
  data_list <- list(path = function(path) path)
  expect_type(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      data_type = "custom",
      data = data_list
    )$path,
    "character"
  )
})

test_that("for customized outputs, data-generating functions must be provided", {
  expect_error(
    simulate_model(model, priors, sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim", data_type = "custom"),
    "Models using custom data types must provide a list of data-generating function\\(s\\)"
  )
})

test_that("data_type = 'ts' allows only 'ts' and 'model' to be available", {
  expect_error(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      data = list(
        ts = function(path, model) file.path(path, "output.trees") %>% ts_load(model),
        gt = function(path, model) file.path(path, "output.trees") %>% ts_load(model) %>% ts_mutate(1e-8) %>% ts_genotypes
      )
    ),
    "The following function arguments are not valid: \"path\"."
  )

  expect_true(
    is.list(data <- simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      data_type = "custom", data = list(
        ts = function(path, model) file.path(path, "output.trees") %>% ts_load(model),
        gt = function(path, model) file.path(path, "output.trees") %>% ts_load(model) %>% ts_mutate(1e-8) %>% ts_genotypes
      )
    ))
  )

  expect_s3_class(data$ts, "slendr_ts")
  expect_s3_class(data$gt, "data.frame")
})

test_that("data_type = \"custom\" allows only \"path\" and \"model\" to be available", {
  expect_error(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      data_type = "custom",
      data = list(
        gt = function(ts) ts_mutate(ts, 1e-8) %>% ts_genotypes()
      )
    ),
    "The following function arguments are not valid: \"ts\"."
  )

  expect_s3_class(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      data_type = "custom",
      data = list(
        gt = function(path, model) file.path(path, "output.trees") %>% ts_load(model) %>% ts_mutate(1e-8) %>% ts_genotypes()
      )
    )$gt,
    "data.frame"
  )
})

test_that("custom output types are not allowed for slendr/msprime models", {
  expect_error(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "msprime",
      data_type = "custom",
      data = list(
        ts = ts,
        gt = function(ts) ts_mutate(ts, 1e-8) %>% ts_genotypes
      )
    ),
    "When using the slendr msprime engine, \"ts\" is the only valid output type."
  )

  # data list directly
  expect_true(
    is.list(data <- simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "msprime",
      data = list(
        ts = ts,
        gt = function(ts) ts_load(ts, model) %>% ts_mutate(1e-8) %>% ts_genotypes
      )
    ))
  )

  expect_s3_class(data$ts, "slendr_ts")
  expect_s3_class(data$gt, "data.frame")

  # data list as a variable
  data_list <- list(
      ts = function(ts, model) ts_load(ts, model),
      gt = function(ts, model) ts_load(ts, model) %>% ts_mutate(1e-8) %>% ts_genotypes
  )
  expect_true(
    is.list(data <- simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "msprime",
      data = data_list
    ))
  )

  expect_s3_class(data$ts, "slendr_ts")
  expect_s3_class(data$gt, "data.frame")
})

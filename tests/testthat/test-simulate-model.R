skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE, quit = FALSE))

library(slendr)
init_env(quiet = TRUE)

priors <- list(N ~ 100)

model <- function(N) {
  model <- population("asdf", time = 100, N = N) %>%
    compile_model(simulation_length = 100, direction = "forward", generation_time = 1)
  return(model)
}

test_that("simulate_model can also use non-functional outputs", {
  expect_type(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      output_type = "custom",
      outputs = list(path = path)
    )$path,
    "character"
  )
})

test_that("output_type = 'ts' allows only 'ts' and 'model' to be available", {
  expect_error(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      outputs = list(
        ts = function(path, model) file.path(path, "output.trees") %>% ts_load(model),
        gt = function(path, model) file.path(path, "output.trees") %>% ts_load(model) %>% ts_mutate(1e-8) %>% ts_genotypes
      )
    ),
    "The following arguments are not valid: \"path\"."
  )

  expect_true(
    is.list(outputs <- simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      output_type = "custom", outputs = list(
        ts = function(path, model) file.path(path, "output.trees") %>% ts_load(model),
        gt = function(path, model) file.path(path, "output.trees") %>% ts_load(model) %>% ts_mutate(1e-8) %>% ts_genotypes
      )
    ))
  )

  expect_s3_class(outputs$ts, "slendr_ts")
  expect_s3_class(outputs$gt, "data.frame")
})

test_that("output_type = \"custom\" allows only \"path\" and \"model\" to be available", {
  expect_error(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      output_type = "custom",
      outputs = list(
        gt = function(ts) ts_mutate(ts, 1e-8) %>% ts_genotypes()
      )
    ),
    "The following arguments are not valid: \"ts\"."
  )

  expect_s3_class(
    simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "slim",
      output_type = "custom",
      outputs = list(
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
      output_type = "custom",
      outputs = list(
        ts = ts,
        gt = function(ts) ts_mutate(ts, 1e-8) %>% ts_genotypes
      )
    ),
    "When using the slendr msprime engine, \"ts\" is the only valid output type."
  )

  expect_true(
    is.list(outputs <- simulate_model(
      model, priors,
      sequence_length = 1e6, recombination_rate = 1e-8,
      engine = "msprime",
      outputs = list(
        ts = ts,
        gt = function(ts) ts_load(ts, model) %>% ts_mutate(1e-8) %>% ts_genotypes
      )
    ))
  )

  expect_s3_class(outputs$ts, "slendr_ts")
  expect_s3_class(outputs$gt, "data.frame")
})

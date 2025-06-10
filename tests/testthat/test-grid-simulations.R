skip_if(!slendr::check_dependencies(python = TRUE))

library(slendr)
init_env(quiet = TRUE)

model <- function(Ne_A, Ne_B, Ne_C, Ne_D, T_1, T_2, T_3) {
  popA <- population("popA", time = 1,   N = Ne_A)
  popB <- population("popB", time = T_1, N = Ne_B, parent = popA)
  popC <- population("popC", time = T_2, N = Ne_C, parent = popB)
  popD <- population("popD", time = T_3, N = Ne_D, parent = popC)

  model <- compile_model(
    populations = list(popA, popB, popC, popD),
    generation_time = 1, simulation_length = 10000,
    direction = "forward"
  )

  return(model)
}

compute_diversity <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  ts_diversity(ts, sample_sets = samples)
}
compute_divergence <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  ts_divergence(ts, sample_sets = samples)
}
compute_f4 <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  ts_f4(ts,
        W = list(popA = samples$popA),
        X = list(popB = samples$popB),
        Y = list(popC = samples$popC),
        Z = list(popD = samples$popD))
}

functions <- list(diversity = compute_diversity, divergence = compute_divergence, f4 = compute_f4)

test_that("broken grid leads to an expected error (strict = TRUE)", {
  grid <- data.frame(
    Ne_A = 1, Ne_B = 1, Ne_C = 1, Ne_D = 1,
    T_1 = c(10, 20, 30), T_2 = c(10, 20, 30), T_3 = c(10, 20, 30)
  )
  expect_error(
    # A new release of future_apply appears to have changed handling of the case
    # of some iterations failing, which now results in a warning:
    #    "Warning: Caught simpleError. Canceling all iterations ..."
    # This slightly messes up our own error handling because we don't need
    # this warning. In this test, let's therefore silence this warning and check
    # that our error is produced too.
    suppressWarnings(simulate_grid(model, grid, functions, replicates = 1,
                  sequence_length = 1e5, recombination_rate = 0, strict = TRUE)),
    "An unexpected error was raised"
  )
})

test_that("inconsistent parameter grid leads to an expected error (strict = TRUE)", {
  grid <- data.frame(
    Ne_A = 1, Ne_B = 1, Ne_C = 1, Ne_D = 1,
    T_1 = 30, T_2 = 20, T_3 = 10
  )
  expect_error(
    suppressWarnings(simulate_grid(model, grid, functions, replicates = 1,
                     sequence_length = 1e5, recombination_rate = 0, strict = TRUE)),
    "The model implies forward time direction but the specified split"
  )
})

test_that("completely invalid grids lead to an error", {
  grid <- data.frame(
    Ne_A = 1, Ne_B = 1, Ne_C = 1, Ne_D = 1,
    T_1 = c(40, 30), T_2 = c(20, 10), T_3 = c(3, 1)
  )
  expect_error(
    suppressMessages(simulate_grid(model, grid, functions, replicates = 1,
                                   sequence_length = 1e5, recombination_rate = 0, strict = FALSE)),
    "All model simulation runs were invalid"
  )
})

test_that("partially invalid grids only retain valid simulations", {
  grid <- data.frame(
    Ne_A = 1, Ne_B = 1, Ne_C = 1, Ne_D = 1,
    T_1 = c(10, 20, 30, 10), T_2 = c(10, 20, 30, 20), T_3 = c(10, 20, 30, 30)
  )
  expect_s3_class(
    suppressMessages(
      res <- simulate_grid(model, grid, functions, replicates = 1,
                           sequence_length = 1e5, recombination_rate = 0, strict = FALSE)),
    "data.frame"
  )
  expect_true(nrow(res) == 1)
})

test_that("valid grids results in an appropriately sized data frame", {
  grid <- data.frame(
    Ne_A = 1, Ne_B = 1, Ne_C = 1, Ne_D = 1,
    T_1 = c(2, 10), T_2 = c(3, 20), T_3 = c(4, 30)
  )
  expect_s3_class(
    suppressMessages(res <- simulate_grid(model, grid, functions, replicates = 1,
                              sequence_length = 1e5, recombination_rate = 0, strict = FALSE)),
    "data.frame"
  )
  expect_true(nrow(res) == 2)
})

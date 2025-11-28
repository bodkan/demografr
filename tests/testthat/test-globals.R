# this batch of tests verifies that "external" functions and symbols used by
# summary statistic functions can be pulled into parallelized runs performed
# by all simulation routines

skip_if(!slendr::check_dependencies(python = TRUE))
skip_on_cran()

library(slendr)
init_env(quiet = TRUE)

library(future)
plan(multisession, workers = 3)

model <- function(Ne_A, Ne_B, Ne_C, Ne_D, T_1, T_2, T_3, gf) {
  A <- population("A", time = 1,   N = Ne_A)
  B <- population("B", time = T_1, N = Ne_B, parent = A)
  C <- population("C", time = T_2, N = Ne_C, parent = B)
  D <- population("D", time = T_3, N = Ne_D, parent = C)

  gf <- gene_flow(from = B, to = C, start = 9000, end = 9301, rate = gf)

  model <- compile_model(
    populations = list(A, B, C, D), gene_flow = gf,
    generation_time = 1, simulation_length = 10000,
    direction = "forward", serialize = FALSE
  )

  samples <- schedule_sampling(
    model, times = 10000,
    list(A, 50), list(B, 50), list(C, 50), list(D, 50),
    strict = TRUE
  )

  return(list(model, samples))
}

priors <- list(
  Ne... ~ runif(100, 10000),

  T_1   ~ runif(1,    4000),
  T_2   ~ runif(3000, 9000),
  T_3   ~ runif(5000, 10000),

  gf    ~ runif(0, 1)
)

observed_diversity <- read.table(system.file("examples/basics_diversity.tsv", package = "demografr"), header = TRUE)
observed_divergence <- read.table(system.file("examples/basics_divergence.tsv", package = "demografr"), header = TRUE)
observed_f4 <- read.table(system.file("examples/basics_f4.tsv", package = "demografr"), header = TRUE)
observed <- list(diversity  = observed_diversity, divergence = observed_divergence, f4 = observed_f4)

abc <- readRDS(system.file("examples/basics_abc.rds", package = "demografr"))

par_grid <- structure(
  list(T_1 = c(1797.53701523009, 3050.18120482161, 2372.6227705764),
       T_2 = c(5248.24362379375, 7577.25178638202, 6710.65408732185),
       T_3 = c(9523.46865690348, 8936.26090924774, 9685.32653747777),
       gf = c(0.0162311367169201, 0.0662672568074403, 0.687938508366051),
       Ne_A = c(1857.86374449629, 2365.27494890084, 2018.62402997767),
       Ne_B = c(726.097407571101, 1222.33371060468, 1262.86765690345),
       Ne_C = c(9459.1091039811, 3197.0082336752, 9451.35153361622),
       Ne_D = c(4588.7814708209, 2109.3942827438, 4504.7667624409),
       rep = c(1L, 1L, 1L)),
  row.names = c(NA, -3L),
  class = c("tbl_df",  "tbl", "data.frame")
)

# 'self-contained' summary functions --------------------------------------

compute_diversity1 <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  ts_diversity(ts, sample_sets = samples)
}
compute_divergence1 <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  ts_divergence(ts, sample_sets = samples)
}
compute_f41 <- function(ts) {
  samples <- ts_names(ts, split = "pop")
  A <- samples["A"]; B <- samples["B"]
  C <- samples["C"]; D <- samples["D"]
  ts_f4(ts, A, B, C, D)
}

functions1 <- list(diversity = compute_diversity1, divergence = compute_divergence1, f4 = compute_f41)

test_that("simulation routines all succeed with 'self-contained' summary functions", {
  expect_s3_class(abc1 <- simulate_abc(model, priors, functions1, observed, iterations = 3, sequence_length = 1e6, recombination_rate = 0), "demografr_abc_sims")
  expect_s3_class(pred1 <- predict(abc, samples = 3, posterior = "unadj", functions = functions1), "data.frame")
  expect_s3_class(grid1 <- simulate_grid(model, grid = par_grid, functions = functions1, sequence_length = 1e6, recombination_rate = 0, rep = 3), "data.frame")
})

# summary functions depending on an 'external' function -------------------

get_pop_samples <- function(ts) { ts_names(ts, split = "pop") }

compute_diversity2 <- function(ts) { ts_diversity(ts, sample_sets = get_pop_samples(ts)) }
compute_divergence2 <- function(ts) { ts_divergence(ts, sample_sets = get_pop_samples(ts)) }
compute_f42 <- function(ts) {
  samples <- get_pop_samples(ts)
  A <- samples["A"]; B <- samples["B"]
  C <- samples["C"]; D <- samples["D"]
  ts_f4(ts, A, B, C, D)
}

functions2 <- list(diversity = compute_diversity2, divergence = compute_divergence2, f4 = compute_f42)

test_that("simulation routines fail with missing 'external' function in summary functions", {
  # fails on not providing the external `get_pop_samples` symbol
  expect_error(suppressWarnings(abc2 <- simulate_abc(model, priors, functions2, observed, iterations = 10, sequence_length = 1e6, recombination_rate = 0)))
  expect_error(suppressMessages(suppressWarnings(pred2 <- predict(abc, samples = 3, posterior = "unadj", functions = functions2))))
  expect_error(suppressWarnings(grid2 <- simulate_grid(model, grid = par_grid, functions = functions2, sequence_length = 1e6, recombination_rate = 0, rep = 3)))
})

test_that("simulation routines succeed when 'external' function is provided", {
  # succeeds on providing the external `get_pop_samples` symbol
  expect_s3_class(abc2 <- simulate_abc(model, priors, functions2, observed, iterations = 3, sequence_length = 1e6, recombination_rate = 0, globals = "get_pop_samples"), "demografr_abc_sims")
  expect_s3_class(pred2 <- predict(abc, samples = 3, posterior = "unadj", functions = functions2, globals = "get_pop_samples"), "data.frame")
  expect_s3_class(grid2 <- simulate_grid(model, grid = par_grid, functions = functions2, sequence_length = 1e6, recombination_rate = 0, rep = 3, globals = "get_pop_samples"), "data.frame")
})

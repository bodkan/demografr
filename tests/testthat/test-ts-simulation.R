skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE))
slendr::init_env(quiet = TRUE)

SEED <- 42

model <- function(Ne_p1, Ne_p2, Ne_p3, Ne_p4) {
  p1 <- slendr::population("p1", time = 1, N = 1000)
  p2 <- slendr::population("p2", time = 20, N = 3000, parent = p1)
  p3 <- slendr::population("p3", time = 40, N = 10000, parent = p2)
  p4 <- slendr::population("p4", time = 60, N = 5000, parent = p3)

  model <- slendr::compile_model(
    populations = list(p1, p2, p3, p4),
    generation_time = 1,
    simulation_length = 100
  )

  return(model)
}

test_that("simulate_model generates a tree sequence from priors", {
  priors <- list(
    Ne_p1 ~ runif(1, 100),
    Ne_p2 ~ runif(1, 100),
    Ne_p3 ~ runif(1, 100),
    Ne_p4 ~ runif(1, 100)
  )

  ts1 <- simulate_model(model, priors, engine = "msprime", sequence_length = 1e6, recombination_rate = 0)
  expect_s3_class(ts1, "slendr_ts")

  skip_if(Sys.which("slim") == "")
  ts2 <- simulate_model(model, priors, engine = "slim", sequence_length = 1e6, recombination_rate = 0)
  expect_s3_class(ts2, "slendr_ts")
})

test_that("simulate_model generates a tree sequence from a parameter list", {
  params <- list(Ne_p1 = 1, Ne_p2 = 2, Ne_p3 = 3, Ne_p4 = 4)

  ts1 <- simulate_model(model, params, engine = "msprime", sequence_length = 1e6, recombination_rate = 0)
  expect_s3_class(ts1, "slendr_ts")

  skip_if(Sys.which("slim") == "")
  ts2 <- simulate_model(model, params, engine = "slim",, sequence_length = 1e6, recombination_rate = 0)
  expect_s3_class(ts2, "slendr_ts")
})

skip_if(!slendr::check_dependencies(python = TRUE))
slendr::init_env(quiet = TRUE)

model <- function(N_a, N_b, T_a1, T_a2, T_a3, T_a4, T_a5, T_b1, T_b2, T_b3, T_b4, T_b5) {
  a1 <- slendr::population("a1", time = T_a1, N = N_a)
  a2 <- slendr::population("a2", time = T_a2, N = N_a, parent = a1)
  a3 <- slendr::population("a3", time = T_a3, N = N_a, parent = a1)
  a4 <- slendr::population("a4", time = T_a4, N = N_a, parent = a1)
  a5 <- slendr::population("a5", time = T_a5, N = N_a, parent = a1)

  b1 <- slendr::population("b1", time = T_b1, N = N_b, parent = a1)
  b2 <- slendr::population("b2", time = T_b2, N = N_b, parent = b1)
  b3 <- slendr::population("b3", time = T_b3, N = N_b, parent = b1)
  b4 <- slendr::population("b4", time = T_b4, N = N_b, parent = b1)
  b5 <- slendr::population("b5", time = T_b5, N = N_b, parent = b1)

  model <- slendr::compile_model(
    list(a1, a2, a3, a4, a5, b1, b2, b3, b4, b5),
    generation_time = 30, direction = "backward")

  return(model)
}

individual_priors <- list(
  N_a ~ runif(10, 3000),
  N_b ~ runif(1000, 7000),

  T_a1 ~ runif(1, 100000),
  T_a2 ~ runif(1, 100000),
  T_a3 ~ runif(1, 100000),
  T_a4 ~ runif(1, 100000),
  T_a5 ~ runif(1, 100000),
  T_b1 ~ runif(50000, 100000),
  T_b2 ~ runif(50000, 100000),
  T_b3 ~ runif(50000, 100000),
  T_b4 ~ runif(50000, 100000),
  T_b5 ~ runif(50000, 100000)
)

# generate fake setup data for a testing ABC
ts <- simulate_ts(model, individual_priors, sequence_length = 1)
functions <- list(diversity = function(ts) { slendr::ts_diversity(ts, sample_sets = 0:10) })
observed <- list(diversity = functions$diversity(ts))

test_that("Nonsensical priors are correctly caught", {
  error_msg <- "Parameters must be given as"
  expect_error(simulate_ts(model, parameters = "asdf"), error_msg)
  expect_error(simulate_ts(model, parameters = list(123)), error_msg)
  expect_error(simulate_ts(model, parameters = list(123, xyz = 42)), error_msg)
})

test_that("Normal prior sampling expressions validate correctly", {
  expect_output(validate_abc(model, individual_priors, functions, observed))
})

templated_priors <- list(
  T_a... ~ runif(1,     100000),
  T_b... ~ runif(50000, 100000),
  N_a    ~ runif(10,    3000),
  N_b    ~ runif(1000,  7000)
)

test_that("Templated prior sampling expressions validate correctly", {
  expect_output(validate_abc(model, templated_priors, functions, observed))
})

test_that("expand_fromulas() produces parameters equal to formal arguments of a model", {
  expanded_priors <- expand_formulas(templated_priors, model)
  expect_true(all(get_prior_names(expanded_priors) == names(formals(model))))
})

test_that("With the same seed, both sets of priors give the same tree sequence", {
  expanded_priors <- expand_formulas(templated_priors, model)

  # first seed is for prior sampling, second for tree sequence generation

  set.seed(42)
  ts1 <- simulate_ts(model, individual_priors, sequence_length = 1, engine_args = list(random_seed = 42), mutation_rate = 1e-8)

  set.seed(42)
  ts2 <- simulate_ts(model, expanded_priors, sequence_length = 1, engine_args = list(random_seed = 42), mutation_rate = 1e-8)

  set.seed(42)
  ts3 <- simulate_ts(model, templated_priors, sequence_length = 1, engine_args = list(random_seed = 42), mutation_rate = 1e-8)

  expect_equal(slendr::ts_table(ts1, "nodes"),       slendr::ts_table(ts2, "nodes"))
  expect_equal(slendr::ts_table(ts1, "edges"),       slendr::ts_table(ts2, "edges"))
  expect_equal(slendr::ts_table(ts1, "individuals"), slendr::ts_table(ts2, "individuals"))
  expect_equal(slendr::ts_table(ts1, "mutations"),   slendr::ts_table(ts2, "mutations"))

  expect_equal(slendr::ts_table(ts2, "edges"),       slendr::ts_table(ts3, "edges"))
  expect_equal(slendr::ts_table(ts2, "edges"),       slendr::ts_table(ts3, "edges"))
  expect_equal(slendr::ts_table(ts2, "individuals"), slendr::ts_table(ts3, "individuals"))
  expect_equal(slendr::ts_table(ts2, "mutations"),   slendr::ts_table(ts3, "mutations"))
})

test_that("expand_formulas() catches multiple template matches", {
  templated_priors <- list(
    T_a1... ~ runif(1,     100000),
    T_a...  ~ runif(50000, 100000),
    T_b...  ~ runif(50000, 100000),
    N_a     ~ runif(10,    3000),
    N_b     ~ runif(1000,  7000)
  )
  expect_error(
    simulate_ts(model, templated_priors),
    "Multiple matching priors for the model parameter 'T_a1'"
  )
})

test_that("Sampling from empty prior list is correctly caught", {
  broken_prior <- list()
  expect_error(sample_prior(broken_prior), "A prior expression must take a form of an R formula")
  expect_error(simulate_ts(model, broken_prior),
               "A model generating function, parameters and sequence information must be provided")
})

test_that("Catch attempts at templating of vectorized priors", {
  error_msg <- "Templating of vector priors is not supported"
  broken_prior <- T_a...[10]  ~ runif(50000, 100000)
  expect_error(sample_prior(broken_prior), error_msg)
  expect_error(quiet(simulate_ts(model, list(broken_prior))), error_msg)
})

test_that("catch attempts at templating of vectorized priors", {
  broken_priors <- list(
    T_a...[10] ~ runif(1,     100000),
    T_b... ~ runif(50000, 100000),
    N_a    ~ runif(10,    3000),
    N_b    ~ runif(1000,  7000)
  )

  expect_error(simulate_ts(model, broken_priors),
               "Templating of vector priors is not supported")
})

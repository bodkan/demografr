skip_if(!slendr:::is_slendr_env_present())
skip_if(Sys.which("slim") == "")
slendr::init_env(quiet = TRUE)

p1 <- slendr::population("pop1", time = 1, N = 123)
p2 <- slendr::population("pop2", parent = p1, time = 2, N = 456)

model <- slendr::compile_model(list(p1, p2), generation_time = 1, simulation_length = 100)

samples <- rbind(
  slendr::schedule_sampling(model, times = c(3, 10), list(p1, 2), list(p2, 2), strict = TRUE),
  slendr::schedule_sampling(model, times = 101, list(p1, 10), list(p2, 10), strict = TRUE)
)

slim_ts <- tempfile(fileext = ".trees")
msprime_ts <- tempfile(fileext = ".trees")

slendr::slim(model, sequence_length = 100000, recombination_rate = 0, output = slim_ts,
    method = "batch", random_seed = 314159, samples = samples, verbose = FALSE, load = FALSE)

slendr::msprime(model, sequence_length = 100000, recombination_rate = 0, output = msprime_ts,
        random_seed = 314159, verbose = FALSE, load = FALSE, samples = samples)

ts_slim <- slendr::ts_load(model, file = slim_ts)
ts_msprime <- slendr::ts_load(model, file = msprime_ts)

# check correctness of recorded sampling times:
# library(dplyr)
# unique(slendr::ts_samples(ts_slim)$time)
# slendr::ts_nodes(ts_slim) %>% filter(sampled) %>% .$time %>% unique
# slendr::ts_nodes(ts_slim) %>% filter(sampled) %>% .$time_tskit %>% unique
# unique(slendr::ts_samples(ts_msprime)$time)
# slendr::ts_nodes(ts_msprime) %>% filter(sampled) %>% .$time %>% unique
# slendr::ts_nodes(ts_msprime) %>% filter(sampled) %>% .$time_tskit %>% unique

test_that("extract_names(ts) returns a simple character vector", {
  expect_type(extract_names(ts_msprime), "character")
  expect_type(extract_names(ts_slim), "character")

  expect_true(length(extract_names(ts_msprime)) == sum(samples$n))
  expect_true(length(extract_names(ts_slim)) == sum(samples$n))
})

test_that("extract_names(ts, split = 'pop') returns a list of character vectors", {
  expect_type(extract_names(ts_msprime, split = "pop"), "list")
  expect_type(extract_names(ts_slim, split = "pop"), "list")

  expect_true(length(extract_names(ts_msprime, split = "pop")) == 2)
  expect_true(length(extract_names(ts_slim, split = "pop")) == 2)
})

test_that("extract_names(ts, split = 'time') returns a list of character vectors", {
  expect_type(extract_names(ts_msprime, split = "time"), "list")
  expect_type(extract_names(ts_slim, split = "time"), "list")

  expect_true(length(extract_names(ts_msprime, split = "time")) == length(unique(samples$time)))
  expect_true(length(extract_names(ts_slim, split = "time")) == length(unique(samples$time)))
})

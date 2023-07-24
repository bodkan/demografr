skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE))
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

# TODO: extract_names() used to be tested here -- add tests of other internal utility functions
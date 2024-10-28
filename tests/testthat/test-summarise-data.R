skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE, quit = FALSE))

library(slendr)
init_env(quiet = TRUE)

priors <- list(N ~ 100)

extension <- r"(
initialize() {
    // negative s to silence SLiM warning about neutral mutations
    initializeMutationType("m1", 0.5, "f", -0.0001);

    initializeGenomicElementType("g1", m1, 1.0);
    initializeGenomicElement(g1, 0, SEQUENCE_LENGTH - 1);

    initializeMutationRate(1e-8);
    initializeRecombinationRate(RECOMBINATION_RATE);
}

SIMULATION_END late() {
    ts_file = PATH + "/slim.trees";
    stats_file = PATH + "/stats.tsv";

    // save tree sequence
    save_ts(ts_file);

    // save summary statistics values
    writeFile(stats_file, "het_pop1\thet_pop2\tfst");
    het_pop1 = calcHeterozygosity(population("pop1").genomes);
    het_pop2 = calcHeterozygosity(population("pop2").genomes);
    fst = calcFST(
        population("pop1").genomes,
        population("pop2").genomes
    );
    writeFile(stats_file, het_pop1 + "\t" + het_pop2 + "\t" + fst, append = T);
}
)"

model <- function(split_time) {
  pop_anc <- population("pop_anc", time = 1, N = 1000)
  pop1 <- population("pop1", time = split_time, N = 1000, parent = pop_anc)
  pop2 <- population("pop2", time = split_time, N = 1000, parent = pop_anc)

  model <- compile_model(
    list(pop_anc, pop1, pop2),
    simulation_length = 1000, direction = "forward", generation_time = 1,
    extension = extension
  )

  samples <- schedule_sampling(model, time = 1001, list(pop1, 25), list(pop2, 25))

  return(list(model = model, samples = samples))
}

# m <- model(10)
# slim(m$model, samples = m$samples, sequence_length = 1e6, recombination_rate = 1e-8, path = "/tmp/testing")

data <- simulate_model(
  model, parameters = list(split_time = 100),
  sequence_length = 1e6, recombination_rate = 1e-8,
  engine = "slim",
  format = "files",
  data = list(
    ts = function(path, model) file.path(path, "slim.trees") %>% ts_read(model),
    samples = function(path, model) file.path(path, "slim.trees") %>% ts_read(model) %>% ts_names(split = "pop"),
    stats = function(path) file.path(path, "stats.tsv") %>% read.table(header = TRUE)
  )
)

test_that("summarise_data refuses functions with invalid arguments", {
  functions <- list(stat = function(invalid_arg) random_function(invalid_arg))
  expect_error(summarise_data(data, functions),
               "The following function arguments are not valid: \"invalid_arg\".")

  functions <- list(pi = function(ts, samples) ts_diversity(ts, samples),
                    stat = function(invalid_arg) random_function(invalid_arg))
  expect_error(summarise_data(data, functions),
               "The following function arguments are not valid: \"invalid_arg\".")
})

test_that("summarise_data produces data frames for every summary function", {
  functions <- list(
    pi = function(ts, samples) ts_diversity(ts, samples),
    fst = function(ts, samples) ts_fst(ts, samples)
  )
  expect_type(output <- summarise_data(data, functions), "list")
  expect_true(all(sapply(output, is.data.frame)))
})

test_that("summarise_data can utilize pre-computed statistics as data (pre-defined data functions)", {
  data_functions <- list(
    ts_pi = function(path, model) {
      ts <- file.path(path, "slim.trees") %>% ts_read(model)
      ts_diversity(ts, ts_names(ts, split = "pop"))
    },
    slim_pi = function(path) file.path(path, "stats.tsv") %>%
      read.table(header = TRUE) %>%
      .[, c("het_pop1", "het_pop2")]
  )

  data <- simulate_model(
    model, parameters = list(split_time = 100),
    sequence_length = 1e6, recombination_rate = 1e-8,
    engine = "slim",
    format = "files",
    data = data_functions
  )

  expect_type(summarise_data(data, functions = list(ts_pi = ts_pi, slim_pi = slim_pi)), "list")
  expect_type(summarise_data(data, functions = list(ts_pi = function(ts_pi) ts_pi, slim_pi = function(slim_pi) slim_pi)), "list")
})

test_that("summarise_data can utilize pre-computed statistics as data (inline data functions)", {
  data <- simulate_model(
    model, parameters = list(split_time = 100),
    sequence_length = 1e6, recombination_rate = 1e-8,
    engine = "slim",
    format = "files",
    data = list(
      ts_pi = function(path, model) {
        ts <- file.path(path, "slim.trees") %>% ts_read(model)
        ts_diversity(ts, ts_names(ts, split = "pop"))
      },
      slim_pi = function(path) file.path(path, "stats.tsv") %>%
        read.table(header = TRUE) %>%
        .[, c("het_pop1", "het_pop2")]
    )
  )

  expect_type(summarise_data(data, functions = list(ts_pi = ts_pi, slim_pi = slim_pi)), "list")
  expect_type(summarise_data(data, functions = list(ts_pi = function(ts_pi) ts_pi, slim_pi = function(slim_pi) slim_pi)), "list")
})

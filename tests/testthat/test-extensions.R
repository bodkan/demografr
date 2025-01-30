skip_if_not(slendr::check_dependencies(python = TRUE, slim = TRUE))

library(slendr)
init_env(quiet = TRUE)

# model function including a SLiM selection extension
model <- function(s, onset_time, origin_pop, target_pop) {
  extension_code <- r"(
  // Define model constants (to be substituted) all in one place
  // (each {{placeholder}} will be replaced by a value passed from R).
  // Note that string constant template patterns are surrounded by "quotes"!
  initialize() {
      defineConstant("s", {{s}});
      defineConstant("onset_time", {{onset_time}});
      defineConstant("target_pop", "{{target_pop}}");
      defineConstant("origin_pop", "{{origin_pop}}");

      // paths to trajectory files
      defineConstant("origin_file", PATH + "/" + "origin_traj.tsv");
      defineConstant("target_file", PATH + "/" + "target_traj.tsv");
  }
  // Because we want to simulate non-neutral evolution, we have to provide a
  // custom initialization callback -- slendr will use it to replace its default
  // neutral genomic architecture (i.e. the initialize() {...} callback it uses
  // by default for neutral simulations). Note that we can refer to slendr's
  // constants SEQUENCE_LENGTH and RECOMBINATION_RATE, which will carry values
  // passed through from R via slendr's slim() R function.
  initialize() {
      initializeMutationType("m1", 0.5, "f", s);

      initializeGenomicElementType("g1", m1, 1.0);
      initializeGenomicElement(g1, 0, SEQUENCE_LENGTH - 1);

      initializeMutationRate(0);
      initializeRecombinationRate(RECOMBINATION_RATE);
  }

  function (void) add_mutation(void) {
      // sample one target carrier of the new mutation...
      target = sample(population(origin_pop).genomes, 1);
      // ... and add the mutation in the middle of it
      mut = target.addNewDrawnMutation(m1, position = asInteger(SEQUENCE_LENGTH / 2));

      // save the mutation for later reference
      defineGlobal("MUTATION", mut);

      write_log("adding beneficial mutation to population " + target_pop);

      writeFile(origin_file, "time\tfrequency");
      writeFile(target_file, "time\tfrequency");
  }

  tick(onset_time) late() {
      // save simulation state in case we need to restart if the mutation is lost
      save_state();

      add_mutation();
  }

  tick(onset_time):SIMULATION_END late() {
      // the mutation is not segregating and is not fixed either -- we must restart
      if (!MUTATION.isSegregating & !MUTATION.isFixed) {
          write_log("mutation lost -- restarting");

          reset_state();

          add_mutation();
      }

      // compute the frequency of the mutation of interest and save it (if the
      // mutation is missing at this time, save its frequency as NA)
      freq_origin = "NA";
      freq_target = "NA";
      if (population(origin_pop, check = T))
        freq_origin = population(origin_pop).genomes.mutationFrequenciesInGenomes();
      if (population(target_pop, check = T))
        freq_target = population(target_pop).genomes.mutationFrequenciesInGenomes();

      writeFile(origin_file, model_time(community.tick) + "\t" + freq_origin, append = T);
      writeFile(target_file, model_time(community.tick) + "\t" + freq_target, append = T);
  }
  )"

  extension <- substitute_values(
    extension_code,
    s = s, onset_time = as.integer(onset_time),
    origin_pop = origin_pop, target_pop = target_pop
  )

  afr <- population("AFR", time = 90000, N = 3000)
  ooa <- population("OOA", parent = afr, time = 60000, N = 500, remove = 23000)
  ehg <- population("EHG", parent = ooa, time = 28000, N = 1000, remove = 6000)
  eur <- population("EUR", parent = ehg, time = 25000, N = 5000)
  ana <- population("ANA", time = 28000, N = 3000, parent = ooa, remove = 4000)
  yam <- population("YAM", time = 8000, N = 500, parent = ehg, remove = 2500)

  gf <- list(
    gene_flow(from = ana, to = yam, rate = 0.4, start = 7900, end = 7800),
    gene_flow(from = ana, to = eur, rate = 0.5, start = 6000, end = 5000),
    gene_flow(from = yam, to = eur, rate = 0.65, start = 4000, end = 3500)
  )

  model <- compile_model(
    populations = list(afr, ooa, ehg, eur, ana, yam),
    gene_flow = gf, generation_time = 30,
    extension = extension
  )

  samples <- schedule_sampling(model, time = 0, list(eur, 50))

  return(list(model, samples))
}

priors <- list(
  s ~ runif(0, 0.1),
  onset_time ~ runif(3000, 20000)
)

# even a SLiM/selection model can be run in msprime (its demographic part, of course)
test_that("format = 'files' produces only a .trees file (msprime)", {
  # only path is saved when just a path is provided as data
  data <- simulate_model(
    model, priors,
    sequence_length = 1e6, recombination_rate = 0,
    model_args = list(origin_pop = "EUR", target_pop = "EUR"), engine_args = list(random_seed = 42),
    engine = "msprime",
    format = "files",
    data = list(path = path),
  )
  expect_true(dir(data$path) == "msprime.trees")

  # tree sequence is loaded when a reading function is provided
  data <- simulate_model(
    model, priors,
    sequence_length = 1e6, recombination_rate = 0,
    model_args = list(origin_pop = "EUR", target_pop = "EUR"), engine_args = list(random_seed = 42),
    engine = "msprime",
    format = "files",
    data = list(
      ts_data = function(path, model) ts_read(file.path(path, "msprime.trees"), model) %>% ts_mutate(1e-8)
    )
  )
  expect_true(names(data) == "ts_data")
  expect_s3_class(data$ts_data, "slendr_ts")
})


# even a SLiM/selection model can be run in msprime (its demographic part, of course)
test_that("format = 'files' produces only a .trees file (msprime)", {
  # only path is saved when just a path is provided as data
  data <- simulate_model(
    model, priors,
    sequence_length = 1e6, recombination_rate = 0,
    model_args = list(origin_pop = "EUR", target_pop = "EUR"),
    engine = "msprime",
    format = "files",
    data = list(path = path)
  )
  expect_true(dir(data$path) == "msprime.trees")

  # tree sequence is loaded when a reading function is provided
  data <- simulate_model(
    model, priors,
    sequence_length = 1e6, recombination_rate = 0,
    model_args = list(origin_pop = "EUR", target_pop = "EUR"),
    engine = "msprime",
    format = "files",
    data = list(
      ts_data = function(path, model) ts_read(file.path(path, "msprime.trees"), model) %>% ts_mutate(1e-8)
    )
  )
  expect_true(names(data) == "ts_data")
  expect_s3_class(data$ts_data, "slendr_ts")
})


skip_if(!slendr::check_dependencies(python = TRUE, slim = TRUE))
skip_on_cran()

library(slendr)
init_env(quiet = TRUE)

test_that("distinguishing between competing models works as intended", {
  # read a list of three different ABC models
  models <- lapply(c("X", "Y", "Z"), function(i) { readRDS(url(
    paste0("https://raw.githubusercontent.com/bodkan/demografr/refs/heads/main/inst/examples/downstream_abc", i, ".rds")
  )) })

  # run cross validation to find out if we have even the power to distinguish
  # our competing models(see the abc package vignette for interpretation)
  suppressWarnings(quiet(cv_models <- cross_validate(models, nval = 2, tols = 0.05, method = "neuralnet")))

  expect_s3_class(cv_models, "demografr_cv_modsel")
})

test_that("estimating model parameters can be done as expected", {
  # read an example result of an ABC inference
  abc_res <- readRDS(system.file("examples/basics_abc.rds", package = "demografr"))

  # perform cross-validation
  suppressWarnings(quiet(cv_params <- cross_validate(abc_res, nval = 2, tols = 0.05, method = "neuralnet")))

  expect_s3_class(cv_params, "demografr_cv_abc")
})

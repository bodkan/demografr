test_that("Simplest case compiles", {

  library(slendr)

  model <- function(Ne_A, Ne_B, Ne_C, Ne_D, T_AB, T_BC, T_CD) {
    popA <- population("popA", 1, Ne_A)
    popB <- population("popB", T_AB, Ne_B, popA)
    popC <- population("popC", T_BC,Ne_C, popB)
    popD <- population("popD", T_CD, Ne_D, popC)

    model <- compile_model(
      populations = list(popA, popB, popC, popD),
      generation_time = 1, simulation_length = 10000
    )

    return (model)
  }

  testthat::expect_no_error(plot_specification(model, ntries = 100))

})

test_that("Simplest case but without calling slendr ", {

  model <- function(Ne_A, Ne_B, Ne_C, Ne_D, T_AB, T_BC, T_CD) {
    popA <- slendr::population("popA", 1, Ne_A)
    popB <- slendr::population("popB", T_AB, Ne_B, popA)
    popC <- slendr::population("popC", T_BC,Ne_C, popB)
    popD <- slendr::population("popD", T_CD, Ne_D, popC)

    model <- slendr::compile_model(
      populations = list(popA, popB, popC, popD),
      generation_time = 1, simulation_length = 10000
    )

    return (model)
  }

  testthat::expect_no_error(plot_specification(model, ntries = 100))

})


test_that("More complex scenario with gene flow and bounded parameter ", {

  model <- function(Ne_A, Ne_B, T_AB, rate) {
    popA <- slendr::population("popA", 1, Ne_A)
    popB <- slendr::population("popB", T_AB, Ne_B, popA)

    gf <- list(
      slendr::gene_flow(
        from = popA, to = popB, rate = rate, start = 1000,
        end = 2000, overlap = FALSE)
    )

    model <- slendr::compile_model(
      populations = list(popA, popB),
      gene_flow = gf,
      generation_time = 1, simulation_length = 10000
    )

    return (model)
  }

  prior_list <- list(
    T_AB ~ runif(2, 1000-1),
    rate ~ runif(0, 1)
  )

  plot_specification(model, 1000,priors = prior_list) |>
    testthat::expect_no_error()

})


test_that("Nonexistent function cannot be used for sampling", {
  expect_error(sample_prior(pop ~ qwerty(1, 10)), "An unknown function")
  expect_true(is.list(sample_prior(pop ~ runif(1, 10))))
})

test_that("Incorrect prior formula fails gracefully (warnings not allowed)", {
  expect_error(sample_prior(pop ~ runif(10, 1)), "A warning was raised")
  expect_true(is.list(sample_prior(pop ~ runif(1, 10))))
})

test_that("Incorrect prior formula fails gracefully (errors not allowed)", {
  expect_error(sample_prior(pop ~ plot(10, 1)), "A warning was raised")
  expect_true(is.list(sample_prior(pop ~ runif(1, 10))))
})

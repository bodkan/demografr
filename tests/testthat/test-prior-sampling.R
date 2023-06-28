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

#
# scalar priors generated correctly
#

test_that("A scalar 'fixed prior' gives a single number", {
  sample_result <- sample_prior(paramX ~ 42)

  expect_true(all(names(sample_result) == c("variable", "value")))
  expect_true(all.equal(sample_result$value, 42))
  expect_true(sample_result$variable == "paramX")
})

test_that("A prior samples a single number", {
  set.seed(123)
  sample_result <- sample_prior(paramY ~ runif(1, 10))
  expect_true(all(names(sample_result) == c("variable", "value")))
  expect_true(all.equal(sample_result$value, 3.588198, tol = 1e-7))
  expect_true(sample_result$variable == "paramY")
})

#
# vectorized priors generated correctly
#

test_that("A vector 'fixed prior' gives a vector", {
  expect_error(
    sample_prior(paramX ~ c(42, 123, -1)),
    "Make sure to specify the correct N when defining '.*\\[N\\]'."
  )

  sample_result <- sample_prior(paramX[3] ~ c(42, 123, -1))
  expect_true(all(names(sample_result) == c("variable", "value")))
  expect_true(all(sample_result$value == c(42, 123, -1)))
  expect_true(sample_result$variable == "paramX")
})

test_that("A vector prior gives a vector", {
  set.seed(123)
  sample_result <- sample_prior(paramX[3] ~ runif(1, 10))
  expect_true(all(names(sample_result) == c("variable", "value")))
  expect_true(all.equal(sample_result$value, c(3.588198, 8.094746, 4.680792), tol = 1e-7))
  expect_true(sample_result$variable == "paramX")
})

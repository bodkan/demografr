error_msg <- "A bounds expression must take the form of an R formula such as"

test_that("Nonexistent function cannot be used for bounds specification", {
  expect_error(parse_bounds(pop ~ qwerty(1, 10)), error_msg)
  expect_true(is.list(parse_bounds(pop ~ between(1, 10))))
})

test_that("'Lower' bound must be lower than the 'higher' bound", {
  expect_error(parse_bounds(pop ~ qwerty(10, 1)), error_msg)
  expect_true(is.list(parse_bounds(pop ~ between(1, 10))))
})

test_that("Incorrect bounds formula fails gracefully (errors not allowed)", {
  expect_error(parse_bounds(pop ~ warning()), error_msg)
  expect_error(parse_bounds(pop ~ 42), error_msg)
})

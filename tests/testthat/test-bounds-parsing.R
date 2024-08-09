error_msg <- "A bounds expression must take the form of an R formula such as"

# dummy model which has the parameter specified in bound expressions below
model <- function(pop) { NULL }

test_that("Nonexistent function cannot be used for bounds specification", {
  expect_error(parse_bounds(pop ~ qwerty(1, 10)), "Bounds expressions must be wrapped in a list")
  expect_error(parse_bounds(list(pop ~ qwerty(1, 10))), error_msg)
  expect_true(is.list(
    parse_bounds(list(pop ~ between(1, 10)), model = model, model_args = NULL)
  ))
})

test_that("'Lower' bound cannot be higher than the 'higher' bound", {
  expect_error(parse_bounds(list(pop ~ qwerty(10, 1)), model, NULL), error_msg)
  expect_true(is.list(parse_bounds(list(pop ~ between(1, 10)), model, NULL)))
})

test_that("Incorrect bounds formula fails gracefully (errors not allowed)", {
  expect_error(parse_bounds(list(pop ~ warning())), error_msg)
  expect_error(parse_bounds(list(pop ~ 42)), error_msg)
})

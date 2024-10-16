data_files <- vapply(c("X", "Y", "Z"),
                     function(model) system.file(paste0("examples/downstream_data", model, ".rds"), package = "demografr"),
                     FUN.VALUE = character(1))
abc_files <- vapply(c("X", "Y", "Z"),
                     function(model) system.file(paste0("examples/downstream_abc", model, ".rds"), package = "demografr"),
                     FUN.VALUE = character(1))

data <- lapply(data_files, readRDS)
abc <- lapply(abc_files, readRDS)

test_that("unpacking data generates the correct merged summary statistics (single model)", {
  unpacked <- unpack(data[["X"]])

  expect_true(all(unpacked$sumstat[unpacked$index == "modelX", ] == dataX$simulated))
  expect_true(all(unpacked$index == "modelX"))
})

test_that("unpacking data generates the correct merged summary statistics (multiple models)", {
  unpacked <- unpack(data)

  expect_true(all(unpacked$sumstat[unpacked$index == "modelX", ] == data[["X"]]$simulated))
  expect_true(all(unpacked$sumstat[unpacked$index == "modelY", ] == data[["Y"]]$simulated))
  expect_true(all(unpacked$sumstat[unpacked$index == "modelZ", ] == data[["Z"]]$simulated))

  expect_true(all(unpacked$index[unpacked$index == "modelX"] == "modelX"))
  expect_true(all(unpacked$index[unpacked$index == "modelY"] == "modelY"))
  expect_true(all(unpacked$index[unpacked$index == "modelZ"] == "modelZ"))
})

test_that("unpacking abc generates the correct merged summary statistics (single model)", {
  unpacked <- unpack(abc[["X"]])

  expect_true(all(unpacked$sumstat[unpacked$index == "modelX", ] == attr(abc[["X"]], "components")$simulated))
  expect_true(all(unpacked$index == "modelX"))
})

test_that("unpacking data generates the correct merged summary statistics (multiple models)", {
  unpacked <- unpack(abc)

  expect_true(all(unpacked$sumstat[unpacked$index == "modelX", ] == abc[["X"]]$simulated))
  expect_true(all(unpacked$sumstat[unpacked$index == "modelY", ] == abc[["Y"]]$simulated))
  expect_true(all(unpacked$sumstat[unpacked$index == "modelZ", ] == abc[["Z"]]$simulated))

  expect_true(all(unpacked$index[unpacked$index == "modelX"] == "modelX"))
  expect_true(all(unpacked$index[unpacked$index == "modelY"] == "modelY"))
  expect_true(all(unpacked$index[unpacked$index == "modelZ"] == "modelZ"))
})

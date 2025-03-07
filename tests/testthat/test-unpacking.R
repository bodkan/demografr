data_files <- here::here(paste0("inst/examples/downstream_data", c("X", "Y", "Z"), ".rds"))
abc_files <- here::here(paste0("inst/examples/downstream_data", c("X", "Y", "Z"), ".rds"))

skip_if_not(Sys.getenv("RUNNER_OS") == "" && all(file.exists(c(data_files, abc_files))))

data <- lapply(data_files, readRDS)
abc <- lapply(abc_files, readRDS)

test_that("unpacking data generates the correct merged summary statistics (single model)", {
  unpacked <- unpack(data[["X"]])

  expect_true(all(unpacked$sumstat[unpacked$index == "modelX", ] == data[["X"]]$simulated))
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

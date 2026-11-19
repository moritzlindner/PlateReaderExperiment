test_that("plate-matrix Tecan exports normalize to 4-D", {
  f <- system.file("extdata", "tecan_plate_matrix_1.xlsx", package = "PlateReaderExperiment")
  x <- read.tecan(f, tz = "Europe/Berlin")
  expect_s4_class(x, "PlateReaderExperiment")
  expect_equal(dim(x), c(36, 8, 1, 1))
  expect_equal(positionData(x)$position_id, "P001")
  expect_equal(assay(x)["A1", "CFP", "T001", "P001"], 3170)
  expect_equal(metadata(x)$source_format, "plate_matrix")
})

test_that("a folder of plate matrices becomes the time dimension", {
  td <- tempfile(); dir.create(td)
  f1 <- system.file("extdata", "tecan_plate_matrix_1.xlsx", package = "PlateReaderExperiment")
  f2 <- system.file("extdata", "tecan_plate_matrix_2.xlsx", package = "PlateReaderExperiment")
  file.copy(c(f1, f2), td)
  x <- read.tecan(td, tz = "Europe/Berlin")
  expect_equal(dim(x), c(36, 8, 2, 1))
  expect_equal(nrow(timeData(x)), 2)
  expect_equal(nrow(acquisitionData(x)), 36 * 8 * 2)
})

test_that("kinetic multi-read Tecan exports retain positions and incomplete cycles", {
  f <- system.file("extdata", "tecan_kinetic_multiread.xlsx", package = "PlateReaderExperiment")
  x <- read.tecan(f, tz = "Europe/Berlin")
  expect_equal(dim(x), c(24, 2, 3, 12))
  expect_equal(channelData(x)$channel_id, c("eGFP-Peak-Peak", "eGFP-Peak-Trough"))
  expect_equal(timeData(x)$cycle, 1:3)
  expect_equal(nrow(positionData(x)), 12)
  expect_equal(assay(x)["A1", "eGFP-Peak-Peak", "T001", "1;2"], 20708)
  expect_true(is.na(assay(x)["A1", "eGFP-Peak-Peak", "T003", "2;3"]))
  expect_true(is.na(assay(x)["A2", "eGFP-Peak-Peak", "T003", "1;2"]))
  expect_equal(metadata(x)$kinetic$requested_cycles, 4)
  expect_equal(metadata(x)$kinetic$unobserved_requested_cycles, 4)
})

test_that("single-time multi-read Tecan exports retain within-well positions", {
  f <- system.file("extdata", "tecan_single_time_multiread.xlsx", package = "PlateReaderExperiment")
  x <- read.tecan(f, tz = "Europe/Berlin")
  expect_s4_class(x, "PlateReaderExperiment")
  expect_equal(dim(x), c(24, 4, 1, 12))
  expect_equal(channelData(x)$channel_id,
               c("eGFP-Peak-Peak", "miRFP", "mCherry", "LSS-Mkate"))
  expect_equal(timeData(x)$time_id, "T001")
  expect_equal(nrow(positionData(x)), 12)
  expect_equal(assay(x)["A1", "eGFP-Peak-Peak", "T001", "1;2"], 13939)
  expect_equal(assay(x)["D6", "LSS-Mkate", "T001", "4;3"], 13748)
  expect_equal(metadata(x)$source_format, "single_time_multi_read")
  expect_equal(nrow(acquisitionData(x)), 24 * 4)
  expect_true(all(acquisitionData(x)$n_positions_observed == 12))
})

test_that("a folder of single-time multi-read exports becomes the time dimension", {
  td <- tempfile(); dir.create(td)
  f <- system.file("extdata", "tecan_single_time_multiread.xlsx", package = "PlateReaderExperiment")
  file.copy(f, file.path(td, "read_1.xlsx"))
  file.copy(f, file.path(td, "read_2.xlsx"))
  x <- read.tecan(td, tz = "Europe/Berlin", order_by = "file")
  expect_equal(dim(x), c(24, 4, 2, 12))
  expect_equal(nrow(timeData(x)), 2)
  expect_equal(nrow(acquisitionData(x)), 24 * 4 * 2)
})


test_that("SparkControl OVER and INVALID cells become NA with warnings", {
  expect_warning(
    over <- PlateReaderExperiment:::.tecan_cell_number(
      "OVER", "example.xlsx", 67L, "measurement at 2;4", TRUE
    ),
    "converted to NA"
  )
  expect_true(is.na(over))

  expect_warning(
    invalid <- PlateReaderExperiment:::.tecan_cell_number(
      "INVALID", "example.xlsx", 68L, "measurement at 2;5", TRUE
    ),
    "converted to NA"
  )
  expect_true(is.na(invalid))
})

make_merge_experiment <- function(offset = 0, wells = c("A1", "A2")) {
  a <- array(seq_len(length(wells) * 2 * 3 * 2) + offset,
             dim = c(length(wells), 2, 3, 2),
             dimnames = list(sample = wells, channel = c("GFP", "RFP"),
                             time = c("T001", "T002", "T003"), position = c("1;1", "1;2")))
  acq <- expand.grid(sample_id = wells, channel_id = c("GFP", "RFP"),
                     time_id = c("T001", "T002", "T003"), stringsAsFactors = FALSE)
  acq$timestamp <- as.POSIXct("2026-01-01", tz = "UTC") + offset
  acq$valid <- TRUE
  PlateReaderExperiment(
    a,
    sampleData = data.frame(sample_id = wells, well = wells, plate_id = "plate1"),
    channelData = data.frame(channel_id = c("GFP", "RFP"), excitation = c(488, 561), gain = 100),
    timeData = data.frame(time_id = c("T001", "T002", "T003"), time_index = 1:3,
                          cycle = 1:3, nominal_time_s = c(0, 60, 120), observed_time_s = c(0, 61, 122)),
    positionData = data.frame(position_id = c("1;1", "1;2"), position_index = 1:2,
                              grid_row = 1L, grid_column = 1:2, position_type = "within_well_grid"),
    design = data.frame(sample_id = wells, group = rep(c("control", "treated"), length.out = length(wells)),
                        biological_sample = paste0("mouse", seq_along(wells))),
    acquisitionData = acq, metadata = list(run = offset, processing_history = list())
  )
}

test_that("merging appends wells and preserves annotations, values and provenance", {
  x <- make_merge_experiment()
  y <- make_merge_experiment(100, wells = c("B3", "C4", "D5"))
  y@design$group <- c("treated", "control", "treated")
  y@data[2, 1, 3, 2] <- NA_real_
  z <- merge_experiments(day1 = x, day2 = y)
  expect_s4_class(z, "PlateReaderExperiment")
  expect_true(validObject(z))
  expect_equal(dim(z), c(5, 2, 3, 2))
  expect_equal(unname(assay(z)[1:2, , , , drop = FALSE]), unname(assay(x)))
  expect_equal(unname(assay(z)[3:5, , , , drop = FALSE]), unname(assay(y)))
  expect_identical(assay(x), assay(make_merge_experiment()))
  expect_equal(sampleData(z)$sample_id, c("day1::A1", "day1::A2", "day2::B3", "day2::C4", "day2::D5"))
  expect_equal(sampleData(z)$well, c("A1", "A2", "B3", "C4", "D5"))
  expect_equal(sampleData(z)$plate_id, rep(c("day1::plate1", "day2::plate1"), c(2, 3)))
  expect_equal(sampleData(z)$source_plate_id, rep("plate1", 5))
  expect_equal(design(z)$group, c(design(x)$group, design(y)$group))
  expect_equal(design(z)$biological_sample, c(design(x)$biological_sample, design(y)$biological_sample))
  expect_equal(design(z)$experiment_id, rep(c("day1", "day2"), c(2, 3)))
  expect_equal(metadata(z)$experiments$day2$metadata, metadata(y))
  expect_equal(metadata(z)$processing_history[[1]]$operation, "merge_experiments")
  expect_equal(acquisitionData(z)$timestamp, c(acquisitionData(x)$timestamp, acquisitionData(y)$timestamp))
  expect_equal(length(unique(acquisitionData(z)$sample_id)), 5)
})

test_that("overlapping wells stay separate and all input forms work", {
  x <- make_merge_experiment()
  z <- merge_experiments(list(first = x, second = x))
  expect_equal(sampleData(z)$well, rep(c("A1", "A2"), 2))
  expect_equal(sampleData(z)$source_sample_id, rep(c("A1", "A2"), 2))
  expect_equal(anyDuplicated(sampleData(z)$sample_id), 0L)
  expect_equal(anyDuplicated(acquisitionData(z)[, c("sample_id", "channel_id", "time_id")]), 0L)
  explicit <- merge_experiments(x, x, experiment_ids = c("first", "second"))
  expect_equal(assay(explicit), assay(z))
  expect_equal(sampleData(explicit), sampleData(z))
  expect_equal(merge_experiments(x, x)@sampleData$experiment_id,
               rep(c("experiment1", "experiment2"), each = 2))
  expect_equal(merge_experiments(x, second = x)@sampleData$experiment_id,
               rep(c("experiment1", "second"), each = 2))
})

test_that("channels, times and positions are aligned by identifiers", {
  x <- make_merge_experiment()
  reordered <- x[, c("RFP", "GFP"), c("T003", "T001", "T002"), c("1;2", "1;1")]
  # Column order is not part of the structure either.
  reordered@channelData <- reordered@channelData[, c("gain", "channel_id", "excitation")]
  z <- merge_experiments(x, reordered)
  expect_equal(unname(assay(z)[3:4, , , , drop = FALSE]), unname(assay(x)))
  expect_equal(channelData(z), channelData(x))
  expect_equal(timeData(z), timeData(x))
  expect_equal(positionData(z), positionData(x))
})

test_that("equivalent read positions can have different identifiers", {
  x <- make_merge_experiment()
  y <- x
  y@positionData$position_id <- c("position_A", "position_B")
  dimnames(y@data)[[4]] <- y@positionData$position_id
  z <- merge_experiments(x, y)
  expect_equal(unname(assay(z)[3:4, , , , drop = FALSE]), unname(assay(x)))
  expect_equal(metadata(z)$experiments$experiment2$position_id_map$source_position_id,
               c("position_A", "position_B"))
  expect_true(validObject(z))
})

test_that("different read structures and settings fail with informative errors", {
  x <- make_merge_experiment()
  expect_error(merge_experiments(x, x[, "GFP", 1:3, 1:2]), "channel identifiers/count")
  expect_error(merge_experiments(x, x[, 1:2, "T001", 1:2]), "time identifiers/count")
  expect_error(merge_experiments(x, x[, 1:2, 1:3, "1;1"]), "position identifiers/count")
  changed <- x
  changed@channelData$gain[1] <- 101
  expect_error(merge_experiments(x, changed), "experiment2.*channel.*gain")
  changed <- x
  changed@channelData$excitation[1] <- 405
  expect_error(merge_experiments(x, changed), "channel.*excitation")
  changed <- x
  changed@channelData$gain <- NULL
  expect_error(merge_experiments(x, changed), "channel metadata columns")
  changed <- x
  changed@positionData$grid_column[1] <- 3
  expect_error(merge_experiments(x, changed), "position.*grid_column")
  changed <- x
  changed@timeData$nominal_time_s[2] <- 600
  expect_error(merge_experiments(x, changed), "logical time.*nominal_time_s")
  changed <- x
  changed@timeData$cycle[1] <- 4L
  expect_error(merge_experiments(x, changed), "logical time.*cycle")
})

test_that("same channel/time counts with different identifiers are rejected", {
  x <- make_merge_experiment()
  for (axis in 2:3) {
    changed <- x
    dn <- dimnames(changed@data)
    dn[[axis]][1] <- "different"
    dimnames(changed@data) <- dn
    slot_name <- c("sampleData", "channelData", "timeData", "positionData")[[axis]]
    id <- c("sample_id", "channel_id", "time_id", "position_id")[[axis]]
    slot(changed, slot_name)[[id]][1] <- "different"
    changed@acquisitionData <- changed@acquisitionData[FALSE, ]
    expect_error(merge_experiments(x, changed), "identifiers/count")
  }
})

test_that("run-specific time metadata are retained without first-run leakage", {
  x <- make_merge_experiment()
  y <- make_merge_experiment(100)
  x@timeData$source_file <- rep("one.xlsx", 3)
  y@timeData$source_file <- rep("two.xlsx", 3)
  y@timeData$observed_time_s <- c(0, 62, 124)
  z <- merge_experiments(one = x, two = y)
  expect_false("observed_time_s" %in% names(timeData(z)))
  expect_false("source_file" %in% names(timeData(z)))
  df <- as.data.frame(z)
  expect_equal(unique(df$observed_time_s[df$experiment_id == "one" & df$time_id == "T002"]), 61)
  expect_equal(unique(df$observed_time_s[df$experiment_id == "two" & df$time_id == "T002"]), 62)
  expect_equal(unique(df$source_file[df$experiment_id == "one"]), "one.xlsx")
  expect_equal(unique(df$source_file[df$experiment_id == "two"]), "two.xlsx")
  expect_equal(metadata(z)$experiments$two$timeData, timeData(y))
})

test_that("extra descriptor columns and empty or sparse acquisition tables are supported", {
  x <- make_merge_experiment()
  y <- make_merge_experiment(100)
  x@acquisitionData <- x@acquisitionData[FALSE, ]
  y@acquisitionData <- y@acquisitionData[1, ]
  y@acquisitionData$source_file <- "actual.xlsx"
  y@timeData$source_file <- "logical.xlsx"
  y@sampleData$extra <- as.Date("2026-01-01")
  x@design$group <- factor(x@design$group)
  y@design$group <- factor(c("placebo", "drug"))
  y@design$dose <- c(1, 2)
  z <- merge_experiments(x, y)
  expect_s3_class(sampleData(z)$extra, "Date")
  expect_true(all(is.na(sampleData(z)$extra[1:2])))
  expect_equal(design(z)$group, c("control", "treated", "placebo", "drug"))
  expect_equal(design(z)$dose, c(NA, NA, 1, 2))
  expect_true(validObject(z))
  expect_equal(nrow(acquisitionData(z)), 24)
  expect_equal(acquisitionData(z)$source_file[13], "actual.xlsx")
  expect_equal(acquisitionData(z)$source_file[14:24], rep("logical.xlsx", 11))
})

test_that("empty acquisition metadata remain empty when time descriptors agree", {
  x <- make_merge_experiment()
  x@acquisitionData <- x@acquisitionData[FALSE, ]
  z <- merge_experiments(x, x)
  expect_equal(nrow(acquisitionData(z)), 0)
  expect_true(validObject(z))
})

test_that("provenance column collisions and subsequent merges preserve earlier labels", {
  x <- make_merge_experiment()
  z <- merge_experiments(first = x, second = x)
  again <- merge_experiments(previous = z, third = x)
  expect_true(validObject(again))
  expect_equal(design(again)$experiment_id.input[1:4], rep(c("first", "second"), each = 2))
  expect_equal(design(again)$experiment_id, c(rep("previous", 4), rep("third", 2)))
  expect_equal(sampleData(again)$source_sample_id[1:4], sampleData(z)$sample_id)
  expect_equal(sampleData(again)$source_sample_id.input[1:4], rep(c("A1", "A2"), 2))
  expect_equal(metadata(again)$experiments$previous$metadata, metadata(z))
})

test_that("generated identifiers remain unique even with namespace separators", {
  x <- make_merge_experiment(wells = c("b::A1", "b::A2"))
  y <- make_merge_experiment()
  z <- merge_experiments(x, y, experiment_ids = c("a", "a::b"))
  expect_equal(anyDuplicated(sampleData(z)$sample_id), 0L)
  expect_true(validObject(z))
})

test_that("invalid merge inputs and experiment labels are rejected", {
  x <- make_merge_experiment()
  expect_error(merge_experiments(), "at least two")
  expect_error(merge_experiments(x), "at least two")
  expect_error(merge_experiments(x, NULL), "Input 2")
  expect_error(merge_experiments(list(x, "oops")), "Input 2")
  for (labels in list("a", c("a", "a"), c("a", NA), c("a", " "), 1:2)) {
    expect_error(merge_experiments(x, x, experiment_ids = labels), "experiment_ids")
  }
  invalid <- x
  invalid@design$sample_id[1] <- "unknown"
  expect_error(merge_experiments(x, invalid), "invalid class")
})

test_that("merged objects support subsetting, grouping and long-format conversion", {
  x <- make_merge_experiment()
  z <- merge_experiments(first = x, second = x)
  expect_equal(nrow(as.data.frame(z)), prod(dim(z)))
  second <- subset(z, experiment_id = "second")
  expect_equal(unname(assay(second)), unname(assay(x)))
  expect_true(validObject(average_positions(z)))
  expect_equal(dim(average_by_group(z, by = c("experiment_id", "group"))), c(4, 2, 3, 2))
})

test_that("real Tecan imports can be merged after separate plate annotation", {
  f <- system.file("extdata", "tecan_kinetic_multiread.xlsx", package = "PlateReaderExperiment")
  x <- suppressWarnings(read.tecan(f, tz = "Europe/Berlin"))
  y <- x
  x <- set_design(x, data.frame(well = sampleData(x)$well, group = "control"), by = "well")
  y <- set_design(y, data.frame(well = sampleData(y)$well, group = "treated"), by = "well")
  z <- merge_experiments(control_plate = x, treated_plate = y)
  expect_equal(dim(z), c(48, 2, 3, 12))
  expect_equal(design(z)$group, rep(c("control", "treated"), each = 24))
  expect_true(validObject(z))
  expect_true(validObject(average_by_group(average_positions(z), by = "group")))
})

test_that("identical layouts with distinct acquisition times merge on time", {
  x <- make_merge_experiment()
  y <- make_merge_experiment(3600)
  x@acquisitionData$timestamp <- x@acquisitionData$timestamp +
    (match(x@acquisitionData$time_id, timeData(x)$time_id) - 1) * 60
  y@acquisitionData$timestamp <- y@acquisitionData$timestamp +
    (match(y@acquisitionData$time_id, timeData(y)$time_id) - 1) * 60
  z <- merge_experiments(early = x, late = y)
  expect_true(validObject(z))
  expect_equal(dim(z), c(2, 2, 6, 2))
  expect_equal(timeData(z)$observed_time_s, c(0, 60, 120, 3600, 3660, 3720))
  expect_equal(timeData(z)$source_time_id, rep(c("T001", "T002", "T003"), 2))
  expect_equal(timeData(z)$experiment_id, rep(c("early", "late"), each = 3))
  expect_equal(timeData(z)$nominal_time_s, rep(c(0, 60, 120), 2))
  expect_equal(unname(assay(z)[, , 1:3, , drop = FALSE]), unname(assay(x)))
  expect_equal(unname(assay(z)[, , 4:6, , drop = FALSE]), unname(assay(y)))
  expect_equal(nrow(acquisitionData(z)), 24)
  expect_equal(length(unique(acquisitionData(z)$time_id)), 6)
  expect_equal(metadata(z)$merge_axis, "time")
})

test_that("same plate labels with different designs remain separate plates", {
  x <- make_merge_experiment()
  y <- make_merge_experiment(3600)
  x@acquisitionData$timestamp <- x@acquisitionData$timestamp +
    (match(x@acquisitionData$time_id, timeData(x)$time_id) - 1) * 60
  y@acquisitionData$timestamp <- y@acquisitionData$timestamp +
    (match(y@acquisitionData$time_id, timeData(y)$time_id) - 1) * 60
  y@design$group <- rev(y@design$group)
  expect_equal(dim(merge_experiments(x, y)), c(4, 2, 3, 2))
  expect_equal(dim(merge_experiments(x, x, axis = "sample")), c(4, 2, 3, 2))
  expect_error(merge_experiments(x, y, axis = "time"), "identical plate layout/design")
})

test_that("real-time alignment preserves nominal schedule and recorded timestamps", {
  x <- make_merge_experiment()
  x@acquisitionData$timestamp <- x@acquisitionData$timestamp +
    (match(x@acquisitionData$time_id, timeData(x)$time_id) - 1) * 60
  x@acquisitionData$actual_time_s <- 0
  origin <- as.POSIXct("2025-12-31 23:00:00", tz = "UTC")
  y <- offset_time_to_real(x, origin = origin)
  expect_equal(timeData(y)$observed_time_s, c(3600, 3660, 3720))
  expect_equal(timeData(y)$nominal_time_s, c(0, 60, 120))
  expect_s3_class(timeData(y)$absolute_time, "POSIXct")
  expect_equal(acquisitionData(y)$actual_time_s,
               as.numeric(acquisitionData(x)$timestamp) - as.numeric(origin))
  expect_equal(timeData(x)$observed_time_s, c(0, 61, 122))
  x@acquisitionData$timestamp <- NULL
  expect_error(offset_time_to_real(x), "timestamp")
})

test_that("explicit absolute_time wins over unchanged acquisition timestamps", {
  x <- make_merge_experiment()
  x@acquisitionData$timestamp <- x@acquisitionData$timestamp +
    (match(x@acquisitionData$time_id, timeData(x)$time_id) - 1) * 60
  x <- offset_time_to_real(x)
  y <- x
  x@metadata$time_origin <- x@metadata$time_origin - 1
  x@timeData$absolute_time <- x@timeData$absolute_time - 1
  z <- merge_experiments(x, y, axis = "time")
  expect_true(validObject(z))
  expect_equal(dim(z), c(2, 2, 6, 2))
  expect_equal(timeData(z)$observed_time_s, c(0, 60, 120, 1, 61, 121))
  expect_equal(as.numeric(timeData(z)$absolute_time),
               c(as.numeric(timeData(x)$absolute_time), as.numeric(timeData(y)$absolute_time)))
})

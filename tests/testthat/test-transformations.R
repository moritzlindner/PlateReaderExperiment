make_kinetic <- function() {
  f <- system.file("extdata", "tecan_kinetic_multiread.xlsx", package = "PlateReaderExperiment")
  read.tecan(f, tz = "Europe/Berlin")
}

test_that("position averaging keeps a length-one fourth dimension", {
  x <- make_kinetic()
  y <- average_positions(x)
  expect_equal(dim(y), c(24, 2, 3, 1))
  expect_equal(positionData(y)$position_id, "mean")
  expect_equal(assay(y)["A1", "eGFP-Peak-Peak", "T001", "mean"], 20549.25)
  expect_equal(assay(y)["A1", "eGFP-Peak-Peak", "T003", "mean"], 20787)
})

test_that("design, grouping, control subtraction and subsetting work", {
  x <- make_kinetic()
  d <- data.frame(
    well = sampleData(x)$well,
    group = rep(c("control", "treated"), each = 12),
    biological_sample = paste0("sample_", seq_len(24)),
    stringsAsFactors = FALSE
  )
  x <- set_design(x, d, by = "well")
  expect_equal(design(x)$group[1], "control")

  xs <- subset(x, group = "treated", channels = "eGFP-Peak-Peak")
  expect_equal(dim(xs)[1:2], c(12, 1))

  xc <- subtract_control(x, control = "control")
  vals <- assay(xc)[design(xc)$group == "control", "eGFP-Peak-Peak", "T001", "1;2"]
  expect_equal(mean(vals, na.rm = TRUE), 0, tolerance = 1e-10)

  xg <- average_by_group(x, by = "group")
  expect_equal(dim(xg), c(2, 2, 3, 12))
  expect_equal(design(xg)$group, c("control", "treated"))
})

test_that("long data frame contains descriptors and design", {
  x <- make_kinetic()
  d <- data.frame(well = sampleData(x)$well,
                  group = rep(c("control", "treated"), each = 12),
                  stringsAsFactors = FALSE)
  x <- set_design(x, d, by = "well")
  df <- as.data.frame(x)
  expect_equal(nrow(df), prod(dim(x)))
  expect_true(all(c("sample_id", "well", "group", "channel_id", "time_id",
                    "position_id", "value", "grid_row", "grid_column") %in% names(df)))
})

test_that("plot helpers return ggplot objects", {
  skip_if_not_installed("ggplot2")
  x <- make_kinetic()
  d <- data.frame(well = sampleData(x)$well,
                  group = rep(c("control", "treated"), each = 12),
                  biological_sample = paste0("sample_", seq_len(24)),
                  stringsAsFactors = FALSE)
  x <- set_design(x, d, by = "well")
  p1 <- plot_violin(x, group_by = "group", channels = "eGFP-Peak-Peak",
                    times = "T001", colour_by = "biological_sample",
                    show_positions = TRUE)
  p2 <- plot_timeseries(x, by = "group", channels = "eGFP-Peak-Peak")
  p3 <- plot_boxplot(x, group_by = "group", channels = "eGFP-Peak-Peak",
                     times = "T001", colour_by = "biological_sample",
                     show_positions = TRUE)
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
  expect_equal(nrow(p3$layers[[1]]$data), 24L)
  expect_equal(nrow(p3$layers[[2]]$data), 24L * 12L)
})

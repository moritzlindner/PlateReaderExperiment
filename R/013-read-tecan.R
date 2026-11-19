#' Read Tecan SparkControl Excel results
#'
#' Automatically detects the supported SparkControl export layout and converts
#' all supported formats to one PlateReaderExperiment representation. A folder
#' of single-time plate-matrix or multi-position exports is interpreted as a time
#' series; a kinetic multi-read export contains its time points in one workbook.
#' Interrupted single-time multi-read exports are imported best-effort: observed
#' values are retained and missing measurements or timestamps are stored as `NA`.
#'
#' @param path Excel workbook or folder containing workbooks.
#' @param sheet Expected result worksheet name. If a workbook has exactly one
#'   worksheet, that worksheet is accepted automatically.
#' @param pattern File-selection regular expression when path is a folder.
#' @param recursive Search subdirectories.
#' @param order_by For folders of single-time exports, order time points by
#'   start time or file name.
#' @param tz Time zone for timestamps lacking an explicit zone.
#' @return PlateReaderExperiment.
#' @export
read.tecan <- function(
    path,
    sheet = "Result sheet",
    pattern = "\\.(xlsx|xlsm)$",
    recursive = FALSE,
    order_by = c("start_time", "file"),
    tz = Sys.timezone()) {

  order_by <- match.arg(order_by)
  if (is.na(tz) || !nzchar(tz)) tz <- "UTC"

  if (dir.exists(path)) {
    files <- list.files(path, pattern = pattern, full.names = TRUE,
                        recursive = recursive, ignore.case = TRUE)
    files <- files[!grepl("^~\\$", basename(files))]
    files <- sort(files, method = "radix")
    if (!length(files)) .pre_abort("No Tecan Excel workbooks found in '%s'.", path)
    formats <- vapply(files, .tecan_detect_format, character(1L), sheet = sheet)

    if (length(unique(formats)) != 1L) {
      .pre_abort("Folder contains mixed Tecan export formats: %s",
                 paste(sprintf("%s=%s", basename(files), formats), collapse = "; "))
    }
    if (formats[[1L]] == "plate_matrix") {
      return(.tecan_build_plate_experiment(files, sheet, tz, order_by))
    }
    if (formats[[1L]] == "single_time_multi_read") {
      return(.tecan_build_single_multiread_experiment(files, sheet, tz, order_by))
    }
    if (length(files) != 1L) {
      .pre_abort(paste0(
        "A folder with multiple kinetic multi-read workbooks is ambiguous: files could be ",
        "replicate plates rather than consecutive time points. Read each workbook separately."
      ))
    }
    return(.tecan_build_kinetic_experiment(files[[1L]], sheet, tz))
  }

  if (!file.exists(path)) .pre_abort("Path does not exist: %s", path)
  format <- .tecan_detect_format(path, sheet)
  if (format == "plate_matrix") {
    .tecan_build_plate_experiment(path, sheet, tz, order_by)
  } else if (format == "single_time_multi_read") {
    .tecan_build_single_multiread_experiment(path, sheet, tz, order_by)
  } else if (format == "kinetic_multi_read") {
    .tecan_build_kinetic_experiment(path, sheet, tz)
  } else {
    .pre_abort("Unsupported Tecan format '%s'.", format)
  }
}

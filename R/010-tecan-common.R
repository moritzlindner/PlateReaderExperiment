.tecan_repair_xlsx <- function(file) {
  unzip_bin <- Sys.which("unzip")
  if (!nzchar(unzip_bin)) return(NULL)
  tmp <- tempfile("tecan_xlsx_")
  dir.create(tmp)
  old <- getwd()
  on.exit({
    try(setwd(old), silent = TRUE)
    unlink(tmp, recursive = TRUE, force = TRUE)
  }, add = TRUE)
  suppressWarnings(system2(unzip_bin, c("-o", shQuote(normalizePath(file)), "-d", shQuote(tmp)),
                           stdout = FALSE, stderr = FALSE))
  if (!file.exists(file.path(tmp, "xl", "workbook.xml"))) return(NULL)

  ct <- file.path(tmp, "[Content_Types].xml")
  if (!file.exists(ct)) {
    sheets <- list.files(file.path(tmp, "xl", "worksheets"), pattern = "\\.xml$", full.names = FALSE)
    overrides <- paste0(
      '<Override PartName="/xl/worksheets/', sheets,
      '" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
      collapse = ""
    )
    text <- paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
      '<Default Extension="xml" ContentType="application/xml"/>',
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
      overrides,
      '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
      if (file.exists(file.path(tmp, "xl", "sharedStrings.xml")))
        '<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>' else "",
      '</Types>'
    )
    writeLines(text, ct, useBytes = TRUE)
  }

  rel_dir <- file.path(tmp, "_rels")
  dir.create(rel_dir, showWarnings = FALSE)
  rel <- file.path(rel_dir, ".rels")
  if (!file.exists(rel)) {
    writeLines(paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
      '</Relationships>'
    ), rel, useBytes = TRUE)
  }

  repaired <- tempfile(fileext = ".xlsx")
  setwd(tmp)
  files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!dir.exists(files)]
  status <- tryCatch({
    utils::zip(repaired, files = files, flags = "-r9X")
    0L
  }, error = function(e) 1L)
  if (status != 0L || !file.exists(repaired)) return(NULL)
  repaired
}

.tecan_read_matrix <- function(file, sheet = "Result sheet") {
  read_one <- function(path) {
    sheets <- openxlsx::getSheetNames(path)
    chosen <- sheet
    if (!(chosen %in% sheets)) {
      if (length(sheets) == 1L) chosen <- sheets[[1L]] else
        .pre_abort("Worksheet '%s' is absent in '%s'. Available: %s",
                   sheet, basename(file), paste(sheets, collapse = ", "))
    }
    raw <- openxlsx::read.xlsx(
      path, sheet = chosen, colNames = FALSE,
      skipEmptyRows = FALSE, skipEmptyCols = FALSE,
      check.names = FALSE, detectDates = FALSE
    )
    m <- as.matrix(raw)
    storage.mode(m) <- "character"
    m[is.na(m)] <- ""
    m[] <- trimws(m)
    m
  }

  out <- tryCatch(read_one(file), error = identity)
  if (inherits(out, "error")) {
    repaired <- .tecan_repair_xlsx(file)
    if (is.null(repaired)) {
      .pre_abort("Could not read Tecan workbook '%s' with openxlsx: %s",
                 basename(file), conditionMessage(out))
    }
    on.exit(unlink(repaired, force = TRUE), add = TRUE)
    out <- tryCatch(read_one(repaired), error = identity)
    if (inherits(out, "error")) {
      .pre_abort("Could not read Tecan workbook '%s', including repair attempt: %s",
                 basename(file), conditionMessage(out))
    }
  }
  if (nrow(out) == 0L || ncol(out) < 2L) .pre_abort("Workbook '%s' is empty.", basename(file))
  out
}

.tecan_row_value <- function(m, row) {
  .pre_nonblank(m[row, -1L, drop = TRUE])
}

.tecan_find_plate <- function(m, first_channel_row, file) {
  rr <- which(m[, 1L] == "Plate area" & seq_len(nrow(m)) < first_channel_row)
  if (length(rr) != 1L) .pre_abort("'%s': expected one Plate area before channels.", basename(file))
  area <- .tecan_row_value(m, rr[[1L]])
  wells <- tryCatch(.pre_expand_wells(area), error = function(e) {
    .pre_abort("'%s': invalid Plate area '%s': %s", basename(file), area, conditionMessage(e))
  })
  name_rows <- which(m[, 1L] == "Name" & seq_len(nrow(m)) < rr[[1L]])
  plate_name <- if (length(name_rows)) .tecan_row_value(m, max(name_rows)) else NA_character_
  layout_rows <- which(m[, 1L] == "Plate layout" & seq_len(nrow(m)) < first_channel_row)
  layout <- if (length(layout_rows)) .tecan_row_value(m, layout_rows[[1L]]) else NA_character_
  list(name = plate_name, area = area, wells = wells, layout = layout)
}

.tecan_general_info <- function(m, stop_row) {
  z <- list()
  for (r in seq_len(max(0L, stop_row - 1L))) {
    label <- m[r, 1L]
    if (!nzchar(label)) next
    val <- .tecan_row_value(m, r)
    if (is.na(val) && grepl(":", label, fixed = TRUE)) {
      parts <- strsplit(label, ":", fixed = TRUE)[[1L]]
      key <- trimws(parts[[1L]])
      val <- trimws(paste(parts[-1L], collapse = ":"))
    } else {
      key <- sub(":$", "", label)
    }
    z[[length(z) + 1L]] <- data.frame(row = r, key = key, value = val, stringsAsFactors = FALSE)
  }
  if (length(z)) do.call(rbind, z) else data.frame(row = integer(), key = character(), value = character())
}

.tecan_metadata_rows <- function(m, from, to) {
  out <- list()
  for (r in seq.int(from, to)) {
    key <- m[r, 1L]
    if (!nzchar(key)) next
    val <- .tecan_row_value(m, r)
    if (is.na(val)) next
    out[[key]] <- val
  }
  out
}

.tecan_channel_df <- function(blocks, tz = "UTC") {
  labels <- unique(unlist(lapply(blocks, function(z) names(z$metadata)), use.names = FALSE))
  rows <- vector("list", length(blocks))
  for (i in seq_along(blocks)) {
    md <- blocks[[i]]$metadata
    row <- list(channel_id = blocks[[i]]$channel, channel_name = blocks[[i]]$channel)
    for (label in labels) {
      if (label %in% c("Name", "Start Time", "End Time", "Temperature [°C]")) next
      nm <- .pre_clean_name(label)
      v <- md[[label]] %||% NA_character_
      n <- .pre_number(v)
      row[[nm]] <- if (!is.na(n) && grepl("^[+-]?[0-9.,eE]+$", v)) n else v
    }
    rows[[i]] <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  }
  # rbind while allowing differing metadata columns
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(z) {
    for (nm in setdiff(all_names, names(z))) z[[nm]] <- NA
    z[, all_names, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.tecan_cell_number <- function(x, file, row, context, allow_missing = TRUE) {
  if (is.na(x) || !nzchar(trimws(as.character(x)))) {
    if (allow_missing) return(NA_real_)
    .pre_abort("'%s' row %d: missing %s.", basename(file), row, context)
  }

  token <- toupper(trimws(as.character(x)))
  if (token %in% c("OVER", "INVALID")) {
    warning(
      sprintf(
        "'%s' row %d: %s value '%s' was reported by SparkControl and has been converted to NA.",
        basename(file), row, context, token
      ),
      call. = FALSE
    )
    return(NA_real_)
  }

  n <- .pre_number(x)
  if (is.na(n)) {
    .pre_abort("'%s' row %d: non-numeric %s '%s'.", basename(file), row, context, x)
  }
  n
}


.tecan_is_interrupted <- function(m) {
  warning_rows <- which(m[, 1L] == "Warning")
  if (!length(warning_rows)) return(FALSE)
  warning_text <- vapply(
    warning_rows,
    function(r) .tecan_row_value(m, r),
    character(1L)
  )
  any(grepl("interrupted by the user", warning_text, ignore.case = TRUE),
      na.rm = TRUE)
}

.tecan_infer_multiread_positions <- function(metadata) {
  labels <- grep("^Multiple Reads per Well", names(metadata), value = TRUE)
  if (!length(labels)) return(character())

  nr <- nc <- NA_integer_
  geometry <- ""
  for (label in labels) {
    value <- metadata[[label]] %||% NA_character_
    text <- paste(label, value)
    hit <- regexec("([0-9]+)\\s*[xX×]\\s*([0-9]+)", text, perl = TRUE)
    parts <- regmatches(text, hit)[[1L]]
    if (length(parts) == 3L) {
      nr <- suppressWarnings(as.integer(parts[[2L]]))
      nc <- suppressWarnings(as.integer(parts[[3L]]))
      geometry <- tolower(label)
      break
    }
  }
  if (is.na(nr) || is.na(nc) || nr < 1L || nc < 1L) return(character())

  grid <- do.call(rbind, lapply(seq_len(nr), function(rr) {
    data.frame(row = rr, column = seq_len(nc))
  }))

  # SparkControl's filled-circle layout omits grid points outside the circular
  # read area. Normalising by half the declared grid size reproduces the common
  # 4 x 4 layout (12 positions, i.e. the four corners are omitted) and extends
  # naturally to other grid sizes.
  if (grepl("circle", geometry, fixed = TRUE)) {
    yy <- (grid$row - (nr + 1) / 2) / (nr / 2)
    xx <- (grid$column - (nc + 1) / 2) / (nc / 2)
    grid <- grid[(xx * xx + yy * yy) <= (1 + 1e-10), , drop = FALSE]
  }

  paste0(grid$row, ";", grid$column)
}

.tecan_detect_format <- function(file, sheet = "Result sheet") {
  m <- .tecan_read_matrix(file, sheet)

  if (any(m[, 1L] == "Cycles / Well")) return("kinetic_multi_read")
  mode_rows <- which(m[, 1L] == "Mode")
  mode_vals <- vapply(mode_rows, function(r) .tecan_row_value(m, r), character(1L))
  if (any(mode_vals == "Kinetic")) return("kinetic_multi_read")
  if (any(m[, 1L] == "<>")) return("plate_matrix")
  multi_read <- any(grepl("^Multiple Reads per Well", m[, 1L]))
  if (multi_read) return("single_time_multi_read")
  .pre_abort("Unsupported Tecan SparkControl result format in '%s'.", basename(file))
}

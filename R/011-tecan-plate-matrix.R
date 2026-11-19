.tecan_parse_plate_channel <- function(m, start_row, end_row, file, tz) {
  block <- m[start_row:end_row, , drop = FALSE]
  header <- which(block[, 1L] == "<>")
  if (length(header) != 1L) {
    .pre_abort("'%s' row %d: expected one <> plate-data header in channel block.", basename(file), start_row)
  }
  header <- header[[1L]]
  end_local <- which(block[, 1L] == "End Time")
  if (length(end_local) != 1L || end_local[[1L]] <= header) {
    .pre_abort("'%s' row %d: missing End Time after plate matrix.", basename(file), start_row)
  }
  end_local <- end_local[[1L]]
  metadata <- .tecan_metadata_rows(block, 1L, header - 1L)
  metadata[["End Time"]] <- .tecan_row_value(block, end_local)
  required <- c("Mode", "Name", "Excitation wavelength [nm]", "Emission wavelength [nm]",
                "Part of Plate", "Start Time", "End Time")
  miss <- setdiff(required, names(metadata))
  if (length(miss)) .pre_abort("'%s' row %d: channel metadata missing: %s",
                               basename(file), start_row, paste(miss, collapse = ", "))
  channel <- metadata[["Name"]]
  if (!nzchar(channel)) .pre_abort("'%s' row %d: empty channel name.", basename(file), start_row)

  col_text <- block[header, -1L, drop = TRUE]
  used <- which(nzchar(col_text))
  if (!length(used) || !identical(used, seq_len(max(used)))) {
    .pre_abort("'%s' row %d: non-contiguous plate columns.", basename(file), start_row + header - 1L)
  }
  cols <- suppressWarnings(as.integer(col_text[used]))
  if (anyNA(cols) || anyDuplicated(cols)) {
    .pre_abort("'%s' row %d: invalid plate column labels.", basename(file), start_row + header - 1L)
  }

  data_start <- header + 1L
  data_stop <- data_start - 1L
  while (data_stop + 1L < end_local && grepl("^[A-Za-z]+$", block[data_stop + 1L, 1L])) {
    data_stop <- data_stop + 1L
  }
  if (data_stop < data_start) .pre_abort("'%s': channel '%s' has no plate rows.", basename(file), channel)
  rows <- toupper(block[data_start:data_stop, 1L])
  if (anyDuplicated(rows)) .pre_abort("'%s': duplicate plate row labels.", basename(file))

  measured <- tryCatch(.pre_expand_wells(metadata[["Part of Plate"]]), error = function(e) {
    .pre_abort("'%s': invalid Part of Plate for channel '%s': %s",
               basename(file), channel, conditionMessage(e))
  })
  values <- setNames(rep(NA_real_, length(measured)), measured)
  for (k in seq_along(measured)) {
    pos <- .pre_split_well(measured[[k]])
    rr <- match(pos$row, rows)
    cc <- match(pos$column, cols)
    if (is.na(rr) || is.na(cc)) {
      .pre_abort("'%s': declared well %s is absent from channel '%s' matrix.",
                 basename(file), measured[[k]], channel)
    }
    sheet_row <- start_row + data_start + rr - 2L
    values[[k]] <- .tecan_cell_number(
      block[data_start + rr - 1L, cc + 1L], file, sheet_row,
      sprintf("measurement for %s/%s", channel, measured[[k]]), TRUE
    )
  }

  start_time <- .pre_parse_datetime(metadata[["Start Time"]], tz)
  end_time <- .pre_parse_datetime(metadata[["End Time"]], tz)
  if (is.na(start_time) || is.na(end_time)) {
    .pre_abort("'%s': could not parse start/end time for channel '%s'.", basename(file), channel)
  }
  list(
    channel = channel,
    metadata = metadata,
    measured_wells = measured,
    values = values,
    start_time = start_time,
    end_time = end_time
  )
}

.tecan_parse_plate_file <- function(file, sheet, tz) {
  m <- .tecan_read_matrix(file, sheet)
  mode_rows <- which(m[, 1L] == "Mode")
  if (!length(mode_rows)) .pre_abort("'%s': no channel Mode blocks found.", basename(file))
  mode_values <- vapply(mode_rows, function(r) .tecan_row_value(m, r), character(1L))
  if (any(mode_values == "Kinetic")) .pre_abort("'%s' is kinetic, not plate-matrix format.", basename(file))

  plate <- .tecan_find_plate(m, mode_rows[[1L]], file)
  blocks <- vector("list", length(mode_rows))
  for (i in seq_along(mode_rows)) {
    end <- if (i < length(mode_rows)) mode_rows[[i + 1L]] - 1L else nrow(m)
    blocks[[i]] <- .tecan_parse_plate_channel(m, mode_rows[[i]], end, file, tz)
  }
  channels <- vapply(blocks, `[[`, character(1L), "channel")
  if (anyDuplicated(channels)) .pre_abort("'%s': duplicate channel names.", basename(file))
  measured <- unique(unlist(lapply(blocks, `[[`, "measured_wells"), use.names = FALSE))
  outside <- setdiff(measured, plate$wells)
  if (length(outside)) .pre_abort("'%s': measured wells outside Plate area: %s",
                                  basename(file), paste(outside, collapse = ", "))
  list(
    file = normalizePath(file, winslash = "/", mustWork = TRUE),
    plate = plate,
    blocks = blocks,
    channels = channels,
    measured_wells = plate$wells[plate$wells %in% measured],
    representative_time = min(vapply(blocks, function(z) as.numeric(z$start_time), numeric(1L))),
    general_info = .tecan_general_info(m, mode_rows[[1L]])
  )
}

.tecan_static_channel_signature <- function(block) {
  md <- block$metadata
  md[c("Start Time", "End Time", "Temperature [°C]")] <- NULL
  paste(names(md), unlist(md, use.names = FALSE), sep = "=", collapse = "\r")
}

.tecan_validate_plate_files <- function(parsed) {
  template <- parsed[[1L]]
  for (i in seq_along(parsed)[-1L]) {
    x <- parsed[[i]]
    if (!identical(x$plate$wells, template$plate$wells) ||
        !identical(x$plate$name, template$plate$name) ||
        !identical(x$plate$layout, template$plate$layout)) {
      .pre_abort("Tecan plate definition in '%s' does not match the first file.", basename(x$file))
    }
    if (!identical(x$channels, template$channels)) {
      .pre_abort("Tecan channel order/names in '%s' do not match the first file.", basename(x$file))
    }
    for (ch in seq_along(x$blocks)) {
      if (!identical(.tecan_static_channel_signature(x$blocks[[ch]]),
                     .tecan_static_channel_signature(template$blocks[[ch]]))) {
        .pre_abort("Static metadata for channel '%s' in '%s' do not match the first file.",
                   x$channels[[ch]], basename(x$file))
      }
    }
  }
  invisible(TRUE)
}

.tecan_build_plate_experiment <- function(files, sheet, tz, order_by = c("start_time", "file")) {
  order_by <- match.arg(order_by)
  parsed <- lapply(files, .tecan_parse_plate_file, sheet = sheet, tz = tz)
  .tecan_validate_plate_files(parsed)
  if (order_by == "start_time") {
    oo <- order(vapply(parsed, `[[`, numeric(1L), "representative_time"), basename(files))
    parsed <- parsed[oo]
    files <- files[oo]
  }
  template <- parsed[[1L]]
  samples <- template$measured_wells
  channels <- template$channels
  nt <- length(parsed)
  time_ids <- sprintf("T%03d", seq_len(nt))
  position_ids <- "P001"

  arr <- array(
    NA_real_, dim = c(length(samples), length(channels), nt, 1L),
    dimnames = list(sample = samples, channel = channels, time = time_ids, position = position_ids)
  )
  for (tt in seq_along(parsed)) {
    for (ch in seq_along(channels)) {
      b <- parsed[[tt]]$blocks[[ch]]
      si <- match(names(b$values), samples)
      arr[si, ch, tt, 1L] <- unname(b$values)
    }
  }

  origins <- vapply(parsed, function(z) min(vapply(z$blocks, function(b) as.numeric(b$start_time), numeric(1L))), numeric(1L))
  experiment_origin <- min(origins)
  observed <- origins - experiment_origin
  time_data <- data.frame(
    time_id = time_ids,
    time_index = seq_len(nt),
    cycle = NA_integer_,
    nominal_time_s = observed,
    observed_time_s = observed,
    source_file = basename(files),
    stringsAsFactors = FALSE
  )
  pos_data <- data.frame(
    position_id = position_ids,
    position_index = 1L,
    grid_row = NA_integer_, grid_column = NA_integer_,
    position_type = "single_read",
    stringsAsFactors = FALSE
  )
  sample_data <- .pre_well_df(samples)
  channel_data <- .tecan_channel_df(template$blocks, tz)

  acq <- vector("list", length(samples) * length(channels) * nt)
  k <- 0L
  for (tt in seq_along(parsed)) {
    for (ch in seq_along(channels)) {
      b <- parsed[[tt]]$blocks[[ch]]
      temp <- .pre_number(b$metadata[["Temperature [°C]"]] %||% NA_character_)
      for (s in seq_along(samples)) {
        k <- k + 1L
        kname <- samples[[s]]
        val <- if (kname %in% names(b$values)) b$values[[kname]] else NA_real_
        acq[[k]] <- data.frame(
          sample_id = kname,
          channel_id = channels[[ch]],
          time_id = time_ids[[tt]],
          relative_time_s = 0,
          actual_time_s = as.numeric(b$start_time) - experiment_origin,
          timestamp = b$start_time,
          end_timestamp = b$end_time,
          temperature_c = temp,
          reported_mean = val,
          reported_sd = NA_real_,
          n_positions_observed = as.integer(!is.na(val)),
          valid = !is.na(val),
          source_file = basename(parsed[[tt]]$file),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  acq <- do.call(rbind, acq)
  rownames(acq) <- NULL

  PlateReaderExperiment(
    data = arr,
    sampleData = sample_data,
    channelData = channel_data,
    timeData = time_data,
    positionData = pos_data,
    acquisitionData = acq,
    metadata = list(
      source = "Tecan SparkControl",
      source_format = "plate_matrix",
      source_files = normalizePath(files, winslash = "/", mustWork = TRUE),
      timezone = tz,
      plate = template$plate,
      experiment_info = template$general_info,
      processing_history = list()
    )
  )
}

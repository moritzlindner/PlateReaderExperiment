.tecan_parse_single_multiread_channel <- function(
    m, start_row, end_row, file, tz, interrupted = FALSE) {

  block <- m[start_row:end_row, , drop = FALSE]

  well_rows <- which(block[, 1L] == "Well")
  if (length(well_rows) > 1L) {
    .pre_abort("'%s' row %d: expected at most one Well header in single-time multi-read channel block.",
               basename(file), start_row)
  }
  if (!length(well_rows) && !interrupted) {
    .pre_abort("'%s' row %d: expected one Well header in single-time multi-read channel block.",
               basename(file), start_row)
  }
  well_row <- if (length(well_rows)) well_rows[[1L]] else NA_integer_

  metadata_stop <- if (is.na(well_row)) nrow(block) else max(1L, well_row - 1L)
  metadata <- .tecan_metadata_rows(block, 1L, metadata_stop)
  metadata[grepl("^[0-9]+;[0-9]+$", names(metadata))] <- NULL
  metadata[c("Warning", "Mean", "StDev")] <- NULL

  # Preserve timestamp fields even when SparkControl emitted their labels but
  # left their values blank after an interrupted measurement.
  for (label in c("Start Time", "End Time")) {
    rows <- which(block[, 1L] == label)
    if (length(rows)) metadata[[label]] <- .tecan_row_value(block, rows[[1L]])
  }

  required <- c("Mode", "Name", "Part of Plate")
  miss <- setdiff(required, names(metadata))
  if (length(miss)) {
    .pre_abort("'%s' row %d: single-time multi-read channel metadata missing: %s",
               basename(file), start_row, paste(miss, collapse = ", "))
  }

  multi_labels <- grep("^Multiple Reads per Well", names(metadata), value = TRUE)
  if (!length(multi_labels)) {
    .pre_abort("'%s' row %d: channel '%s' has no Multiple Reads per Well metadata.",
               basename(file), start_row, metadata[["Name"]])
  }

  channel <- metadata[["Name"]]
  if (is.na(channel) || !nzchar(channel)) {
    .pre_abort("'%s' row %d: empty channel name.", basename(file), start_row)
  }

  start_time <- .pre_parse_datetime(metadata[["Start Time"]] %||% NA_character_, tz)
  end_time <- .pre_parse_datetime(metadata[["End Time"]] %||% NA_character_, tz)
  if (!interrupted && (is.na(start_time) || is.na(end_time))) {
    .pre_abort("'%s': could not parse start/end time for channel '%s'.",
               basename(file), channel)
  }

  # Preserve the explicit SparkControl position-grid order when available.
  # Position labels appearing only in the result rows may be a partial set after
  # interruption, so they are not treated as the complete declared grid.
  layout_positions <- character()
  if (!is.na(well_row) && well_row > 1L) {
    for (rr in seq_len(well_row - 1L)) {
      for (cc in seq_len(ncol(block))) {
        token <- block[rr, cc]
        if (grepl("^[0-9]+;[0-9]+$", token)) {
          layout_positions <- c(layout_positions, token)
        }
      }
    }
  }
  layout_positions <- unique(layout_positions)

  explicit_positions <- character()
  for (rr in seq_len(nrow(block))) {
    for (cc in seq_len(ncol(block))) {
      token <- block[rr, cc]
      if (grepl("^[0-9]+;[0-9]+$", token)) {
        explicit_positions <- c(explicit_positions, token)
      }
    }
  }
  explicit_positions <- unique(explicit_positions)

  inferred_positions <- .tecan_infer_multiread_positions(metadata)
  positions <- if (length(layout_positions)) {
    layout_positions
  } else if (interrupted && length(inferred_positions)) {
    unique(c(inferred_positions, explicit_positions))
  } else {
    explicit_positions
  }
  if (!length(positions)) {
    .pre_abort("'%s': channel '%s' has Multiple Reads per Well but its position grid cannot be determined.",
               basename(file), channel)
  }

  declared_wells <- tryCatch(.pre_expand_wells(metadata[["Part of Plate"]]), error = function(e) {
    .pre_abort("'%s': invalid Part of Plate for channel '%s': %s",
               basename(file), channel, conditionMessage(e))
  })

  values <- matrix(
    NA_real_, nrow = length(positions), ncol = length(declared_wells),
    dimnames = list(position = positions, well = declared_wells)
  )
  reported_mean <- setNames(rep(NA_real_, length(declared_wells)), declared_wells)
  reported_sd <- setNames(rep(NA_real_, length(declared_wells)), declared_wells)

  # A channel that was never started may have metadata but no Well matrix at
  # all. Its full declared assay slice is retained as NA.
  if (is.na(well_row)) {
    return(list(
      channel = channel,
      metadata = metadata,
      positions = positions,
      declared_wells = declared_wells,
      values = values,
      reported_mean = reported_mean,
      reported_sd = reported_sd,
      start_time = start_time,
      end_time = end_time,
      interrupted = interrupted,
      position_grid_inferred = !length(layout_positions)
    ))
  }

  well_text <- block[well_row, -1L, drop = TRUE]
  used <- which(nzchar(well_text))
  if (!length(used)) {
    if (interrupted) {
      return(list(
        channel = channel,
        metadata = metadata,
        positions = positions,
        declared_wells = declared_wells,
        values = values,
        reported_mean = reported_mean,
        reported_sd = reported_sd,
        start_time = start_time,
        end_time = end_time,
        interrupted = interrupted,
        position_grid_inferred = !length(layout_positions)
      ))
    }
    .pre_abort("'%s' row %d: well columns are missing.",
               basename(file), start_row + well_row - 1L)
  }
  if (!identical(used, seq_len(max(used)))) {
    .pre_abort("'%s' row %d: well columns are non-contiguous.",
               basename(file), start_row + well_row - 1L)
  }

  wells_in_matrix <- toupper(well_text[used])
  for (well in wells_in_matrix) {
    tryCatch(.pre_split_well(well), error = function(e) {
      .pre_abort("'%s' row %d: invalid well identifier '%s'.",
                 basename(file), start_row + well_row - 1L, well)
    })
  }
  if (anyDuplicated(wells_in_matrix)) {
    .pre_abort("'%s' row %d: duplicate well identifiers in multi-read matrix.",
               basename(file), start_row + well_row - 1L)
  }
  excel_cols <- used + 1L
  names(excel_cols) <- wells_in_matrix

  missing_declared <- setdiff(declared_wells, wells_in_matrix)
  if (length(missing_declared) && !interrupted) {
    .pre_abort("'%s': channel '%s' declares wells absent from its Well matrix: %s",
               basename(file), channel, paste(missing_declared, collapse = ", "))
  }

  end_rows <- which(block[, 1L] == "End Time" & seq_len(nrow(block)) > well_row)
  end_local <- if (length(end_rows)) end_rows[[1L]] else NA_integer_
  data_stop <- if (is.na(end_local)) nrow(block) else end_local - 1L
  data_rows <- if (data_stop >= well_row + 1L) {
    seq.int(well_row + 1L, data_stop)
  } else {
    integer()
  }

  mean_r <- data_rows[block[data_rows, 1L] == "Mean"]
  sd_r <- data_rows[block[data_rows, 1L] == "StDev"]
  if (length(mean_r) > 1L || length(sd_r) > 1L ||
      (!interrupted && (length(mean_r) != 1L || length(sd_r) != 1L))) {
    .pre_abort("'%s': channel '%s' requires at most one Mean and StDev row%s.",
               basename(file), channel, if (interrupted) "" else " (exactly one in a complete export)")
  }

  for (well in intersect(declared_wells, wells_in_matrix)) {
    cc <- excel_cols[[well]]
    if (length(mean_r) == 1L) {
      reported_mean[[well]] <- .tecan_cell_number(
        block[mean_r, cc], file, start_row + mean_r - 1L,
        sprintf("reported mean for %s/%s", channel, well), TRUE
      )
    }
    if (length(sd_r) == 1L) {
      reported_sd[[well]] <- .tecan_cell_number(
        block[sd_r, cc], file, start_row + sd_r - 1L,
        sprintf("reported SD for %s/%s", channel, well), TRUE
      )
    }
  }

  for (pp in seq_along(positions)) {
    pr <- data_rows[block[data_rows, 1L] == positions[[pp]]]
    if (length(pr) > 1L) {
      .pre_abort("'%s': channel '%s' position row '%s' is duplicated.",
                 basename(file), channel, positions[[pp]])
    }
    if (!length(pr)) {
      if (!interrupted) {
        .pre_abort("'%s': channel '%s' position row '%s' is missing.",
                   basename(file), channel, positions[[pp]])
      }
      next
    }
    for (well in intersect(declared_wells, wells_in_matrix)) {
      cc <- excel_cols[[well]]
      values[pp, well] <- .tecan_cell_number(
        block[pr, cc], file, start_row + pr - 1L,
        sprintf("measurement for %s/%s/%s", channel, well, positions[[pp]]), TRUE
      )
    }
  }

  # SparkControl reports the across-position mean and sample SD. Verify these
  # summaries whenever both summary and underlying position data are present.
  for (well in declared_wells) {
    v <- values[, well]
    complete_positions <- length(v) > 0L && all(!is.na(v))
    calc_mean <- .pre_mean_or_na(v, TRUE)
    rep_mean <- reported_mean[[well]]
    if (complete_positions && !is.na(calc_mean) && !is.na(rep_mean)) {
      tol <- max(1e-6, abs(rep_mean) * 1e-8)
      if (abs(calc_mean - rep_mean) > tol) {
        .pre_abort("'%s' channel '%s' well %s: reported Mean does not match position values.",
                   basename(file), channel, well)
      }
    }

    observed <- v[!is.na(v)]
    calc_sd <- if (length(observed) >= 2L) stats::sd(observed) else NA_real_
    rep_sd <- reported_sd[[well]]
    if (complete_positions && !is.na(calc_sd) && !is.na(rep_sd)) {
      tol <- max(1e-6, abs(rep_sd) * 1e-8)
      if (abs(calc_sd - rep_sd) > tol) {
        .pre_abort("'%s' channel '%s' well %s: reported StDev does not match position values.",
                   basename(file), channel, well)
      }
    }
  }

  list(
    channel = channel,
    metadata = metadata,
    positions = positions,
    declared_wells = declared_wells,
    values = values,
    reported_mean = reported_mean,
    reported_sd = reported_sd,
    start_time = start_time,
    end_time = end_time,
    interrupted = interrupted,
    position_grid_inferred = !length(layout_positions)
  )
}

.tecan_parse_single_multiread_file <- function(file, sheet, tz) {
  m <- .tecan_read_matrix(file, sheet)
  interrupted <- .tecan_is_interrupted(m)
  mode_rows <- which(m[, 1L] == "Mode")
  if (!length(mode_rows)) .pre_abort("'%s': no channel Mode blocks found.", basename(file))
  mode_values <- vapply(mode_rows, function(r) .tecan_row_value(m, r), character(1L))
  if (any(mode_values == "Kinetic")) {
    .pre_abort("'%s' is kinetic, not single-time multi-read format.", basename(file))
  }

  plate <- .tecan_find_plate(m, mode_rows[[1L]], file)
  blocks <- vector("list", length(mode_rows))
  for (i in seq_along(mode_rows)) {
    end <- if (i < length(mode_rows)) mode_rows[[i + 1L]] - 1L else nrow(m)
    blocks[[i]] <- .tecan_parse_single_multiread_channel(
      m, mode_rows[[i]], end, file, tz, interrupted = interrupted
    )
  }

  channels <- vapply(blocks, `[[`, character(1L), "channel")
  if (anyDuplicated(channels)) .pre_abort("'%s': duplicate channel names.", basename(file))

  declared_union <- unique(unlist(lapply(blocks, `[[`, "declared_wells"), use.names = FALSE))
  outside <- setdiff(declared_union, plate$wells)
  if (length(outside)) {
    .pre_abort("'%s': declared wells outside Plate area: %s",
               basename(file), paste(outside, collapse = ", "))
  }
  samples <- plate$wells[plate$wells %in% declared_union]
  positions <- unique(unlist(lapply(blocks, `[[`, "positions"), use.names = FALSE))

  start_times <- vapply(blocks, function(z) as.numeric(z$start_time), numeric(1L))
  finite_start <- is.finite(start_times)
  representative_time <- if (any(finite_start)) min(start_times[finite_start]) else NA_real_

  list(
    file = normalizePath(file, winslash = "/", mustWork = TRUE),
    plate = plate,
    blocks = blocks,
    channels = channels,
    samples = samples,
    positions = positions,
    representative_time = representative_time,
    interrupted = interrupted,
    general_info = .tecan_general_info(m, mode_rows[[1L]])
  )
}

.tecan_validate_single_multiread_files <- function(parsed) {
  template <- parsed[[1L]]
  if (length(parsed) <= 1L) return(invisible(TRUE))

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
    if (!identical(x$samples, template$samples) || !identical(x$positions, template$positions)) {
      .pre_abort("Tecan well/position layout in '%s' does not match the first file.", basename(x$file))
    }
    for (ch in seq_along(x$blocks)) {
      if (!identical(x$blocks[[ch]]$positions, template$blocks[[ch]]$positions) ||
          !identical(x$blocks[[ch]]$declared_wells, template$blocks[[ch]]$declared_wells)) {
        .pre_abort("Channel '%s' well/position layout in '%s' does not match the first file.",
                   x$channels[[ch]], basename(x$file))
      }
      if (!identical(.tecan_static_channel_signature(x$blocks[[ch]]),
                     .tecan_static_channel_signature(template$blocks[[ch]]))) {
        .pre_abort("Static metadata for channel '%s' in '%s' do not match the first file.",
                   x$channels[[ch]], basename(x$file))
      }
    }
  }
  invisible(TRUE)
}

.tecan_build_single_multiread_experiment <- function(
    files, sheet, tz, order_by = c("start_time", "file")) {

  order_by <- match.arg(order_by)
  parsed <- lapply(files, .tecan_parse_single_multiread_file, sheet = sheet, tz = tz)
  .tecan_validate_single_multiread_files(parsed)

  interrupted <- vapply(parsed, `[[`, logical(1L), "interrupted")
  if (any(interrupted)) {
    warning(
      sprintf(
        "Importing interrupted/incomplete Tecan export%s: %s. Observed measurements are retained; missing measurements and timestamps are represented by NA.",
        if (sum(interrupted) == 1L) "" else "s",
        paste(basename(vapply(parsed[interrupted], `[[`, character(1L), "file")), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (order_by == "start_time") {
    representative <- vapply(parsed, `[[`, numeric(1L), "representative_time")
    source_names <- basename(vapply(parsed, `[[`, character(1L), "file"))
    oo <- order(is.na(representative), representative, source_names)
    parsed <- parsed[oo]
    files <- files[oo]
  }

  template <- parsed[[1L]]
  samples <- template$samples
  channels <- template$channels
  positions <- template$positions
  nt <- length(parsed)
  time_ids <- sprintf("T%03d", seq_len(nt))

  arr <- array(
    NA_real_,
    dim = c(length(samples), length(channels), nt, length(positions)),
    dimnames = list(sample = samples, channel = channels, time = time_ids, position = positions)
  )

  for (tt in seq_along(parsed)) {
    for (ch in seq_along(channels)) {
      b <- parsed[[tt]]$blocks[[ch]]
      pi <- match(rownames(b$values), positions)
      si <- match(colnames(b$values), samples)
      arr[si, ch, tt, pi] <- t(b$values)
    }
  }

  origins <- vapply(parsed, `[[`, numeric(1L), "representative_time")
  finite_origins <- is.finite(origins)
  experiment_origin <- if (any(finite_origins)) min(origins[finite_origins]) else NA_real_
  observed <- if (is.finite(experiment_origin)) origins - experiment_origin else rep(NA_real_, length(origins))

  time_data <- data.frame(
    time_id = time_ids,
    time_index = seq_len(nt),
    cycle = NA_integer_,
    nominal_time_s = observed,
    observed_time_s = observed,
    source_file = basename(files),
    stringsAsFactors = FALSE
  )

  pos_bits <- strsplit(positions, ";", fixed = TRUE)
  position_data <- data.frame(
    position_id = positions,
    position_index = seq_along(positions),
    grid_row = vapply(pos_bits, function(v) {
      if (length(v) == 2L) suppressWarnings(as.integer(v[[1L]])) else NA_integer_
    }, integer(1L)),
    grid_column = vapply(pos_bits, function(v) {
      if (length(v) == 2L) suppressWarnings(as.integer(v[[2L]])) else NA_integer_
    }, integer(1L)),
    position_type = "within_well_grid",
    stringsAsFactors = FALSE
  )

  acq <- vector("list", length(samples) * length(channels) * nt)
  k <- 0L
  for (tt in seq_along(parsed)) {
    for (ch in seq_along(channels)) {
      b <- parsed[[tt]]$blocks[[ch]]
      temp <- .pre_number(b$metadata[["Temperature [°C]"]] %||% NA_character_)
      for (s in seq_along(samples)) {
        well <- samples[[s]]
        k <- k + 1L
        if (well %in% colnames(b$values)) {
          vals <- b$values[, well]
          rep_mean <- b$reported_mean[[well]]
          rep_sd <- b$reported_sd[[well]]
        } else {
          vals <- rep(NA_real_, length(positions))
          rep_mean <- NA_real_
          rep_sd <- NA_real_
        }
        acq[[k]] <- data.frame(
          sample_id = well,
          channel_id = channels[[ch]],
          time_id = time_ids[[tt]],
          relative_time_s = if (is.na(b$start_time)) NA_real_ else 0,
          actual_time_s = if (!is.na(b$start_time) && is.finite(experiment_origin)) {
            as.numeric(b$start_time) - experiment_origin
          } else {
            NA_real_
          },
          timestamp = b$start_time,
          end_timestamp = b$end_time,
          temperature_c = temp,
          reported_mean = rep_mean,
          reported_sd = rep_sd,
          n_positions_observed = sum(!is.na(vals)),
          valid = any(!is.na(vals)),
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
    sampleData = .pre_well_df(samples),
    channelData = .tecan_channel_df(template$blocks, tz),
    timeData = time_data,
    positionData = position_data,
    acquisitionData = acq,
    metadata = list(
      source = "Tecan SparkControl",
      source_format = "single_time_multi_read",
      source_files = normalizePath(files, winslash = "/", mustWork = TRUE),
      timezone = tz,
      plate = template$plate,
      experiment_info = template$general_info,
      incomplete_source = any(vapply(parsed, `[[`, logical(1L), "interrupted")),
      interrupted_files = vapply(
        parsed[vapply(parsed, `[[`, logical(1L), "interrupted")],
        `[[`, character(1L), "file"
      ),
      processing_history = list()
    )
  )
}

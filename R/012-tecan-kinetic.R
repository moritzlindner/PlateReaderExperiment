.tecan_hms_seconds <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_real_)
  z <- strsplit(x, ":", fixed = TRUE)[[1L]]
  if (length(z) != 3L) return(NA_real_)
  z <- suppressWarnings(as.numeric(z))
  if (anyNA(z)) return(NA_real_)
  z[[1L]] * 3600 + z[[2L]] * 60 + z[[3L]]
}

.tecan_parse_kinetic_channel <- function(m, start_row, end_row, file, tz) {
  channel <- NA_character_
  name_rows <- which(m[start_row:end_row, 1L] == "Name") + start_row - 1L
  if (length(name_rows)) channel <- .tecan_row_value(m, name_rows[[1L]])
  if (is.na(channel) || !nzchar(channel)) {
    .pre_abort("'%s' row %d: kinetic channel has no Name.", basename(file), start_row)
  }
  start_rows <- which(m[start_row:end_row, 1L] == "Start Time") + start_row - 1L
  end_rows <- which(m[start_row:end_row, 1L] == "End Time") + start_row - 1L
  if (length(start_rows) != 1L || length(end_rows) != 1L) {
    .pre_abort("'%s': channel '%s' requires one Start Time and End Time.", basename(file), channel)
  }
  sr <- start_rows[[1L]]
  er <- end_rows[[1L]]
  metadata <- .tecan_metadata_rows(m, start_row, sr)
  metadata[["End Time"]] <- .tecan_row_value(m, er)
  required <- c("Mode", "Name", "Part of Plate", "Start Time", "End Time")
  miss <- setdiff(required, names(metadata))
  if (length(miss)) .pre_abort("'%s': channel '%s' metadata missing: %s",
                               basename(file), channel, paste(miss, collapse = ", "))
  start_time <- .pre_parse_datetime(metadata[["Start Time"]], tz)
  end_time <- .pre_parse_datetime(metadata[["End Time"]], tz)
  if (is.na(start_time) || is.na(end_time)) .pre_abort("'%s': invalid channel timestamps.", basename(file))

  cycle_rows <- which(m[start_row:end_row, 1L] == "Cycles / Well") + start_row - 1L
  if (!length(cycle_rows)) .pre_abort("'%s': channel '%s' contains no Cycles / Well blocks.", basename(file), channel)

  title_rows <- which(m[start_row:(cycle_rows[[1L]] - 1L), 1L] == channel) + start_row - 1L
  title_row <- if (length(title_rows)) max(title_rows) else sr
  pos_rows <- seq.int(title_row + 1L, cycle_rows[[1L]] - 1L)
  positions <- character()
  for (pr in pos_rows) {
    for (pc in seq_len(ncol(m))) {
      token <- m[pr, pc]
      if (grepl("^[0-9]+;[0-9]+$", token)) positions <- c(positions, token)
    }
  }
  positions <- unique(positions)
  if (!length(positions)) {
    .pre_abort("'%s': channel '%s' has Cycles / Well data but no position grid.", basename(file), channel)
  }

  declared_wells <- tryCatch(.pre_expand_wells(metadata[["Part of Plate"]]), error = function(e) {
    .pre_abort("'%s': invalid Part of Plate for '%s': %s", basename(file), channel, conditionMessage(e))
  })

  wells <- list()
  observed_wells <- character()
  for (ii in seq_along(cycle_rows)) {
    cr <- cycle_rows[[ii]]
    stop <- if (ii < length(cycle_rows)) cycle_rows[[ii + 1L]] - 1L else er - 1L
    if (cr + 1L > stop) .pre_abort("'%s' row %d: empty Cycles / Well block.", basename(file), cr)
    well <- m[cr + 1L, 1L]
    tryCatch(.pre_split_well(well), error = function(e) {
      .pre_abort("'%s' row %d: expected a well identifier after Cycles / Well.", basename(file), cr)
    })
    if (!(well %in% declared_wells)) {
      .pre_abort("'%s': well %s lies outside channel '%s' Part of Plate.", basename(file), well, channel)
    }
    if (well %in% observed_wells) .pre_abort("'%s': duplicate well block %s in '%s'.", basename(file), well, channel)
    observed_wells <- c(observed_wells, well)

    cycle_text <- m[cr + 1L, -1L, drop = TRUE]
    used <- which(nzchar(cycle_text))
    if (!length(used) || !identical(used, seq_len(max(used)))) {
      .pre_abort("'%s' row %d: cycle columns are missing/non-contiguous.", basename(file), cr + 1L)
    }
    cycles <- suppressWarnings(as.integer(cycle_text[used]))
    if (anyNA(cycles) || anyDuplicated(cycles)) {
      .pre_abort("'%s' row %d: invalid kinetic cycle labels.", basename(file), cr + 1L)
    }
    excel_cols <- used + 1L

    seg_rows <- seq.int(cr + 2L, stop)
    time_r <- seg_rows[m[seg_rows, 1L] == "Time [s]"]
    temp_r <- seg_rows[m[seg_rows, 1L] %in% c("Temp. [°C]", "Temperature [°C]")]
    mean_r <- seg_rows[m[seg_rows, 1L] == "Mean"]
    sd_r <- seg_rows[m[seg_rows, 1L] == "StDev"]
    if (length(time_r) != 1L || length(temp_r) != 1L || length(mean_r) != 1L || length(sd_r) != 1L) {
      .pre_abort("'%s' well %s: expected one Time [s], temperature, Mean and StDev row.", basename(file), well)
    }
    relative <- vapply(excel_cols, function(cc) .tecan_cell_number(m[time_r, cc], file, time_r, "cycle time", TRUE), numeric(1L))
    temperature <- vapply(excel_cols, function(cc) .tecan_cell_number(m[temp_r, cc], file, temp_r, "temperature", TRUE), numeric(1L))
    reported_mean <- vapply(excel_cols, function(cc) .tecan_cell_number(m[mean_r, cc], file, mean_r, "reported mean", TRUE), numeric(1L))
    reported_sd <- vapply(excel_cols, function(cc) .tecan_cell_number(m[sd_r, cc], file, sd_r, "reported SD", TRUE), numeric(1L))

    vals <- matrix(NA_real_, nrow = length(positions), ncol = length(cycles),
                   dimnames = list(position = positions, cycle = as.character(cycles)))
    for (pp in seq_along(positions)) {
      pr <- seg_rows[m[seg_rows, 1L] == positions[[pp]]]
      if (length(pr) != 1L) {
        .pre_abort("'%s' well %s: position row '%s' is missing or duplicated.", basename(file), well, positions[[pp]])
      }
      vals[pp, ] <- vapply(excel_cols, function(cc) {
        .tecan_cell_number(m[pr, cc], file, pr, sprintf("value at %s", positions[[pp]]), TRUE)
      }, numeric(1L))
    }

    for (cc in seq_along(cycles)) {
      calc <- .pre_mean_or_na(vals[, cc], TRUE)
      repm <- reported_mean[[cc]]
      if (!is.na(calc) && !is.na(repm)) {
        tol <- max(1e-6, abs(repm) * 1e-8)
        if (abs(calc - repm) > tol) {
          .pre_abort("'%s' well %s cycle %d: reported Mean does not match position values.",
                     basename(file), well, cycles[[cc]])
        }
      }
    }

    wells[[well]] <- list(cycles = cycles, relative_time_s = relative,
                          temperature_c = temperature, reported_mean = reported_mean,
                          reported_sd = reported_sd, values = vals)
  }

  list(
    channel = channel,
    metadata = metadata,
    positions = positions,
    declared_wells = declared_wells,
    wells = wells,
    start_time = start_time,
    end_time = end_time
  )
}

.tecan_parse_kinetic_file <- function(file, sheet, tz) {
  m <- .tecan_read_matrix(file, sheet)
  mode_rows <- which(m[, 1L] == "Mode")
  if (!length(mode_rows)) .pre_abort("'%s': no Mode rows.", basename(file))
  mode_values <- vapply(mode_rows, function(r) .tecan_row_value(m, r), character(1L))
  kinetic_rows <- mode_rows[mode_values == "Kinetic"]
  if (length(kinetic_rows) != 1L) .pre_abort("'%s': expected one global Kinetic block.", basename(file))
  channel_rows <- mode_rows[mode_values != "Kinetic"]
  if (!length(channel_rows)) .pre_abort("'%s': no kinetic channel blocks.", basename(file))

  plate <- .tecan_find_plate(m, kinetic_rows[[1L]], file)
  kinetic_end <- min(channel_rows) - 1L
  kinetic_md <- .tecan_metadata_rows(m, kinetic_rows[[1L]], kinetic_end)
  requested_cycles <- as.integer(.pre_number(kinetic_md[["Kinetic cycles"]] %||% NA_character_))
  interval_s <- .tecan_hms_seconds(kinetic_md[["Interval time [hhmmss]"]] %||% NA_character_)

  blocks <- vector("list", length(channel_rows))
  for (i in seq_along(channel_rows)) {
    end <- if (i < length(channel_rows)) channel_rows[[i + 1L]] - 1L else nrow(m)
    blocks[[i]] <- .tecan_parse_kinetic_channel(m, channel_rows[[i]], end, file, tz)
  }
  channels <- vapply(blocks, `[[`, character(1L), "channel")
  if (anyDuplicated(channels)) .pre_abort("'%s': duplicate kinetic channel names.", basename(file))
  declared_union <- unique(unlist(lapply(blocks, `[[`, "declared_wells"), use.names = FALSE))
  samples <- plate$wells[plate$wells %in% declared_union]
  outside <- setdiff(declared_union, plate$wells)
  if (length(outside)) .pre_abort("'%s': declared wells outside Plate area: %s", basename(file), paste(outside, collapse = ", "))

  positions <- unique(unlist(lapply(blocks, `[[`, "positions"), use.names = FALSE))
  all_cycles <- sort(unique(unlist(lapply(blocks, function(b) {
    unlist(lapply(b$wells, `[[`, "cycles"), use.names = FALSE)
  }), use.names = FALSE)))
  if (!length(all_cycles)) .pre_abort("'%s': no kinetic cycles were parsed.", basename(file))

  list(
    file = normalizePath(file, winslash = "/", mustWork = TRUE),
    plate = plate,
    kinetic_metadata = kinetic_md,
    requested_cycles = requested_cycles,
    interval_s = interval_s,
    blocks = blocks,
    channels = channels,
    samples = samples,
    positions = positions,
    cycles = all_cycles,
    general_info = .tecan_general_info(m, kinetic_rows[[1L]])
  )
}

.tecan_build_kinetic_experiment <- function(file, sheet, tz) {
  z <- .tecan_parse_kinetic_file(file, sheet, tz)
  samples <- z$samples
  channels <- z$channels
  cycles <- z$cycles
  positions <- z$positions
  time_ids <- sprintf("T%03d", seq_along(cycles))
  cycle_to_time <- setNames(time_ids, as.character(cycles))

  arr <- array(
    NA_real_, dim = c(length(samples), length(channels), length(cycles), length(positions)),
    dimnames = list(sample = samples, channel = channels, time = time_ids, position = positions)
  )
  rel_records <- list()
  rr <- 0L
  for (ch in seq_along(z$blocks)) {
    b <- z$blocks[[ch]]
    for (well in names(b$wells)) {
      if (!(well %in% samples)) next
      w <- b$wells[[well]]
      si <- match(well, samples)
      for (cc in seq_along(w$cycles)) {
        ti <- match(w$cycles[[cc]], cycles)
        pi <- match(rownames(w$values), positions)
        arr[si, ch, ti, pi] <- w$values[, cc]
        rr <- rr + 1L
        rel_records[[rr]] <- data.frame(
          sample_id = well, channel_id = channels[[ch]], time_id = time_ids[[ti]],
          cycle = w$cycles[[cc]], relative_time_s = w$relative_time_s[[cc]],
          temperature_c = w$temperature_c[[cc]],
          reported_mean = w$reported_mean[[cc]], reported_sd = w$reported_sd[[cc]],
          n_positions_observed = sum(!is.na(w$values[, cc])),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  rel_df <- if (length(rel_records)) do.call(rbind, rel_records) else data.frame()

  start_nums <- vapply(z$blocks, function(b) as.numeric(b$start_time), numeric(1L))
  origin <- min(start_nums)
  acq_grid <- expand.grid(
    sample_id = samples, channel_id = channels, time_id = time_ids,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  key <- paste(acq_grid$sample_id, acq_grid$channel_id, acq_grid$time_id, sep = "\r")
  rkey <- if (nrow(rel_df)) paste(rel_df$sample_id, rel_df$channel_id, rel_df$time_id, sep = "\r") else character()
  midx <- match(key, rkey)
  acq_grid$cycle <- cycles[match(acq_grid$time_id, time_ids)]
  acq_grid$relative_time_s <- if (nrow(rel_df)) rel_df$relative_time_s[midx] else NA_real_
  acq_grid$temperature_c <- if (nrow(rel_df)) rel_df$temperature_c[midx] else NA_real_
  acq_grid$reported_mean <- if (nrow(rel_df)) rel_df$reported_mean[midx] else NA_real_
  acq_grid$reported_sd <- if (nrow(rel_df)) rel_df$reported_sd[midx] else NA_real_
  acq_grid$n_positions_observed <- if (nrow(rel_df)) rel_df$n_positions_observed[midx] else NA_integer_
  channel_start <- setNames(start_nums, channels)
  channel_end <- setNames(vapply(z$blocks, function(b) as.numeric(b$end_time), numeric(1L)), channels)
  absolute_num <- channel_start[acq_grid$channel_id] + acq_grid$relative_time_s
  acq_grid$actual_time_s <- absolute_num - origin
  acq_grid$timestamp <- as.POSIXct(absolute_num, origin = "1970-01-01", tz = tz)
  acq_grid$end_timestamp <- as.POSIXct(channel_end[acq_grid$channel_id], origin = "1970-01-01", tz = tz)
  acq_grid$valid <- vapply(seq_len(nrow(acq_grid)), function(i) {
    s <- match(acq_grid$sample_id[[i]], samples)
    ch <- match(acq_grid$channel_id[[i]], channels)
    tt <- match(acq_grid$time_id[[i]], time_ids)
    any(!is.na(arr[s, ch, tt, ]))
  }, logical(1L))
  acq_grid$source_file <- basename(file)

  obs_time <- vapply(cycles, function(cyc) {
    vals <- rel_df$relative_time_s[rel_df$cycle == cyc]
    .pre_mean_or_na(vals, TRUE)
  }, numeric(1L))
  nominal <- if (!is.na(z$interval_s)) (cycles - 1L) * z$interval_s else obs_time
  time_data <- data.frame(
    time_id = time_ids, time_index = seq_along(time_ids), cycle = cycles,
    nominal_time_s = nominal, observed_time_s = obs_time,
    stringsAsFactors = FALSE
  )

  pos_bits <- strsplit(positions, ";", fixed = TRUE)
  pos_data <- data.frame(
    position_id = positions,
    position_index = seq_along(positions),
    grid_row = vapply(pos_bits, function(v) as.integer(v[[1L]]), integer(1L)),
    grid_column = vapply(pos_bits, function(v) as.integer(v[[2L]]), integer(1L)),
    position_type = "within_well_grid",
    stringsAsFactors = FALSE
  )

  PlateReaderExperiment(
    data = arr,
    sampleData = .pre_well_df(samples),
    channelData = .tecan_channel_df(z$blocks, tz),
    timeData = time_data,
    positionData = pos_data,
    acquisitionData = acq_grid,
    metadata = list(
      source = "Tecan SparkControl",
      source_format = "kinetic_multi_read",
      source_files = normalizePath(file, winslash = "/", mustWork = TRUE),
      timezone = tz,
      plate = z$plate,
      kinetic = list(
        requested_cycles = z$requested_cycles,
        requested_interval_s = z$interval_s,
        observed_cycles = z$cycles,
        unobserved_requested_cycles = if (!is.na(z$requested_cycles)) setdiff(seq_len(z$requested_cycles), z$cycles) else integer()
      ),
      experiment_info = z$general_info,
      processing_history = list()
    )
  )
}

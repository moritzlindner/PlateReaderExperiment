`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

.pre_abort <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

.pre_nonblank <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0L) NA_character_ else trimws(x[[1L]])
}

.pre_clean_name <- function(x) {
  x <- gsub("[µμ]", "u", x)
  x <- gsub("°", "", x, fixed = TRUE)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}

.pre_number <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(trimws(as.character(x)))) {
    return(NA_real_)
  }
  x <- trimws(as.character(x))
  if (toupper(x) == "INVALID") return(NA_real_)
  if (grepl(",", x, fixed = TRUE) && !grepl("\\.", x)) {
    x <- sub(",", ".", x, fixed = TRUE)
  }
  suppressWarnings(as.numeric(x))
}

.pre_parse_datetime <- function(x, tz = "UTC") {
  if (is.na(x) || !nzchar(trimws(as.character(x)))) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz))
  }
  x <- trimws(as.character(x))
  numeric_x <- suppressWarnings(as.numeric(x))
  if (!is.na(numeric_x) && numeric_x > 20000) {
    return(as.POSIXct((numeric_x - 25569) * 86400,
                      origin = "1970-01-01", tz = tz))
  }
  formats <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
    "%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M"
  )
  for (fmt in formats) {
    out <- suppressWarnings(as.POSIXct(strptime(x, fmt, tz = tz)))
    if (!is.na(out)) return(out)
  }
  as.POSIXct(NA_real_, origin = "1970-01-01", tz = tz)
}

.pre_row_to_int <- function(row) {
  z <- utf8ToInt(toupper(row)) - utf8ToInt("A") + 1L
  if (length(z) == 0L || any(z < 1L | z > 26L)) {
    .pre_abort("Invalid plate row '%s'.", row)
  }
  Reduce(function(a, b) a * 26L + b, z, init = 0L)
}

.pre_int_to_row <- function(x) {
  x <- as.integer(x)
  if (is.na(x) || x < 1L) .pre_abort("Plate row index must be positive.")
  out <- character()
  while (x > 0L) {
    rem <- (x - 1L) %% 26L
    out <- c(intToUtf8(utf8ToInt("A") + rem), out)
    x <- (x - 1L) %/% 26L
  }
  paste0(out, collapse = "")
}

.pre_split_well <- function(well) {
  hit <- regexec("^([A-Za-z]+)([0-9]+)$", trimws(well), perl = TRUE)
  z <- regmatches(trimws(well), hit)[[1L]]
  if (length(z) != 3L) .pre_abort("Invalid well identifier '%s'.", well)
  list(row = toupper(z[[2L]]), column = as.integer(z[[3L]]))
}

.pre_expand_wells <- function(expression) {
  if (is.na(expression) || !nzchar(trimws(expression))) {
    .pre_abort("Plate range expression is empty.")
  }
  tokens <- trimws(unlist(strsplit(expression, "[;,]", perl = TRUE)))
  tokens <- tokens[nzchar(tokens)]
  out <- character()
  for (token in tokens) {
    ends <- trimws(unlist(strsplit(token, "-", fixed = TRUE)))
    if (length(ends) == 1L) {
      w <- .pre_split_well(ends)
      out <- c(out, paste0(w$row, w$column))
    } else if (length(ends) == 2L) {
      a <- .pre_split_well(ends[[1L]])
      b <- .pre_split_well(ends[[2L]])
      ar <- .pre_row_to_int(a$row)
      br <- .pre_row_to_int(b$row)
      if (br < ar || b$column < a$column) {
        .pre_abort("Plate range '%s' is reversed.", token)
      }
      for (r in seq.int(ar, br)) {
        for (cc in seq.int(a$column, b$column)) {
          out <- c(out, paste0(.pre_int_to_row(r), cc))
        }
      }
    } else {
      .pre_abort("Invalid plate range token '%s'.", token)
    }
  }
  unique(out)
}

.pre_well_df <- function(wells, plate_id = "plate1") {
  bits <- lapply(wells, .pre_split_well)
  data.frame(
    sample_id = wells,
    plate_id = plate_id,
    well = wells,
    plate_row = vapply(bits, `[[`, character(1L), "row"),
    plate_row_index = vapply(bits, function(z) .pre_row_to_int(z$row), integer(1L)),
    plate_column = vapply(bits, `[[`, integer(1L), "column"),
    stringsAsFactors = FALSE
  )
}

.pre_mean_or_na <- function(x, na.rm = TRUE) {
  if (length(x) == 0L || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = na.rm)
}

.pre_history <- function(x, operation, details = list()) {
  h <- x@metadata$processing_history %||% list()
  h[[length(h) + 1L]] <- c(
    list(operation = operation, timestamp = Sys.time()),
    details
  )
  md <- x@metadata
  md$processing_history <- h
  x@metadata <- md
  x
}

.pre_resolve <- function(value, choices, label) {
  if (missing(value) || is.null(value)) return(seq_along(choices))
  if (is.logical(value)) {
    if (length(value) != length(choices)) {
      .pre_abort("Logical %s selector must have length %d.", label, length(choices))
    }
    return(which(value))
  }
  if (is.numeric(value)) {
    value <- as.integer(value)
    if (any(is.na(value) | value < 1L | value > length(choices))) {
      .pre_abort("Invalid %s index.", label)
    }
    return(value)
  }
  value <- as.character(value)
  idx <- match(value, choices)
  if (anyNA(idx)) {
    .pre_abort("Unknown %s: %s", label, paste(value[is.na(idx)], collapse = ", "))
  }
  idx
}

.pre_merge_descriptor <- function(df, descriptor, key) {
  if (nrow(descriptor) == 0L) return(df)
  extra <- setdiff(names(descriptor), names(df))
  if (length(extra) == 0L) return(df)
  descriptor <- descriptor[, c(key, extra), drop = FALSE]
  merge(df, descriptor, by = key, all.x = TRUE, sort = FALSE)
}

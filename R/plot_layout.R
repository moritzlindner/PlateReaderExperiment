#' Plot the physical plate layout
#'
#' Display the wells of a \code{PlateReaderExperiment} in their physical plate
#' arrangement. Well fill can represent a design or sample variable, and one or
#' more variables can be printed inside each measured well.
#'
#' The function uses \code{plate_row_index} and \code{plate_column} from
#' \code{sampleData(x)} when available. If those fields are absent, coordinates
#' are derived from standard well identifiers such as \code{"A1"} or
#' \code{"H12"}.
#'
#' @param x A \code{PlateReaderExperiment}.
#' @param fill_by Optional single column name from \code{sampleData(x)} or
#'   \code{design(x)} used to fill measured wells. The default is \code{"group"}.
#'   Use \code{NULL} for an uncoloured layout.
#' @param label_by Character vector of column names from \code{sampleData(x)} or
#'   \code{design(x)} printed inside measured wells. The default is
#'   \code{c("well", "group")}. Missing columns and missing values are omitted
#'   from the label.
#' @param show_empty Logical. If \code{TRUE}, show the complete inferred plate
#'   grid and leave unmeasured wells empty. If \code{FALSE}, only measured wells
#'   are drawn.
#' @param plate_format Optional plate format used when \code{show_empty = TRUE}.
#'   Supply one of \code{6}, \code{12}, \code{24}, \code{48}, \code{96},
#'   \code{384}, or \code{1536}. If \code{NULL}, the smallest standard format
#'   containing all observed well coordinates is inferred separately for each
#'   plate.
#' @param label_sep Separator placed between multiple label fields.
#' @param text_size Text size for labels inside wells.
#' @param title Optional plot title.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' \dontrun{
#' x <- read.tecan("results.xlsx")
#'
#' plot_layout(x)
#'
#' plot_layout(
#'   x,
#'   fill_by = "treatment",
#'   label_by = c("well", "biological_sample", "treatment")
#' )
#'
#' plot_layout(x, fill_by = NULL, label_by = "well", plate_format = 96)
#' }
#'
#' @export
plot_layout <- function(
    x,
    fill_by = "group",
    label_by = c("well", "group"),
    show_empty = TRUE,
    plate_format = NULL,
    label_sep = "\n",
    text_size = 3.5,
    title = NULL) {

  if (!inherits(x, "PlateReaderExperiment")) {
    stop("'x' must be a PlateReaderExperiment.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plot_layout(). ",
      "Install it with install.packages('ggplot2').",
      call. = FALSE
    )
  }

  sd <- sampleData(x)
  d <- design(x)

  if (!is.data.frame(sd) || nrow(sd) == 0L) {
    stop("sampleData(x) does not contain any samples.", call. = FALSE)
  }
  if (!("sample_id" %in% names(sd))) {
    stop("sampleData(x) must contain 'sample_id'.", call. = FALSE)
  }
  if (!("well" %in% names(sd))) {
    stop("sampleData(x) must contain 'well' to plot a plate layout.", call. = FALSE)
  }

  # Add design columns in sample order. Design values take precedence over
  # same-named non-key columns in sampleData().
  dat <- sd
  if (is.data.frame(d) && "sample_id" %in% names(d)) {
    idx <- match(dat$sample_id, d$sample_id)
    if (anyNA(idx)) {
      stop(
        "design(x) does not contain exactly one matching row for each sample.",
        call. = FALSE
      )
    }

    design_cols <- setdiff(names(d), "sample_id")
    for (nm in design_cols) {
      dat[[nm]] <- d[[nm]][idx]
    }
  }

  # Plate identifier is optional; a single implicit plate is sufficient.
  if (!("plate_id" %in% names(dat))) {
    dat$plate_id <- "Plate 1"
  }
  dat$plate_id <- as.character(dat$plate_id)
  dat$plate_id[is.na(dat$plate_id) | !nzchar(dat$plate_id)] <- "Plate 1"

  .layout_row_to_int <- function(row) {
    row <- toupper(as.character(row))
    vapply(
      row,
      function(z) {
        chars <- utf8ToInt(z) - utf8ToInt("A") + 1L
        if (length(chars) == 0L || any(chars < 1L | chars > 26L)) {
          return(NA_integer_)
        }
        Reduce(function(total, digit) total * 26L + digit, chars, init = 0L)
      },
      integer(1L)
    )
  }

  .layout_int_to_row <- function(i) {
    vapply(
      as.integer(i),
      function(z) {
        if (is.na(z) || z < 1L) {
          return(NA_character_)
        }
        out <- character()
        while (z > 0L) {
          z <- z - 1L
          out <- c(intToUtf8((z %% 26L) + utf8ToInt("A")), out)
          z <- z %/% 26L
        }
        paste0(out, collapse = "")
      },
      character(1L)
    )
  }

  .layout_parse_well <- function(well) {
    well <- toupper(trimws(as.character(well)))
    m <- regexec("^([A-Z]+)([0-9]+)$", well)
    p <- regmatches(well, m)

    row <- vapply(
      p,
      function(z) if (length(z) == 3L) z[[2L]] else NA_character_,
      character(1L)
    )
    column <- suppressWarnings(as.integer(vapply(
      p,
      function(z) if (length(z) == 3L) z[[3L]] else NA_character_,
      character(1L)
    )))

    data.frame(
      plate_row = row,
      plate_row_index = .layout_row_to_int(row),
      plate_column = column,
      stringsAsFactors = FALSE
    )
  }

  parsed <- .layout_parse_well(dat$well)

  if ("plate_row" %in% names(dat)) {
    dat$plate_row <- as.character(dat$plate_row)
  }

  if (!("plate_row" %in% names(dat))) {
    dat$plate_row <- parsed$plate_row
  }
  if (!("plate_row_index" %in% names(dat))) {
    dat$plate_row_index <- parsed$plate_row_index
  }
  if (!("plate_column" %in% names(dat))) {
    dat$plate_column <- parsed$plate_column
  }

  # Fill missing/incomplete coordinate metadata from the well identifier.
  bad_row <- is.na(dat$plate_row_index)
  bad_col <- is.na(dat$plate_column)
  bad_row_name <- is.na(dat$plate_row) | !nzchar(as.character(dat$plate_row))

  dat$plate_row_index[bad_row] <- parsed$plate_row_index[bad_row]
  dat$plate_column[bad_col] <- parsed$plate_column[bad_col]
  dat$plate_row[bad_row_name] <- parsed$plate_row[bad_row_name]

  dat$plate_row_index <- suppressWarnings(as.integer(dat$plate_row_index))
  dat$plate_column <- suppressWarnings(as.integer(dat$plate_column))

  if (anyNA(dat$plate_row_index) || anyNA(dat$plate_column) ||
      any(dat$plate_row_index < 1L) || any(dat$plate_column < 1L)) {
    bad <- dat$well[
      is.na(dat$plate_row_index) | is.na(dat$plate_column) |
        dat$plate_row_index < 1L | dat$plate_column < 1L
    ]
    stop(
      sprintf(
        "Could not determine valid plate coordinates for well(s): %s",
        paste(unique(bad), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  key <- paste(dat$plate_id, dat$plate_row_index, dat$plate_column, sep = "\r")
  if (anyDuplicated(key)) {
    stop(
      "Several samples map to the same physical well; a plate layout requires ",
      "one sample per plate/well coordinate.",
      call. = FALSE
    )
  }

  available <- names(dat)

  if (!is.null(fill_by)) {
    if (!is.character(fill_by) || length(fill_by) != 1L || is.na(fill_by) ||
        !nzchar(fill_by)) {
      stop("'fill_by' must be NULL or one non-empty column name.", call. = FALSE)
    }
    if (!(fill_by %in% available)) {
      stop(
        sprintf(
          "Unknown 'fill_by' column '%s'. Available columns: %s",
          fill_by,
          paste(available, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (is.null(label_by)) {
    label_by <- character()
  }
  if (!is.character(label_by)) {
    stop("'label_by' must be NULL or a character vector of column names.",
         call. = FALSE)
  }
  label_by <- unique(label_by[label_by %in% available])

  .layout_value_text <- function(z) {
    if (length(z) == 0L || is.na(z)) {
      return("")
    }
    z <- as.character(z)
    if (!nzchar(z)) "" else z
  }

  dat$.label <- vapply(
    seq_len(nrow(dat)),
    function(i) {
      if (length(label_by) == 0L) {
        return("")
      }
      values <- vapply(
        label_by,
        function(nm) .layout_value_text(dat[[nm]][i]),
        character(1L)
      )
      values <- values[nzchar(values)]
      paste(values, collapse = label_sep)
    },
    character(1L)
  )

  standard_formats <- data.frame(
    format = c(6L, 12L, 24L, 48L, 96L, 384L, 1536L),
    rows = c(2L, 3L, 4L, 6L, 8L, 16L, 32L),
    columns = c(3L, 4L, 6L, 8L, 12L, 24L, 48L),
    stringsAsFactors = FALSE
  )

  if (!is.null(plate_format)) {
    if (length(plate_format) != 1L || is.na(plate_format) ||
        !(as.integer(plate_format) %in% standard_formats$format)) {
      stop(
        "'plate_format' must be NULL or one of 6, 12, 24, 48, 96, 384, 1536.",
        call. = FALSE
      )
    }
    plate_format <- as.integer(plate_format)
  }

  .layout_dimensions <- function(rows_observed, cols_observed) {
    max_row <- max(rows_observed)
    max_col <- max(cols_observed)

    if (!is.null(plate_format)) {
      spec <- standard_formats[standard_formats$format == plate_format, ,
                               drop = FALSE]
      if (max_row > spec$rows || max_col > spec$columns) {
        stop(
          sprintf(
            "Observed wells do not fit inside a %s-well plate (%s x %s).",
            plate_format, spec$rows, spec$columns
          ),
          call. = FALSE
        )
      }
      return(c(rows = spec$rows[[1L]], columns = spec$columns[[1L]]))
    }

    candidates <- standard_formats[
      standard_formats$rows >= max_row &
        standard_formats$columns >= max_col, ,
      drop = FALSE
    ]

    if (nrow(candidates) == 0L) {
      return(c(rows = max_row, columns = max_col))
    }

    c(
      rows = candidates$rows[[1L]],
      columns = candidates$columns[[1L]]
    )
  }

  plate_ids <- unique(dat$plate_id)
  grids <- vector("list", length(plate_ids))

  for (i in seq_along(plate_ids)) {
    pid <- plate_ids[[i]]
    current <- dat[dat$plate_id == pid, , drop = FALSE]
    dims <- .layout_dimensions(
      current$plate_row_index,
      current$plate_column
    )

    if (isTRUE(show_empty)) {
      g <- expand.grid(
        plate_row_index = seq_len(dims[["rows"]]),
        plate_column = seq_len(dims[["columns"]]),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      g <- unique(current[, c("plate_row_index", "plate_column"), drop = FALSE])
    }

    g$plate_id <- pid
    grids[[i]] <- g
  }

  grid <- do.call(rbind, grids)
  rownames(grid) <- NULL

  dat$.measured <- TRUE

  if (!is.null(fill_by)) {
    fill_values <- dat[[fill_by]]
    if (is.factor(fill_values)) {
      fill_values <- as.character(fill_values)
    }
    dat$.fill_value <- fill_values
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = grid,
      ggplot2::aes(x = plate_column, y = plate_row_index),
      width = 0.90,
      height = 0.90,
      fill = NA,
      colour = "grey70",
      linewidth = 0.45
    )

  if (is.null(fill_by)) {
    p <- p +
      ggplot2::geom_tile(
        data = dat,
        ggplot2::aes(x = plate_column, y = plate_row_index),
        width = 0.90,
        height = 0.90,
        fill = "grey90",
        colour = "grey40",
        linewidth = 0.55
      )
  } else {
    p <- p +
      ggplot2::geom_tile(
        data = dat,
        ggplot2::aes(
          x = plate_column,
          y = plate_row_index,
          fill = .fill_value
        ),
        width = 0.90,
        height = 0.90,
        colour = "grey40",
        linewidth = 0.55
      )
  }

  if (length(label_by) > 0L) {
    p <- p +
      ggplot2::geom_text(
        data = dat,
        ggplot2::aes(
          x = plate_column,
          y = plate_row_index,
          label = .label
        ),
        size = text_size,
        lineheight = 0.9
      )
  }

  max_col <- max(grid$plate_column)
  max_row <- max(grid$plate_row_index)

  p <- p +
    ggplot2::scale_x_continuous(
      breaks = seq_len(max_col),
      position = "top",
      expand = ggplot2::expansion(add = 0.6)
    ) +
    ggplot2::scale_y_reverse(
      breaks = seq_len(max_row),
      labels = .layout_int_to_row(seq_len(max_row)),
      expand = ggplot2::expansion(add = 0.6)
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = fill_by,
      title = title
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(),
      axis.ticks = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(),
      plot.title.position = "plot"
    )

  if (length(plate_ids) > 1L) {
    p <- p + ggplot2::facet_wrap(~plate_id)
  }

  p
}

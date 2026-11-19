# PlateReaderExperiment

`PlateReaderExperiment` is an R package for importing, representing, transforming,
and plotting plate-reader experiments. It currently includes dedicated import for
Tecan SparkControl Excel exports and uses one canonical S4 representation for simple plate reads,
single-time multi-position reads, and kinetic experiments with multiple positions per well.

## Core idea

Every experiment is stored as one numeric four-dimensional assay array in the fixed
order

```text
sample × channel × time × position
```

The four dimensions are never dropped. A simple single-read-per-well experiment has
a position dimension of length 1. A single-time multi-read experiment can have several
spatial positions with one time point, while a kinetic multi-read experiment can have several
time points and several spatial positions per well. Missing or interrupted
measurements are represented by `NA`.

This allows the same downstream functions to be used regardless of which supported
Tecan export format produced the data.

## Installation

Install the package directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("moritzlindner/PlateReaderExperiment", upgrade = "never")
library(PlateReaderExperiment)
```

The installer installs the required `openxlsx` dependency. For plotting, also
install `ggplot2` with `install.packages("ggplot2")`.

To install from a local checkout while developing the package:

```r
install.packages("devtools")
devtools::document()  # regenerate NAMESPACE and help pages after editing R/ files
devtools::check()
devtools::install()
```

## 1. The `PlateReaderExperiment` object

A `PlateReaderExperiment` contains the assay and seven associated metadata
components:

```r
x
assay(x)
sampleData(x)
channelData(x)
timeData(x)
positionData(x)
design(x)
acquisitionData(x)
metadata(x)
```

### `assay(x)`

Returns the numeric four-dimensional array:

```text
sample × channel × time × position
```

Use:

```r
dim(x)
dimnames(x)
```

to inspect its dimensions and identifiers.

### `sampleData(x)`

One row per element of the sample dimension. For Tecan imports, a sample normally
corresponds to a physical well. Typical columns are:

- `sample_id`: unique identifier used by the assay dimension;
- `plate_id`: plate identifier;
- `well`: physical well, e.g. `A1`, `A2`, `B1`;
- `plate_row`: letter part of the well;
- `plate_row_index`: numeric row index;
- `plate_column`: numeric column index.

For imported plates the wells are normally ordered row-wise, e.g.
`A1, A2, A3, ..., B1, B2, ...`, but experimental design should not rely on this
ordering. Use explicit well identifiers when assigning design information.

### `channelData(x)`

One row per channel. `channel_id` identifies the channel and additional columns are
parsed from the Tecan channel metadata where available, such as excitation,
emission, bandwidth, gain, integration settings, or read mode.

The exact additional columns can vary between SparkControl methods. Inspect them
with:

```r
channelData(x)
names(channelData(x))
```

### `timeData(x)`

One row per logical time point. Typical fields are:

- `time_id`: stable identifier such as `T001`;
- `time_index`: sequential index;
- `cycle`: Tecan kinetic cycle, where applicable;
- `nominal_time_s`: requested/logical time in seconds;
- `observed_time_s`: observed time in seconds when available;
- `source_file`: source workbook for plate-matrix time series where applicable.

The logical time dimension is deliberately separated from exact acquisition
metadata because individual channels can start at different absolute times.

### `positionData(x)`

One row per within-well read position. For a single-read experiment this contains
one position only. For multi-read wells it can contain fields such as:

- `position_id`, e.g. `1;2`, `1;3`, `2;1`;
- `position_index`;
- `grid_row`;
- `grid_column`;
- `position_type`.

### `design(x)`

Experimental annotations for the sample dimension. There is exactly one design row
per `sample_id`. The design can contain arbitrary columns, for example:

```text
sample_id
biological_sample
group
genotype
treatment
dose
batch
technical_replicate
```

The default imported design contains `sample_id`, `biological_sample`, and `group`.
`group` is initially missing until assigned by the user.

### `acquisitionData(x)`

Metadata that are specific to sample × channel × time combinations. Depending on
the Tecan export this can include:

- `relative_time_s`;
- `actual_time_s`;
- `timestamp`;
- `end_timestamp`;
- `temperature_c`;
- `reported_mean`;
- `reported_sd`;
- `n_positions_observed`;
- `valid`;
- `source_file`.

For an interrupted read, missing measurements remain `NA` in the assay and
`valid` indicates whether a sample/channel/time combination contains any observed
position.

### `metadata(x)`

Free-form experiment metadata and provenance. Tecan imports record information such
as:

- source and source format;
- source files;
- time zone;
- plate metadata;
- experiment information;
- kinetic settings where applicable;
- processing history.

Processing functions such as `average_positions()`, `average_by_group()`, and
`subtract_control()` append entries to the processing history.

## 2. Creating an object manually

Most users will use `read.tecan()`, but objects can also be created directly.
`data` must always be numeric and four-dimensional.

```r
a <- array(
  rnorm(4 * 2 * 3 * 1),
  dim = c(4, 2, 3, 1),
  dimnames = list(
    sample = c("A1", "A2", "B1", "B2"),
    channel = c("GFP", "RFP"),
    time = c("T001", "T002", "T003"),
    position = "P001"
  )
)

x <- PlateReaderExperiment(a)
```

If descriptor data frames are supplied, their identifiers must exactly match the
corresponding assay dimnames and order. The S4 validity checks reject inconsistent
objects.

## 3. Importing Tecan SparkControl files

### `read.tecan()`

```r
x <- read.tecan(path, tz = "Europe/Berlin")
```

`read.tecan()` automatically detects the supported SparkControl layout and returns
the same `PlateReaderExperiment` class for all supported formats.

### Format A: plate-matrix exports

These are exports in which each channel is followed by a plate-shaped matrix of
well values. When a folder is supplied, each workbook is interpreted as a separate
time point.

```r
x <- read.tecan(
  "path/to/folder",
  tz = "Europe/Berlin",
  order_by = "start_time"
)
```

The first workbook defines the plate and channel structure. Subsequent workbooks
must match that structure; otherwise the import fails rather than silently
combining incompatible data.

For this format the assay is normalized to:

```text
well/sample × channel × workbook/time × one position
```

### Format B: single-time multi-read exports

Some SparkControl methods acquire several spatial positions within each well but
only one logical time point. In these exports, each channel contains a wide matrix
with wells in columns and within-well positions in rows. For example:

```r
x <- read.tecan(
  "24Well-singleTP-autoZ_20260818_114231.xlsx",
  tz = "Europe/Berlin"
)
```

The assay becomes:

```text
well/sample × channel × one workbook/time × position
```

A folder of compatible files in this format is treated as a sequential time
series, just like the plate-matrix format, while retaining the position dimension.

### Format C: kinetic multi-read exports

A kinetic SparkControl workbook can contain several kinetic cycles and several
spatial read positions per well:

```r
x <- read.tecan(
  "24WelleGFPTest_20260808_110332.xlsx",
  tz = "Europe/Berlin"
)
```

The assay becomes:

```text
well/sample × channel × kinetic cycle/time × position
```

If a kinetic acquisition was interrupted, observed data are retained and missing
cycles or positions are represented by `NA`.

### Import arguments

```r
read.tecan(
  path,
  sheet = "Result sheet",
  pattern = "\\.(xlsx|xlsm)$",
  recursive = FALSE,
  order_by = c("start_time", "file"),
  tz = Sys.timezone()
)
```

- `path`: workbook or folder;
- `sheet`: expected SparkControl result worksheet;
- `pattern`: files selected when `path` is a folder;
- `recursive`: search subdirectories;
- `order_by`: order sequential single-time workbooks (plate-matrix or single-time multi-read) by acquisition start time or filename;
- `tz`: time zone applied to timestamps without an explicit zone.

A folder containing multiple kinetic workbooks is intentionally rejected because
those files could represent replicate plates rather than consecutive time points.
Read such workbooks individually.

## 4. Assigning the experimental design

### `set_design()`

Use `set_design()` to add experimental conditions:

```r
layout <- data.frame(
  well = sampleData(x)$well,
  group = c(rep("Control", 12), rep("Treatment", 12)),
  biological_sample = paste0("sample_", seq_len(24))
)

x <- set_design(x, layout, by = "well")
```

**`set_design()` is key-based, not position-based.** With `by = "well"`, it
matches the value in `layout$well` to `sampleData(x)$well`. Therefore the order of
rows in `layout` does not matter.

For example, both of these identify A2 explicitly:

```r
layout <- data.frame(
  well = c("A1", "A2", "B1", "B2"),
  group = c("control", "control", "drug", "drug")
)

x <- set_design(x, layout, by = "well")
```

and the same table in a different row order gives the same assignment.

Other valid keys can be used if they occur in both `sampleData(x)` and the supplied
data frame. `sample_id` is the default.

Existing design columns with the same name are replaced; new columns are added.
Every sample must have exactly one matching design row.

### Direct replacement with `design(x) <- value`

The complete design can also be replaced:

```r
d <- design(x)
d$group <- "control"
design(x) <- d
```

The replacement must contain exactly one unique row for every `sample_id`.

## Combining experiments across plates or time

`merge_experiments()` chooses the merge axis automatically:

- If the wells, sample descriptors, and design match **and the recorded times are
  distinct**, it appends the second run as later time points. Each logical time
  point gets a new ID; `timeData(combined)$source_time_id` and
  `timeData(combined)$experiment_id` retain its origin.
- Otherwise, compatible experiments are appended as separate plates/samples.
  Here the measured wells and their experimental conditions may differ.

```r
first <- set_design(read.tecan("plate1.xlsx"), first_layout, by = "well")
second <- set_design(read.tecan("plate2.xlsx"), second_layout, by = "well")
combined <- merge_experiments(first = first, second = second)
dim(combined)             # sample × channel × time × read position
metadata(combined)$merge_axis  # "time" when joined across time
```

Use `axis = "sample"` to keep two runs as independent plates, or `axis = "time"`
to require a time merge and fail if the layout or time values do not qualify.
For a sample merge, each input's sample and plate IDs receive an experiment prefix,
so an `A1` on each plate remains distinct. Original wells, design annotations,
acquisition metadata, and run provenance are retained.

Tecan imports commonly store times relative to each run, with actual measurement
clock times in `acquisitionData(x)$timestamp`. The merge first uses complete `timeData(x)$absolute_time` values when present.
Otherwise it uses the recorded acquisition timestamps, and lastly falls back to
`observed_time_s`. It requires every time value across runs to be distinct.
When clock times are used, the merge recalculates `observed_time_s` from a shared
earliest origin. Changing only `metadata(x)$time_origin` does not move the
recorded times; edit `absolute_time` or the acquisition timestamps themselves.
You can also align an individual experiment explicitly:

```r
aligned <- offset_time_to_real(first)
timeData(aligned)[, c("nominal_time_s", "observed_time_s", "absolute_time")]

# Use the same clock origin across experiments if needed:
aligned <- offset_time_to_real(first, origin = as.POSIXct("2026-01-01 09:00:00", tz = "Europe/Berlin"))
```

`nominal_time_s` remains the requested schedule. `observed_time_s` is seconds
since the origin; `absolute_time` is a POSIXct clock time. The time merge
requires compatible channels and within-well read positions. In sample mode,
logical time schedules must also match.

## 5. Subsetting

### Array-style subsetting with `[`

Dimension order is always:

```text
sample, channel, time, position
```

For example:

```r
x2 <- x[, "eGFP-Peak-Peak", c("T001", "T002"), ]
```

Dimensions are never dropped, even when one element is selected.

Selectors can be numeric indices, identifiers, or logical vectors.

### Semantic subsetting with `subset()`

```r
x2 <- subset(
  x,
  group = "Treatment",
  channels = "eGFP-Peak-Peak",
  times = "T002"
)
```

Additional named arguments filter arbitrary design columns:

```r
x2 <- subset(
  x,
  genotype = "WT",
  treatment = "Drug",
  channels = "eGFP-Peak-Peak"
)
```

Available explicit selectors are:

```text
samples
channels
times
positions
group
```

After subsetting, all descriptor and acquisition metadata are subset consistently.

## 6. Converting to a long data frame

```r
df <- as.data.frame(x)
```

One row is produced per:

```text
sample × channel × time × position
```

The returned data frame contains `value` plus the corresponding descriptors and
design columns. By default it also contains channel metadata and acquisition
metadata.

Useful options:

```r
df <- as.data.frame(
  x,
  drop_na = TRUE,
  include_channel_metadata = FALSE,
  include_acquisition_metadata = TRUE
)
```

- `drop_na = TRUE` removes unobserved assay cells;
- `include_channel_metadata = FALSE` avoids repeating all filter metadata in every row;
- `include_acquisition_metadata = FALSE` omits sample/channel/time acquisition metadata.

The long representation is convenient for `ggplot2`, `dplyr`, statistical models,
and export to other software.

## 7. Averaging within-well positions

```r
x_mean <- average_positions(x)
```

The mean is calculated separately for every sample × channel × time combination.
The object remains four-dimensional; only the position dimension becomes length 1:

```r
dim(x_mean)
# samples × channels × times × 1

positionData(x_mean)
```

The new position identifier is `mean`. Missing values can be ignored with the
default `na.rm = TRUE`.

This is the preferred way to collapse multi-position well measurements before
analyses in which the well/sample is the experimental unit.

## 8. Averaging samples by experimental group

```r
x_group <- average_by_group(x, by = "group")
```

This averages the first assay dimension separately for every channel × time ×
position combination.

Multiple design factors can define groups:

```r
x_group <- average_by_group(
  x,
  by = c("genotype", "treatment")
)
```

Grouping variables may not contain missing values. The returned object is again a
valid `PlateReaderExperiment`; the first dimension now represents the aggregated
groups and `sampleData()` records `n_source_samples`.

A common workflow is:

```r
x_group <- x |>
  average_positions() |>
  average_by_group(by = "group")
```

## 9. Subtracting a control condition

```r
x_corrected <- subtract_control(
  x,
  control = "Control",
  by = "group"
)
```

The mean of all samples whose `design(x)[[by]]` matches `control` is calculated
**independently for every channel × time × position** and subtracted from every
sample.

Thus, for raw multi-position data, each position gets its own control baseline. If
you instead want the control subtraction to operate on the position-averaged well
signal, average positions first:

```r
x_corrected <- x |>
  average_positions() |>
  subtract_control(control = "Control", by = "group")
```

The control values used in the most recent subtraction are stored in:

```r
metadata(x_corrected)$last_control_values
```

The operation is also recorded in `metadata(x_corrected)$processing_history`.

## 10. Time-series plots

```r
plot_timeseries(
  x,
  by = "group",
  channels = c("eGFP-Peak-Peak", "eGFP-Peak-Trough"),
  position_average = TRUE,
  group_average = TRUE,
  time_unit = "minutes"
)
```

Arguments:

- `by`: design column defining groups/colour;
- `channels`: optional channel selection;
- `position_average`: average positions before plotting;
- `group_average`: average samples within `by` before plotting;
- `time_unit`: `"seconds"`, `"minutes"`, or `"hours"`.

The function prefers `observed_time_s`, then `nominal_time_s`, and finally the time
index if explicit timing is unavailable. Multiple channels are faceted with free
y-scales.

## 11. Violin plots

```r
plot_violin(
  x,
  group_by = "group",
  channels = "eGFP-Peak-Peak",
  times = "T002",
  colour_by = "biological_sample"
)
```

The violin distribution is always calculated from **one position-averaged value
per sample/well**. This avoids treating several spatial reads from one well as
independent experimental units.

To additionally display the raw positions:

```r
plot_violin(
  x,
  group_by = "group",
  channels = "eGFP-Peak-Peak",
  times = "T002",
  colour_by = "biological_sample",
  show_positions = TRUE
)
```

When `show_positions = TRUE`, the violin still uses one mean per well, while the
overlaid dots represent the raw positions. If `shape_by` is not specified,
`position_id` is used automatically.

Explicit shape coding:

```r
plot_violin(
  x,
  group_by = "group",
  colour_by = "biological_sample",
  shape_by = "position_id",
  show_positions = TRUE
)
```

Multiple channels and/or time points are faceted automatically.

## 12. Boxplots

`plot_boxplot()` follows the same experimental-unit logic as `plot_violin()`.

```r
plot_boxplot(
  x,
  group_by = "group",
  channels = "eGFP-Peak-Peak",
  times = "T002",
  colour_by = "biological_sample"
)
```

The quartiles and whiskers are calculated from position-averaged sample/well
values. Jittered sample dots are always superimposed, and the ordinary boxplot
outlier symbols are suppressed because the individual observations are already
shown.

Show raw within-well positions as additional dots with:

```r
plot_boxplot(
  x,
  group_by = "group",
  channels = "eGFP-Peak-Peak",
  times = "T002",
  colour_by = "biological_sample",
  shape_by = "position_id",
  show_positions = TRUE
)
```

Display controls include `box_alpha`, `box_width`, and `point_size`.

## 13. Recommended analysis workflows

### Simple plate-matrix time series

```r
library(PlateReaderExperiment)

x <- read.tecan("results_folder", tz = "Europe/Berlin")

layout <- data.frame(
  well = sampleData(x)$well,
  group = c(...),
  biological_sample = c(...)
)

x <- set_design(x, layout, by = "well")

plot_timeseries(x, by = "group", time_unit = "minutes")
```

### Multi-position kinetic experiment

```r
x <- read.tecan("kinetic.xlsx", tz = "Europe/Berlin")
x <- set_design(x, layout, by = "well")

# Inspect spatial variation without using positions as independent replicates
plot_boxplot(
  x,
  group_by = "group",
  times = "T002",
  colour_by = "biological_sample",
  show_positions = TRUE
)

# Well-level analysis
x_well <- average_positions(x)

# Baseline/control correction
x_corrected <- subtract_control(
  x_well,
  control = "Control",
  by = "group"
)

# Group-level time courses
plot_timeseries(
  x_corrected,
  by = "group",
  group_average = TRUE,
  time_unit = "minutes"
)
```

## 14. Missing and incomplete measurements

The package uses `NA` for unobserved assay cells. It does not insert zeros for
missing reads.

This matters for:

- channels that measure different plate regions;
- interrupted kinetic cycles;
- wells only partially read before an acquisition stopped;
- differing numbers of observed cycles between channels.

`average_positions()`, `average_by_group()`, and `subtract_control()` all provide
`na.rm` control and default to ignoring missing values when calculating means. If
all relevant values are missing, the result remains `NA`.

## 15. Object validity and invariants

A valid `PlateReaderExperiment` must satisfy all of the following:

1. `assay(x)` is numeric and exactly four-dimensional;
2. every dimension has identifiers;
3. `sampleData()`, `channelData()`, `timeData()`, and `positionData()` have exactly one row per corresponding dimension element;
4. their ID columns exactly match the array dimnames and order;
5. `design(x)` has exactly one row per sample;
6. acquisition keys refer only to valid sample, channel, and time identifiers;
7. each sample × channel × time acquisition key is unique.

The constructor and modifying methods validate these invariants so inconsistent
objects fail early.

## 16. Function reference

| Function | Purpose |
|---|---|
| `PlateReaderExperiment()` | Construct an object manually |
| `read.tecan()` | Import supported Tecan SparkControl Excel exports |
| `merge_experiments()` | Combine compatible experiments across time or plates |
| `offset_time_to_real()` | Align observed time to recorded acquisition timestamps |
| `assay()` | Return the 4-D numeric assay |
| `sampleData()` | Sample/well descriptor |
| `channelData()` | Channel/filter descriptor |
| `timeData()` | Logical time descriptor |
| `positionData()` | Within-well position descriptor |
| `design()` | Experimental design table |
| `design<-` | Replace the complete design |
| `set_design()` | Add/replace design columns using an explicit key |
| `acquisitionData()` | Sample × channel × time acquisition metadata |
| `metadata()` | Experiment and processing metadata |
| `[` | Array-like 4-D subsetting |
| `subset()` | Subset by dimensions and design variables |
| `average_positions()` | Average within-well spatial positions |
| `average_by_group()` | Average samples by design factors |
| `subtract_control()` | Subtract control mean per channel/time/position |
| `channel_ratio()` | Calculate a ratio between two channels |
| `as.data.frame()` | Convert to long format |
| `plot_timeseries()` | Group/channel time courses |
| `plot_violin()` | Violin + sample/position dots |
| `plot_boxplot()` | Boxplot + sample/position dots |
| `plot_layout()` | Display the plate layout |

Use normal R help for individual functions, for example:

```r
?PlateReaderExperiment
?read.tecan
?set_design
?average_positions
?average_by_group
?subtract_control
?plot_timeseries
?plot_violin
?plot_boxplot
```

## 17. Current scope

Version 0.1.3 supports three Tecan SparkControl export families used during
development:

1. plate-matrix exports, including folders of sequential workbooks;
2. single-time multi-read exports with wells in columns and positions in rows, including folders of sequential workbooks;
3. kinetic multi-read exports with several read positions and kinetic cycles per well.

The import layer is deliberately separated from the downstream S4 API so additional
plate-reader formats can later be mapped onto the same canonical object without
changing analysis pipelines.

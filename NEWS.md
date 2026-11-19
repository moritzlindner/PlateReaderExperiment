# PlateReaderExperiment 0.1.8

- Time merges use explicit complete `timeData()$absolute_time` values before consulting acquisition timestamps, so manually shifted clock times can be merged.

# PlateReaderExperiment 0.1.7

- `merge_experiments()` now appends time points when the plate layout and design are identical and recorded time values are distinct. `axis` can force time or sample merging.
- Added `offset_time_to_real()` to align observed time to acquisition timestamps and optionally to a supplied clock origin.

# PlateReaderExperiment 0.1.6

- Add `merge_experiments()` to combine compatible experiments with distinct plate layouts and preserved design, acquisition data, and run provenance.
- Include `plot_layout()` and `channel_ratio()` and document installation from GitHub.

# PlateReaderExperiment 0.1.5

* Tecan SparkControl cells containing `OVER` or `INVALID` are now imported as `NA_real_` with a warning instead of aborting the import.

# PlateReaderExperiment 0.1.4

- Interrupted/incomplete SparkControl single-time multi-read exports are imported best-effort instead of rejected.
- Observed values are retained; structurally missing measurements and timestamps are stored as `NA`.
- Multi-read position layouts can be inferred from SparkControl metadata when an interrupted export omits the explicit position grid.

# PlateReaderExperiment 0.1.3

- Added import support for Tecan SparkControl single-time-point multi-read exports in which wells are columns and within-well positions are rows.
- `read.tecan()` now detects this layout as `single_time_multi_read` and normalizes it to sample x channel x time x position.
- Folders of compatible single-time multi-read workbooks are supported as sequential time points while retaining the position dimension.
- Added regression tests using the supplied 24-well, four-channel, 12-position export.

# PlateReaderExperiment 0.1.2

- Added complete package documentation for the S4 object model and all exported functions.
- Added full help pages for import, accessors, design assignment, subsetting, long-format conversion, aggregation, control subtraction, and all plotting functions.
- Expanded the README into a complete user guide with recommended workflows and explicit documentation of key-based `set_design()` matching.

# PlateReaderExperiment 0.1.1

- Added `plot_boxplot()`, analogous to `plot_violin()`.
- Boxplot quartiles are calculated from position-averaged sample/well values.
- Raw within-well positions can optionally be superimposed as colour- and shape-coded dots.
- Added help-page and README documentation and plotting tests.

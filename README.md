# HR-Trim

A local R/Shiny application for marker-based heart-rate analysis of Polar CSV
exports. Load a subject's folder (or a single CSV), pick the *time* and *HR*
columns in an Excel-like grid, trim the session with start/end markers, compute
training-zone times (%HRmax or lactate-threshold models), and append the
results to a running Excel file.

The app runs **locally**: it reads folders directly from disk and writes the
results workbook back next to the data — nothing is uploaded.

## Features

- **Excel-like column picker** — click the header cell to assign `time` (blue)
  and `hr` (red); the Polar layout is auto-detected and validated
  (header must be text, first value must be numeric).
- **Recursive CSV discovery** — a chosen subject folder is searched
  recursively; files are listed with their relative path.
- **Draggable trim markers** — set start/end on an interactive Plotly chart;
  the trimmed region is highlighted.
- **Three zone models** — 5-zone %HRmax, 3-zone %HRmax, and a 3-zone lactate
  model (HR@2 mmol/L and HR@4 mmol/L). Zones are shaded in the plot and shown
  as an interactive percentage bar chart.
- **Continuous Excel export** — one row per analysed session is appended to a
  central `HRTrim_Ergebnisse.xlsx`; already-saved files are marked green.

## Requirements

- R ≥ 4.1
- R packages: `shiny`, `bslib`, `shinyjs`, `plotly`, `DT`, `shinyFiles`, `fs`,
  `openxlsx`, `jsonlite`

```r
install.packages(c(
  "shiny", "bslib", "shinyjs", "plotly", "DT",
  "shinyFiles", "fs", "openxlsx", "jsonlite"
))
```

## Running the app

- **RStudio:** open `app.R` and click *Run App*.
- **Console:** `shiny::runApp("HR Trim")`
- **Dev mode (golem):** `source("dev/run_dev.R")`

## Workflow

1. **Import** — choose a subject folder (all CSVs, incl. subfolders) or a single
   CSV, or load the bundled demo dataset.
2. **Assign columns** — click the header cell to mark `time` (blue) and `hr`
   (red). Values start in the row below the header. Auto-detection pre-fills the
   choice; a collapsible panel opens automatically if HR cannot be read.
3. **Trim** — drag the start and end markers on the HR plot.
4. **Zones** — select a model and enter HRmax or the lactate heart rates. Zone
   times are shown as a percentage bar chart with the total (post-trim) duration.
5. **Save** — appends a row to the central results workbook. If no target file
   is set, a folder prompt appears and `HRTrim_Ergebnisse.xlsx` is created or
   reused there.

## Zone models

| Model            | Boundaries |
|------------------|------------|
| 5-zone %HRmax    | L1 <75, L2 75–80, L3 80–85, L4 85–92, L5 >92 (% of HRmax) |
| 3-zone %HRmax    | <80 / 80–90 / >90 (% of HRmax) |
| 3-zone lactate   | <HR@2 mmol/L / between / >HR@4 mmol/L |

The lowest zone is open-ended, so zone times sum exactly to the total trimmed
duration.

## Export columns

```
Proband · Date · Uhrzeit · Datei · Modell · HRmax · HR_2mmol · HR_4mmol ·
TotalTime · tZ1…tZ5 · pZ1…pZ5
```

Times are in decimal minutes; `pZx` are the percentages of total time per zone.
Unused columns are left empty depending on the selected model.

## Data format

Polar CSV export: session metadata in the first rows, the time-series header
(`Time`, `HR (bpm)`) below it, then 1 Hz samples. The delimiter (`;` / `,`) is
detected automatically. `Date`/`Uhrzeit` are resolved from the Polar metadata,
then the filename (`…_YYYY-MM-DD_HH-MM-SS`), then the file modification time.

## Project structure

```
app.R                    # local entry point (sources R/ and starts the app)
R/
  app_ui.R               # navbar, theme, Info tab, CSS
  app_server.R
  mod_analyse_ui.R       # Import + Analyse tab UIs
  mod_analyse_server.R   # import, markers, zones, export
  utils_import.R         # read grid, auto-detect, parse, validate
  utils_zones.R          # zone models + time-in-zone
  utils_export.R         # append/replace Excel workbook
inst/extdata/default_demo.csv
```

## License

MIT — see [LICENSE](LICENSE).

## Author

Finn Dreisigacker — [ORCID 0009-0008-6419-0751](https://orcid.org/0009-0008-6419-0751)

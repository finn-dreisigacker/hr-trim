#' Append analysis results to a running Excel workbook
#' @noRd

export_columns <- function() {
  c("Proband", "Date", "Uhrzeit", "Datei", "Modell",
    "HRmax", "HR_2mmol", "HR_4mmol", "TotalTime",
    "tZ1", "tZ2", "tZ3", "tZ4", "tZ5",
    "pZ1", "pZ2", "pZ3", "pZ4", "pZ5")
}

.export_key <- function(df) {
  paste(df$Proband, df$Datei, df$Date, df$Uhrzeit, sep = "||")
}

# Which files (Datei) of a given Proband already have a row? (for the green marker)
export_saved_files <- function(file, proband) {
  if (!file.exists(file)) return(character(0))
  df <- tryCatch(openxlsx::read.xlsx(file), error = function(e) NULL)
  if (is.null(df) || !all(c("Proband", "Datei") %in% names(df))) return(character(0))
  unique(as.character(df$Datei[as.character(df$Proband) == proband]))
}

# Is a row with the same Proband+Datei(+Date+Uhrzeit) already present?
export_has_row <- function(file, row) {
  if (!file.exists(file)) return(FALSE)
  df <- tryCatch(openxlsx::read.xlsx(file), error = function(e) NULL)
  if (is.null(df) || !all(c("Proband", "Datei") %in% names(df))) return(FALSE)
  .export_key(row) %in% .export_key(df)
}

# Append (or replace matching) a single result row. Rewrites the whole file.
append_export <- function(file, row, mode = c("append", "replace")) {
  mode <- match.arg(mode)
  cols <- export_columns()

  newdf <- as.data.frame(row[cols], stringsAsFactors = FALSE)
  names(newdf) <- cols

  if (file.exists(file)) {
    old <- tryCatch(openxlsx::read.xlsx(file), error = function(e) NULL)
    if (!is.null(old)) {
      for (cc in cols) if (!cc %in% names(old)) old[[cc]] <- NA
      old <- old[, cols, drop = FALSE]
      if (mode == "replace") {
        old <- old[.export_key(old) != .export_key(row), , drop = FALSE]
      }
      # coerce to character-safe rbind (mixed types across reads)
      out <- rbind(
        data.frame(lapply(old,   as.character), stringsAsFactors = FALSE),
        data.frame(lapply(newdf, as.character), stringsAsFactors = FALSE)
      )
      names(out) <- cols
    } else {
      out <- newdf
    }
  } else {
    out <- newdf
  }

  # numeric columns back to numeric so Excel treats them as numbers
  num_cols <- c("HRmax", "HR_2mmol", "HR_4mmol", "TotalTime",
                paste0("tZ", 1:5), paste0("pZ", 1:5))
  for (cc in num_cols) {
    if (cc %in% names(out)) out[[cc]] <- suppressWarnings(as.numeric(out[[cc]]))
  }

  openxlsx::write.xlsx(out, file)
  invisible(out)
}

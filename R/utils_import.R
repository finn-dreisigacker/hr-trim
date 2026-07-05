#' Import helpers: read raw CSV grid, auto-detect time/hr, parse, validate
#' @noRd

# Excel-style column letters for 1-based indices (1->A, 27->AA)
col_letter <- function(n) {
  vapply(n, function(i) {
    s <- ""
    while (i > 0) {
      r <- (i - 1) %% 26
      s <- paste0(LETTERS[r + 1], s)
      i <- (i - 1) %/% 26
    }
    s
  }, character(1))
}

# Guess the field delimiter from the first few lines
detect_delim <- function(lines) {
  s <- paste(utils::head(lines, 8), collapse = "\n")
  count1 <- function(d) { m <- gregexpr(d, s, fixed = TRUE)[[1]]; sum(m > 0) }
  counts <- c(";" = count1(";"), "," = count1(","), "\t" = count1("\t"))
  if (max(counts) == 0) return(",")
  names(which.max(counts))
}

# Read a CSV into a raw character matrix (no header interpretation)
read_raw_grid <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) == 0) return(NULL)
  delim <- detect_delim(lines)
  cells <- strsplit(lines, delim, fixed = TRUE)
  maxc  <- max(lengths(cells))
  if (!is.finite(maxc) || maxc < 1) return(NULL)
  m <- t(vapply(cells, function(x) { length(x) <- maxc; x }, character(maxc)))
  m[is.na(m)] <- ""
  m <- trimws(m)
  dim(m) <- c(length(cells), maxc)
  list(m = m, delim = delim)
}

is_num_cell <- function(x) {
  x <- gsub(",", ".", x, fixed = TRUE)
  grepl("^-?\\d+(\\.\\d+)?$", x)
}

is_time_cell <- function(x) grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", x)

# Parse a vector of time strings ("HH:MM:SS" / "MM:SS") or numeric seconds -> seconds
parse_time_sec <- function(x) {
  out <- rep(NA_real_, length(x))
  ti  <- is_time_cell(x)
  if (any(ti)) {
    out[ti] <- vapply(strsplit(x[ti], ":", fixed = TRUE), function(p) {
      p <- as.numeric(p)
      if (length(p) == 3) p[1] * 3600 + p[2] * 60 + p[3]
      else if (length(p) == 2) p[1] * 60 + p[2]
      else NA_real_
    }, numeric(1))
  }
  nn <- !ti & is_num_cell(x)
  out[nn] <- as.numeric(gsub(",", ".", x[nn]))
  out
}

# Auto-detect header row + hr column (+ time column) from a raw grid
detect_mapping <- function(m) {
  nr <- nrow(m); nc <- ncol(m)
  hr_like <- function(s) grepl("hr|heart|puls|bpm", tolower(s))
  best <- list(score = 0L, header_row = NA_integer_, hr_col = NA_integer_)

  for (r in seq_len(min(nr - 1L, 15L))) {
    for (c in seq_len(nc)) {
      lbl <- m[r, c]
      if (!nzchar(lbl) || is_num_cell(lbl) || !hr_like(lbl)) next
      run <- 0L; rr <- r + 1L
      while (rr <= nr && is_num_cell(m[rr, c])) { run <- run + 1L; rr <- rr + 1L }
      if (run > best$score) best <- list(score = run, header_row = r, hr_col = c)
    }
  }

  header_row <- best$header_row
  hr_col     <- best$hr_col
  time_col   <- NA_integer_

  if (!is.na(header_row)) {
    for (c in seq_len(nc)) {
      lbl <- m[header_row, c]
      if (grepl("time|zeit", tolower(lbl)) &&
          header_row < nr && is_time_cell(m[header_row + 1L, c])) {
        time_col <- c; break
      }
    }
    if (is.na(time_col)) {
      for (c in seq_len(nc)) {
        if (header_row < nr && is_time_cell(m[header_row + 1L, c])) { time_col <- c; break }
      }
    }
  }
  if (is.na(header_row)) header_row <- 1L
  list(header_row = as.integer(header_row),
       time_col   = if (is.na(time_col)) NA_integer_ else as.integer(time_col),
       hr_col     = if (is.na(hr_col))   NA_integer_ else as.integer(hr_col))
}

# Extract the (t, hr) series given a mapping
extract_series <- function(m, header_row, time_col, hr_col) {
  nr <- nrow(m)
  if (is.na(hr_col) || header_row >= nr) return(NULL)
  rows   <- (header_row + 1L):nr
  hr     <- suppressWarnings(as.numeric(gsub(",", ".", m[rows, hr_col])))
  keep   <- !is.na(hr)
  if (!any(keep)) return(NULL)
  hr     <- hr[keep]
  rows_k <- rows[keep]

  if (!is.na(time_col)) {
    t <- parse_time_sec(m[rows_k, time_col])
    if (all(is.na(t))) {
      t <- seq_along(hr) - 1
    } else if (anyNA(t)) {
      t <- tryCatch(
        stats::approx(which(!is.na(t)), t[!is.na(t)], xout = seq_along(t), rule = 2)$y,
        error = function(e) seq_along(hr) - 1
      )
    }
    t <- t - t[1]
  } else {
    t <- seq_along(hr) - 1
  }
  ord <- order(t)
  data.frame(t = as.numeric(t)[ord], hr = as.numeric(hr)[ord])
}

# Validation flags for the mapping badges
validate_mapping <- function(m, header_row, time_col, hr_col) {
  nr <- nrow(m)
  list(
    hdr_hr_text   = !is.na(hr_col) && nzchar(m[header_row, hr_col]) && !is_num_cell(m[header_row, hr_col]),
    first_hr_num  = !is.na(hr_col) && header_row < nr && is_num_cell(m[header_row + 1L, hr_col]),
    hdr_time_text = !is.na(time_col) && nzchar(m[header_row, time_col]) && !is_num_cell(m[header_row, time_col]),
    first_time_ok = !is.na(time_col) && header_row < nr &&
      (is_time_cell(m[header_row + 1L, time_col]) || is_num_cell(m[header_row + 1L, time_col]))
  )
}

# Look up a Polar session-metadata value (label in the first rows, value below)
meta_lookup <- function(m, pattern) {
  for (r in seq_len(min(3L, nrow(m) - 1L))) {
    for (c in seq_len(ncol(m))) {
      if (grepl(pattern, tolower(m[r, c])) && nzchar(m[r + 1L, c])) return(m[r + 1L, c])
    }
  }
  NA_character_
}

# Date/time defaults: metadata -> filename -> file mtime
guess_date <- function(m, path) {
  d <- meta_lookup(m, "^date$|datum")
  if (!is.na(d) && nzchar(d)) return(d)
  fn <- regmatches(basename(path), regexpr("\\d{4}[-_.]\\d{2}[-_.]\\d{2}", basename(path)))
  if (length(fn) == 1) return(gsub("[_.]", "-", fn))
  format(file.info(path)$mtime, "%Y-%m-%d")
}

guess_time <- function(m, path) {
  tt <- meta_lookup(m, "start time|startzeit|uhrzeit")
  if (!is.na(tt) && nzchar(tt)) return(tt)
  # filename pattern: ..._YYYY-MM-DD_HH-MM-SS.csv  -> HH:MM:SS
  b <- basename(path)
  chunk <- regmatches(b, regexpr("\\d{4}[-_.]\\d{2}[-_.]\\d{2}[_-]\\d{2}-\\d{2}-\\d{2}", b))
  if (length(chunk) == 1 && nzchar(chunk)) {
    tm <- sub("^\\d{4}[-_.]\\d{2}[-_.]\\d{2}[_-]", "", chunk)
    return(gsub("-", ":", tm))
  }
  format(file.info(path)$mtime, "%H:%M")
}

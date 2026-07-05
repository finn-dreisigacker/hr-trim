#' Zone model definitions and time-in-zone computation
#' @noRd

ZONE_MODELS <- c(
  "5-Zonen %HRmax (L1-L5)" = "hrmax5",
  "3-Zonen %HRmax"         = "hrmax3",
  "3-Zonen Laktat (LT1/LT2)" = "lactate3"
)

# Returns a data.frame(name, lo, hi, color) in bpm, or NULL if inputs missing.
# Lowest zone is open downward (-Inf), highest open upward (Inf).
zone_model_defs <- function(model, hrmax = NA, hr2 = NA, hr4 = NA) {
  mk <- function(names, breaks, colors) {
    data.frame(
      name  = names,
      lo    = c(-Inf, breaks),
      hi    = c(breaks, Inf),
      color = colors,
      stringsAsFactors = FALSE
    )
  }
  ok <- function(x) length(x) == 1L && !is.na(x) && is.finite(x) && x > 0
  if (identical(model, "hrmax5")) {
    if (!ok(hrmax)) return(NULL)
    mk(c("L1", "L2", "L3", "L4", "L5"),
       hrmax * c(.75, .80, .85, .92),
       c("#2c7fb8", "#41b6c4", "#a1dab4", "#fed976", "#f03b20"))
  } else if (identical(model, "hrmax3")) {
    if (!ok(hrmax)) return(NULL)
    mk(c("Z1", "Z2", "Z3"),
       hrmax * c(.80, .90),
       c("#91cf60", "#ffffbf", "#fc8d59"))
  } else if (identical(model, "lactate3")) {
    if (!ok(hr2) || !ok(hr4) || hr4 <= hr2) return(NULL)
    mk(c("Z1", "Z2", "Z3"),
       c(hr2, hr4),
       c("#91cf60", "#ffffbf", "#fc8d59"))
  } else {
    NULL
  }
}

# time-in-zone for a trimmed series. Boundary values are lower-inclusive
# (a value on a break belongs to the higher zone). Times returned in minutes.
compute_zone_times <- function(df, zones) {
  n <- nrow(df)
  if (is.null(df) || n < 2 || is.null(zones)) return(NULL)
  dt <- diff(df$t)
  dt <- c(dt, stats::median(dt, na.rm = TRUE))
  dt[!is.finite(dt) | dt < 0] <- 0

  nz     <- nrow(zones)
  breaks <- zones$hi[-nz]
  zi     <- findInterval(df$hr, breaks) + 1L
  zi[zi < 1L]  <- 1L
  zi[zi > nz]  <- nz

  tsec <- tapply(dt, factor(zi, levels = seq_len(nz)), sum)
  tsec[is.na(tsec)] <- 0
  total <- sum(dt)

  res <- data.frame(
    zone  = zones$name,
    color = zones$color,
    t_min = as.numeric(tsec) / 60,
    pct   = if (total > 0) as.numeric(tsec) / total * 100 else rep(0, nz),
    stringsAsFactors = FALSE
  )
  attr(res, "total_min") <- total / 60
  res
}

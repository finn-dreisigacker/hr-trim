#' Analyse module server
#' @noRd

.adjust_alpha <- function(hex, a) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], a)
}

.vline <- function(x, col) {
  list(type = "line", x0 = x, x1 = x, yref = "paper", y0 = 0, y1 = 1,
       line = list(color = col, width = 2))
}

mod_analyse_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    roots <- c(Home = fs::path_home(),
               Desktop = file.path(fs::path_home(), "Desktop"),
               Volumes = "/Volumes", Root = "/")
    roots <- roots[roots == "/" | dir.exists(roots)]
    shinyFiles::shinyDirChoose(input, "prob_dir", roots = roots)
    shinyFiles::shinyFileChoose(input, "single_csv", roots = roots,
                                filetypes = c("csv", "CSV"))
    shinyFiles::shinyDirChoose(input, "target_dir", roots = roots)

    rv <- shiny::reactiveValues(
      dir = NULL, files = character(0), cur = NULL,
      maps = list(), grids = list(), saved = character(0),
      start = NULL, end = NULL, arm = "time", assign_open = FALSE,
      pending_save = FALSE, save_file = NULL
    )

    # Import landing (empty) vs. loaded view
    output$import_body <- shiny::renderUI({
      if (length(rv$files) == 0) import_landing_ui(ns) else import_loaded_ui(ns)
    })

    # path relative to the chosen root folder (unique across subfolders)
    rel_name <- function(f) {
      if (!is.null(rv$dir) && startsWith(f, paste0(rv$dir, "/")))
        sub(paste0(rv$dir, "/"), "", f, fixed = TRUE) else basename(f)
    }

    # ── helpers ───────────────────────────────────────────────────────────
    select_file <- function(path) {
      rv$cur <- path
      if (is.null(rv$grids[[path]]))
        rv$grids[[path]] <- tryCatch(read_raw_grid(path), error = function(e) NULL)
      g <- rv$grids[[path]]
      if (is.null(g)) return()
      if (is.null(rv$maps[[path]])) rv$maps[[path]] <- detect_mapping(g$m)
      shiny::updateTextInput(session, "date",    value = guess_date(g$m, path))
      shiny::updateTextInput(session, "uhrzeit", value = guess_time(g$m, path))
    }

    load_source <- function(files, proband, target) {
      rv$files <- files; rv$maps <- list(); rv$grids <- list()
      shiny::updateTextInput(session, "proband", value = proband)
      shiny::updateTextInput(session, "target",  value = target)
      rv$saved <- export_saved_files(target, proband)
      if (length(files) > 0) select_file(files[[1]]) else rv$cur <- NULL
    }

    cur_map <- shiny::reactive({ shiny::req(rv$cur); rv$maps[[rv$cur]] })
    set_map <- function(mp) { rv$maps[[rv$cur]] <- mp }
    grid_r  <- shiny::reactive({ shiny::req(rv$cur); rv$grids[[rv$cur]] })

    # ── source selection ──────────────────────────────────────────────────
    shiny::observeEvent(input$prob_dir, {
      path <- shinyFiles::parseDirPath(roots, input$prob_dir)
      if (length(path) == 0 || !nzchar(path)) return()
      rv$dir <- path
      load_source(
        files    = list.files(path, pattern = "\\.csv$", ignore.case = TRUE,
                              full.names = TRUE, recursive = TRUE),
        proband  = basename(path),
        target   = file.path(dirname(path), "HRTrim_Ergebnisse.xlsx")
      )
    }, ignoreInit = TRUE)

    # Demo-Datensatz laden (mitgelieferte CSV)
    shiny::observeEvent(input$load_demo, {
      dp <- system.file("extdata", "default_demo.csv", package = "HRTrim")
      if (!nzchar(dp) || !file.exists(dp)) {
        cand <- file.path("inst", "extdata", "default_demo.csv")
        dp <- if (file.exists(cand)) normalizePath(cand) else ""
      }
      if (!nzchar(dp)) { shiny::showNotification("Demo-Datei nicht gefunden.", type = "error"); return() }
      rv$dir <- dirname(dp)
      load_source(files = dp, proband = "Demo", target = "")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$single_csv, {
      fp <- shinyFiles::parseFilePaths(roots, input$single_csv)
      if (nrow(fp) == 0) return()
      path <- as.character(fp$datapath[[1]]); d <- dirname(path)
      rv$dir <- d
      load_source(files = path, proband = basename(d),
                  target = file.path(dirname(d), "HRTrim_Ergebnisse.xlsx"))
    }, ignoreInit = TRUE)

    # refresh green markers when target / proband changes
    shiny::observeEvent(list(input$target, input$proband), {
      shiny::req(input$target)
      rv$saved <- export_saved_files(input$target, input$proband)
    }, ignoreInit = TRUE)

    # ── file list (compact green list, in both tabs) ───────────────────────
    make_file_list <- function() {
      if (length(rv$files) == 0)
        return(shiny::div(class = "small text-muted", "Kein Ordner / keine Datei gewählt."))
      saved <- rv$saved; cur <- rv$cur
      items <- lapply(rv$files, function(f) {
        b <- rel_name(f); is_saved <- b %in% saved
        shiny::tags$div(
          class = trimws(paste("file-item",
                               if (identical(f, cur)) "active" else "",
                               if (is_saved) "saved" else "")),
          onclick = sprintf("Shiny.setInputValue('%s', %s, {priority:'event'});",
                            ns("file_click"), jsonlite::toJSON(f, auto_unbox = TRUE)),
          if (is_saved) shiny::tags$span(class = "file-check", "✓ ") else NULL,
          b
        )
      })
      shiny::div(class = "file-list", items)
    }
    output$file_list_import <- shiny::renderUI(make_file_list())
    output$file_list_main   <- shiny::renderUI(make_file_list())

    shiny::observeEvent(input$file_click, {
      if (!identical(input$file_click, rv$cur)) select_file(input$file_click)
    })

    # ── import: arm sync + click + autodetect (shared for both tables) ─────
    shiny::observeEvent(input$arm,   { if (!identical(rv$arm, input$arm))   rv$arm <- input$arm },   ignoreInit = TRUE)
    shiny::observeEvent(input$arm_a, { if (!identical(rv$arm, input$arm_a)) rv$arm <- input$arm_a }, ignoreInit = TRUE)
    shiny::observeEvent(rv$arm, {
      if (!identical(input$arm,   rv$arm)) shiny::updateRadioButtons(session, "arm",   selected = rv$arm)
      if (!identical(input$arm_a, rv$arm)) shiny::updateRadioButtons(session, "arm_a", selected = rv$arm)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$autodetect,   { g <- grid_r(); shiny::req(g); set_map(detect_mapping(g$m)) })
    shiny::observeEvent(input$autodetect_a, { g <- grid_r(); shiny::req(g); set_map(detect_mapping(g$m)) })

    assign_click <- function(info) {
      if (is.null(info) || length(info) == 0) return()
      dcol <- info$col; drow <- info$row
      if (is.null(dcol) || is.null(drow) || length(dcol) == 0 || dcol < 1) return()
      mp <- cur_map(); if (is.null(mp)) return()
      mp$header_row <- as.integer(drow)
      if (identical(rv$arm, "time")) { mp$time_col <- as.integer(dcol); rv$arm <- "hr" }
      else { mp$hr_col <- as.integer(dcol) }
      set_map(mp)
    }
    shiny::observeEvent(input$import_tbl_cell_clicked,   assign_click(input$import_tbl_cell_clicked))
    shiny::observeEvent(input$import_tbl_a_cell_clicked, assign_click(input$import_tbl_a_cell_clicked))

    build_import_dt <- function() {
      g <- grid_r(); shiny::req(g); mp <- cur_map(); shiny::req(mp)
      m <- g$m
      showr <- min(nrow(m), 30L); showc <- min(ncol(m), 26L)
      disp <- m[seq_len(showr), seq_len(showc), drop = FALSE]
      colnames(disp) <- col_letter(seq_len(showc))
      rownames(disp) <- as.character(seq_len(showr))
      hr_idx <- mp$header_row - 1L
      dt <- DT::datatable(
        disp, selection = "none", rownames = TRUE, class = "compact cell-border nowrap",
        options = list(
          dom = "t", ordering = FALSE, paging = FALSE, scrollX = TRUE, scrollY = "300px",
          rowCallback = DT::JS(sprintf(
            "function(row,data,index){if(index===%d){$('td',row).css({'font-weight':'700','background-color':'#eef3ff'});}}",
            hr_idx))
        )
      )
      if (!is.na(mp$time_col) && mp$time_col <= showc)
        dt <- DT::formatStyle(dt, columns = col_letter(mp$time_col), backgroundColor = "#cfe0ff")
      if (!is.na(mp$hr_col) && mp$hr_col <= showc)
        dt <- DT::formatStyle(dt, columns = col_letter(mp$hr_col), backgroundColor = "#ffd2d2")
      dt
    }
    output$import_tbl   <- DT::renderDT(build_import_dt(), server = FALSE)
    output$import_tbl_a <- DT::renderDT(build_import_dt(), server = FALSE)

    build_map_summary <- function() {
      g <- grid_r(); mp <- cur_map()
      if (is.null(g) || is.null(mp)) return(NULL)
      v <- validate_mapping(g$m, mp$header_row, mp$time_col, mp$hr_col)
      badge <- function(ok, txt) shiny::span(
        class = paste("badge me-1", if (isTRUE(ok)) "bg-success" else "bg-danger"),
        if (isTRUE(ok)) "✓" else "✗", " ", txt)
      hdr <- function(col) if (is.na(col)) "—" else col_letter(col)
      shiny::div(class = "map-summary small mb-2",
        shiny::span(class = "me-3", shiny::tags$b("Kopfzeile: "), sprintf("Zeile %d", mp$header_row)),
        shiny::span(class = "me-3", shiny::HTML("<span class='swatch-blue'></span> time: "), shiny::tags$b(hdr(mp$time_col))),
        shiny::span(class = "me-3", shiny::HTML("<span class='swatch-red'></span> hr: "), shiny::tags$b(hdr(mp$hr_col))),
        shiny::tags$br(),
        badge(v$hdr_hr_text, "hr-Header = Text"),
        badge(v$first_hr_num, "erster hr-Wert = Zahl"),
        badge(v$hdr_time_text, "time-Header = Text"),
        badge(v$first_time_ok, "erster time-Wert gültig")
      )
    }
    output$map_summary   <- shiny::renderUI(build_map_summary())
    output$map_summary_a <- shiny::renderUI(build_map_summary())

    # ── collapsible assign card (default closed, auto-open on HR error) ─────
    shiny::observeEvent(input$assign_toggle, { rv$assign_open <- !isTRUE(rv$assign_open) })
    shiny::observeEvent(rv$assign_open, {
      shinyjs::toggle(id = ns("assign_body"), condition = isTRUE(rv$assign_open))
    })
    output$assign_caret <- shiny::renderUI(
      shiny::icon(if (isTRUE(rv$assign_open)) "chevron-up" else "chevron-down"))
    shiny::observe({
      mp <- tryCatch(cur_map(), error = function(e) NULL)
      df <- tryCatch(series(), error = function(e) NULL)
      err <- is.null(mp) || is.na(mp$hr_col) || is.null(df) || nrow(df) < 2
      if (isTRUE(err) && !isTRUE(rv$assign_open)) rv$assign_open <- TRUE
    })

    # ── series + markers ──────────────────────────────────────────────────
    series <- shiny::reactive({
      g <- grid_r(); mp <- cur_map(); shiny::req(g, mp)
      extract_series(g$m, mp$header_row, mp$time_col, mp$hr_col)
    })

    shiny::observeEvent(series(), {
      df <- series()
      if (is.null(df) || nrow(df) < 2) { rv$start <- NULL; rv$end <- NULL; return() }
      rv$start <- min(df$t); rv$end <- max(df$t)
    }, ignoreNULL = FALSE)

    zones_r <- shiny::reactive({
      zone_model_defs(
        input$model,
        hrmax = suppressWarnings(as.numeric(input$hrmax)),
        hr2   = suppressWarnings(as.numeric(input$hr2)),
        hr4   = suppressWarnings(as.numeric(input$hr4))
      )
    })

    # model inputs persist across file switches (only re-render on model change)
    output$model_inputs <- shiny::renderUI({
      if (input$model %in% c("hrmax5", "hrmax3")) {
        shiny::numericInput(ns("hrmax"), "HRmax (bpm)", value = 190, min = 100, max = 240)
      } else {
        shiny::tagList(
          shiny::numericInput(ns("hr2"), "HR @ 2 mmol/L (bpm)", value = NA, min = 60, max = 220),
          shiny::numericInput(ns("hr4"), "HR @ 4 mmol/L (bpm)", value = NA, min = 60, max = 220)
        )
      }
    })

    trim_series <- shiny::reactive({
      df <- series(); shiny::req(df, rv$start, rv$end)
      df[df$t >= rv$start & df$t <= rv$end, , drop = FALSE]
    })
    zt <- shiny::reactive({ compute_zone_times(trim_series(), zones_r()) })

    output$plot <- plotly::renderPlotly({
      df <- series()
      if (is.null(df) || nrow(df) < 2) {
        pe <- plotly::plotly_empty(type = "scatter", mode = "lines")
        return(plotly::layout(pe, title = list(
          text = "Keine gültige Zeitreihe – bitte time/hr-Spalten zuweisen", font = list(size = 13))))
      }
      z <- zones_r()
      st <- if (is.null(rv$start)) min(df$t) else rv$start
      en <- if (is.null(rv$end))   max(df$t) else rv$end
      yr <- range(df$hr, na.rm = TRUE); pad <- diff(yr) * 0.08 + 2
      ylo <- yr[1] - pad; yhi <- yr[2] + pad; xr <- range(df$t)

      p <- plotly::plot_ly(source = "mainplot")
      if (!is.null(z)) {
        for (i in seq_len(nrow(z))) {
          lo <- max(z$lo[i], ylo); hi <- min(z$hi[i], yhi)
          if (hi <= lo) next
          p <- plotly::add_ribbons(p, x = xr, ymin = rep(lo, 2), ymax = rep(hi, 2),
                 line = list(width = 0), fillcolor = .adjust_alpha(z$color[i], 0.18),
                 hoverinfo = "skip", showlegend = FALSE)
        }
      }
      p <- plotly::add_ribbons(p, x = c(st, en), ymin = rep(ylo, 2), ymax = rep(yhi, 2),
             line = list(width = 0), fillcolor = "rgba(31,61,107,0.10)",
             hoverinfo = "skip", showlegend = FALSE)
      p <- plotly::add_lines(p, x = df$t, y = df$hr,
             line = list(color = "#b3261e", width = 1.3),
             hoverinfo = "x+y", name = "HR")
      p <- plotly::layout(p,
             xaxis = list(title = "Zeit (s)", range = xr, zeroline = FALSE),
             yaxis = list(title = "HR (bpm)", range = c(ylo, yhi)),
             shapes = list(.vline(st, "#1f3d6b"), .vline(en, "#1f3d6b")),
             showlegend = FALSE, margin = list(t = 8, r = 8))
      p <- plotly::config(p, edits = list(shapePosition = TRUE), displaylogo = FALSE,
                          modeBarButtonsToRemove = list("lasso2d", "select2d", "autoScale2d"))
      plotly::event_register(p, "plotly_relayout")
    })

    shiny::observeEvent(plotly::event_data("plotly_relayout", source = "mainplot"), {
      ed <- plotly::event_data("plotly_relayout", source = "mainplot")
      df <- shiny::isolate(series()); if (is.null(df)) return()
      clamp <- function(v) max(min(df$t), min(max(df$t), as.numeric(v)))
      s0 <- ed[["shapes[0].x0"]]; e0 <- ed[["shapes[1].x0"]]
      if (!is.null(s0)) rv$start <- clamp(s0)
      if (!is.null(e0)) rv$end   <- clamp(e0)
      if (!is.null(rv$start) && !is.null(rv$end) && rv$start > rv$end) {
        tmp <- rv$start; rv$start <- rv$end; rv$end <- tmp
      }
    })

    # ── zones: message + interactive bar chart ─────────────────────────────
    output$zone_msg <- shiny::renderUI({
      if (is.null(zones_r())) return(shiny::div(class = "alert alert-info py-2 px-3 mt-2 mb-1 small",
        "ℹ Zonenmodell-Eingaben ergänzen (HRmax bzw. HR@2/4 mmol/L), um Zonen zu berechnen."))
      if (is.null(zt())) return(shiny::div(class = "text-muted small mt-2", "Keine Daten im Trim-Bereich."))
      NULL
    })

    output$zone_hist <- plotly::renderPlotly({
      empty <- plotly::plotly_empty(type = "scatter", mode = "markers")
      z <- zones_r(); if (is.null(z)) return(empty)
      ztab <- zt(); if (is.null(ztab)) return(empty)
      total <- attr(ztab, "total_min")
      lbl <- sprintf("%.1f%%", ztab$pct)
      hov <- sprintf("%s: %.1f%% (%.2f min)", ztab$zone, ztab$pct, ztab$t_min)
      p <- plotly::plot_ly(
        x = factor(ztab$zone, levels = ztab$zone), y = ztab$pct, type = "bar",
        marker = list(color = ztab$color, line = list(color = "rgba(0,0,0,0.25)", width = 1)),
        text = lbl, textposition = "outside", cliponaxis = FALSE,
        hovertext = hov, hoverinfo = "text")
      p <- plotly::layout(p,
        title = list(text = sprintf("Zeit je Zone in %% (Total nach Trim: %.2f min)", total),
                     font = list(size = 13)),
        xaxis = list(title = ""),
        yaxis = list(title = "% der Zeit", range = c(0, 109), ticksuffix = "%"),
        margin = list(t = 36), bargap = 0.35)
      plotly::config(p, displaylogo = FALSE, displayModeBar = FALSE)
    })

    # ── export ────────────────────────────────────────────────────────────
    build_row <- function() {
      ztab <- zt(); total <- attr(ztab, "total_min")
      tmin <- rep(NA_real_, 5); pmin <- rep(NA_real_, 5)
      k <- nrow(ztab)
      tmin[seq_len(k)] <- round(ztab$t_min, 3)
      pmin[seq_len(k)] <- round(ztab$pct, 1)
      list(
        Proband = input$proband, Date = input$date, Uhrzeit = input$uhrzeit,
        Datei = rel_name(rv$cur),
        Modell = names(ZONE_MODELS)[ZONE_MODELS == input$model],
        HRmax    = if (input$model %in% c("hrmax5", "hrmax3")) as.numeric(input$hrmax) else NA_real_,
        HR_2mmol = if (input$model == "lactate3") as.numeric(input$hr2) else NA_real_,
        HR_4mmol = if (input$model == "lactate3") as.numeric(input$hr4) else NA_real_,
        TotalTime = round(total, 3),
        tZ1 = tmin[1], tZ2 = tmin[2], tZ3 = tmin[3], tZ4 = tmin[4], tZ5 = tmin[5],
        pZ1 = pmin[1], pZ2 = pmin[2], pZ3 = pmin[3], pZ4 = pmin[4], pZ5 = pmin[5]
      )
    }

    do_save <- function(file, row, mode) {
      ok <- tryCatch({ append_export(file, row, mode); TRUE },
                     error = function(e) {
                       shiny::showNotification(paste("Fehler:", conditionMessage(e)), type = "error")
                       FALSE
                     })
      if (ok) {
        shiny::showNotification(sprintf("Gespeichert (%s)", mode), type = "message", duration = 3)
        rv$saved <- export_saved_files(file, input$proband)
      }
    }

    target_ok <- function() {
      f <- input$target
      !is.null(f) && nzchar(f) && dir.exists(dirname(f))
    }

    proceed_save <- function() {
      file <- rv$save_file
      row  <- build_row()
      if (export_has_row(file, row)) {
        shiny::showModal(shiny::modalDialog(
          title = "Session existiert bereits",
          sprintf("Für '%s' / '%s' ist bereits eine Zeile vorhanden.", row$Proband, row$Datei),
          footer = shiny::tagList(
            shiny::modalButton("Abbrechen"),
            shiny::actionButton(ns("dup_append"), "Trotzdem anhängen", class = "btn-outline-secondary"),
            shiny::actionButton(ns("dup_replace"), "Ersetzen", class = "btn-danger")
          )
        ))
      } else {
        do_save(file, row, "append")
      }
    }

    shiny::observeEvent(input$save, {
      if (is.null(rv$cur)) { shiny::showNotification("Keine Datei gewählt.", type = "warning"); return() }
      if (is.null(zones_r())) { shiny::showNotification("Zonenmodell unvollständig – HRmax bzw. HR@2/4 mmol/L eingeben.", type = "warning"); return() }
      if (is.null(zt())) { shiny::showNotification("Keine Daten im Trim-Bereich.", type = "warning"); return() }
      if (target_ok()) {
        rv$save_file <- input$target
        proceed_save()
      } else {
        rv$pending_save <- TRUE
        shiny::showModal(shiny::modalDialog(
          title = "Ziel-Ordner wählen",
          shiny::p("Es ist noch keine Ergebnis-Excel gewählt. Wähle einen Ordner – dort wird ",
                   shiny::tags$code("HRTrim_Ergebnisse.xlsx"),
                   " angelegt oder (falls vorhanden) weiterverwendet."),
          shinyFiles::shinyDirButton(ns("target_dir"), "Ordner wählen", "Ordner auswählen",
            icon = shiny::icon("folder"), class = "btn-primary w-100"),
          footer = shiny::modalButton("Abbrechen")
        ))
      }
    })

    shiny::observeEvent(input$target_dir, {
      dir <- shinyFiles::parseDirPath(roots, input$target_dir)
      if (length(dir) == 0 || !nzchar(dir)) return()
      file <- file.path(dir, "HRTrim_Ergebnisse.xlsx")
      shiny::updateTextInput(session, "target", value = file)
      rv$save_file <- file
      shiny::removeModal()
      if (isTRUE(rv$pending_save)) { rv$pending_save <- FALSE; proceed_save() }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$dup_append,  { shiny::removeModal(); do_save(rv$save_file, build_row(), "append") })
    shiny::observeEvent(input$dup_replace, { shiny::removeModal(); do_save(rv$save_file, build_row(), "replace") })

    invisible(NULL)
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

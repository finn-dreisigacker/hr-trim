#' The application User-Interface
#' @import shiny
#' @noRd
app_ui <- function(request) {

  brand <- shiny::tags$a(
    class = "navbar-brand", href = "#",
    shiny::HTML('
      <svg width="30" height="30" viewBox="0 0 64 64" style="vertical-align:middle">
        <rect x="4" y="4" width="56" height="56" rx="14" fill="#0B1220"/>
        <path d="M14 36h8l4-10 6 20 6-16 4 6h8" fill="none" stroke="#E6F0FF"
              stroke-width="3.2" stroke-linecap="round" stroke-linejoin="round"/>
      </svg>
      <span style="margin-left:8px;font-weight:700;">HR-Trim</span>')
  )

  shiny::navbarPage(
    title = brand,
    windowTitle = "HR-Trim",
    id = "main_navbar",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

    header = shiny::tagList(
      shinyjs::useShinyjs(),
      shiny::tags$style(shiny::HTML("
        .import-table-wrap { overflow:auto; }
        table.dataTable td { cursor:pointer; white-space:nowrap; font-size:12px; padding:2px 6px; }
        table.dataTable td:first-child { cursor:default; color:#888; background:#f7f7f9; font-weight:600; }
        .swatch-blue,.swatch-red { display:inline-block; width:11px; height:11px; border-radius:2px;
          vertical-align:middle; margin-right:2px; }
        .swatch-blue { background:#2c6fdb; } .swatch-red { background:#d94040; }
        .map-summary { background:#f6f8fc; border:1px solid #e2e8f5; border-radius:6px; padding:6px 10px; }
        .card-header { font-weight:600; background:#1f3d6b; color:#fff; }
        .assign-header { cursor:pointer; }
        .assign-header a { color:#fff; text-decoration:none; display:block; }
        .gap-2 { gap:.5rem; }

        /* collapsible sidebar sections */
        .sidebar-section { border-top:1px solid #e2e8f5; margin-top:10px; padding-top:8px; }
        .sidebar-section > summary { cursor:pointer; font-weight:600; color:#1f3d6b;
          list-style:none; padding:2px 0; user-select:none; }
        .sidebar-section > summary::-webkit-details-marker { display:none; }
        .sidebar-section > summary::before { content:'▸'; display:inline-block; margin-right:6px;
          transition:transform .15s; }
        .sidebar-section[open] > summary::before { transform:rotate(90deg); }

        /* compact green file list */
        .file-list { border:1px solid #e2e8f5; border-radius:8px; overflow:hidden; max-height:260px; overflow-y:auto; }
        .file-item { padding:5px 10px; font-size:13px; cursor:pointer; border-bottom:1px solid #eef1f7;
          white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
        .file-item:last-child { border-bottom:0; }
        .file-item:hover { background:#eef3ff; }
        .file-item.active { box-shadow:inset 3px 0 0 #1f3d6b; font-weight:600; }
        .file-item.saved { background:#e4f6e7; }
        .file-item.saved.active { background:#d4f0d9; }
        .file-check { color:#2e7d32; font-weight:700; }

        /* Info page */
        .info-page { padding-top:8px; }
        .info-author-card { background:linear-gradient(135deg,#1f3d6b,#2c6fdb); color:#fff;
          border-radius:14px; padding:22px 26px; margin-bottom:20px; display:flex; align-items:center;
          flex-wrap:wrap; gap:16px; }
        .info-author-avatar { width:60px; height:60px; border-radius:50%; background:rgba(255,255,255,.18);
          display:flex; align-items:center; justify-content:center; font-size:22px; font-weight:700; }
        .info-author-name { font-size:20px; font-weight:700; }
        .info-author-role { opacity:.85; font-size:14px; }
        .info-contact-row { display:flex; gap:10px; flex-wrap:wrap; margin-left:auto; }
        .info-contact-item { color:#fff; background:rgba(255,255,255,.16); padding:6px 12px;
          border-radius:20px; font-size:13px; text-decoration:none; }
        .info-contact-item:hover { background:rgba(255,255,255,.30); color:#fff; }
        .info-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(280px,1fr)); gap:16px; }
        .info-card { background:#fff; border:1px solid #e3e8f2; border-radius:12px; padding:16px 20px; }
        .info-card h4 { font-size:16px; margin-bottom:12px; color:#1f3d6b; }
        .info-row { display:flex; justify-content:space-between; padding:4px 0; border-bottom:1px dashed #eef1f7; font-size:14px; }
        .info-row:last-child { border-bottom:0; }
        .info-row-key { color:#6b7280; }
        .info-row-val { font-weight:600; text-align:right; }
        .info-wf-item { display:flex; align-items:center; gap:10px; padding:5px 0; font-size:14px; }
        .info-wf-num { width:24px; height:24px; border-radius:50%; background:#2c6fdb; color:#fff;
          display:flex; align-items:center; justify-content:center; font-size:13px; font-weight:700; flex:0 0 auto; }
        .info-cite-box { background:#f6f8fc; border-left:3px solid #2c6fdb; padding:10px 14px;
          border-radius:6px; font-size:13px; font-style:italic; }

        /* upload landing (empty state) */
        .upload-page { max-width:640px; margin:24px auto; text-align:center; }
        .upload-title { font-size:22px; font-weight:700; color:#1f3d6b; margin-bottom:6px; }
        .upload-sub { color:#6b7280; margin-bottom:22px; }
        .upload-dropzone { border:2px dashed #b8c6e0; border-radius:16px; padding:34px 24px;
          background:#f8fafd; transition:border-color .15s, background .15s; }
        .upload-dropzone:hover { border-color:#2c6fdb; background:#eef3ff; }
        .upload-icon { font-size:40px; color:#2c6fdb; }
        .upload-label { font-size:17px; font-weight:600; margin-top:8px; }
        .upload-hint { color:#6b7280; font-size:13px; margin-bottom:14px; }
        .upload-actions { display:flex; gap:10px; justify-content:center; flex-wrap:wrap; }
        .upload-divider { color:#9aa4b2; font-size:13px; margin:18px 0 12px;
          text-transform:uppercase; letter-spacing:1px; }
        .demo-cards { display:flex; gap:12px; justify-content:center; }
        .demo-card { cursor:pointer; border:1px solid #e3e8f2; border-radius:12px; padding:14px 22px;
          background:#fff; transition:box-shadow .15s, transform .1s; min-width:180px; }
        .demo-card:hover { box-shadow:0 4px 14px rgba(31,61,107,.12); transform:translateY(-1px); }
        .demo-icon { font-size:22px; color:#2c6fdb; }
        .demo-name { font-weight:600; margin-top:4px; }
        .demo-desc { color:#6b7280; font-size:12px; }
      "))
    ),

    # ── Import ───────────────────────────────────────────────────────────────
    shiny::tabPanel(
      title = shiny::tagList(shiny::icon("file-import"), " Import"),
      value = "import",
      shiny::fluidPage(mod_analyse_import_ui("analyse"))
    ),

    # ── Analyse ──────────────────────────────────────────────────────────────
    shiny::tabPanel(
      title = shiny::tagList(shiny::icon("chart-line"), " Analyse"),
      value = "analyse",
      shiny::fluidPage(mod_analyse_main_ui("analyse"))
    ),

    # ── Info ─────────────────────────────────────────────────────────────────
    shiny::tabPanel(
      title = shiny::tagList(shiny::icon("circle-info"), " Info"),
      value = "info",
      shiny::fluidPage(
        class = "info-page",

        # ── Author card ──────────────────────────────────────
        shiny::div(class = "info-author-card",
          shiny::div(class = "info-author-avatar", "FD"),
          shiny::div(class = "info-author-name", "Finn Dreisigacker"),
          shiny::div(class = "info-contact-row",
            shiny::tags$a(class = "info-contact-item",
              href = "mailto:dreisigacker.finn@web.de", target = "_blank",
              shiny::icon("envelope"), "E-Mail"),
            shiny::tags$a(class = "info-contact-item",
              href = "https://github.com/finn-dreisigacker", target = "_blank",
              shiny::icon("github"), "GitHub"),
            shiny::tags$a(class = "info-contact-item",
              href = "https://orcid.org/0009-0008-6419-0751", target = "_blank",
              shiny::icon("id-card"), "ORCID"),
            shiny::tags$a(class = "info-contact-item",
              href = "https://www.dshs-koeln.de/visitenkarte/einrichtung/kreislaufforschung-und-sportmedizin/",
              target = "_blank",
              shiny::icon("building"), "DSHS Köln")
          )
        ),

        # ── Info-Karten ──────────────────────────────────────
        shiny::div(class = "info-grid",

          # Über die App
          shiny::div(class = "info-card",
            shiny::tags$h4(shiny::icon("circle-info"), " Über die App"),
            info_row("App", "HR-Trim"),
            info_row("Version", "1.0.0"),
            info_row("Zweck", "Markerbasierte HR-Analyse von Polar-Messungen (Trim + Zonen)"),
            info_row("Dateiformate", ".csv (Polar), Export .xlsx"),
            info_row("Lizenz", "MIT License"),
            info_row("Datenschutz", "Keine Online-Übertragung.")
          ),

          # Zitation
          shiny::div(class = "info-card",
            shiny::tags$h4(shiny::icon("quote-left"), " Zitation"),
            shiny::div(class = "info-cite-box",
              "Dreisigacker, F. (2026). HR-Trim: Eine Shiny-Applikation zum Analysieren von Herzfrequenz-Dateien ",
              "[R Shiny App]. ",
              shiny::tags$a(
                href = "https://github.com/finn-dreisigacker/hrtrim",
                target = "_blank",
                "https://github.com/finn-dreisigacker/hrtrim")
            ),
            shiny::tags$br(),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "Kontakt"),
              shiny::div(class = "info-row-val",
                shiny::tags$a(href = "mailto:dreisigacker.finn@web.de",
                              "dreisigacker.finn@web.de"))),
            shiny::div(class = "info-row",
              shiny::div(class = "info-row-key", "GitHub"),
              shiny::div(class = "info-row-val",
                shiny::tags$a(href = "https://github.com/finn-dreisigacker",
                              target = "_blank", "Repository")))
          )
        )
      )
    )
  )
}

#' @noRd
info_row <- function(key, val) {
  shiny::div(class = "info-row",
    shiny::div(class = "info-row-key", key),
    shiny::div(class = "info-row-val", val))
}

#' @noRd
info_wf <- function(num, txt) {
  shiny::div(class = "info-wf-item",
    shiny::div(class = "info-wf-num", num), shiny::tags$span(txt))
}

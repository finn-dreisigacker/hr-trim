#' Analyse module UI — Import tab (switches between landing + loaded view)
#' @noRd
mod_analyse_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("import_body"))
}

#' Import landing / empty-state: cleane Dropzone + Demo
#' @noRd
import_landing_ui <- function(ns) {
  shiny::div(
    class = "upload-page",
    shiny::div(class = "upload-title",
      shiny::icon("heart-pulse"), " Polar-Herzfrequenzdaten laden"),
    shiny::div(class = "upload-sub",
      "Proband-Ordner oder einzelne CSV wählen — lokal, keine Online-Übertragung."),
    shiny::div(
      class = "upload-dropzone",
      shiny::div(class = "upload-icon", shiny::icon("folder-open")),
      shiny::div(class = "upload-label", "Quelle wählen"),
      shiny::div(class = "upload-hint", "Polar-CSV (.csv) — Ordner wird rekursiv durchsucht"),
      shiny::div(
        class = "upload-actions",
        shinyFiles::shinyDirButton(ns("prob_dir"), "Proband-Ordner", "Ordner auswählen",
          icon = shiny::icon("folder"), class = "btn-primary"),
        shinyFiles::shinyFilesButton(ns("single_csv"), "Einzelne CSV", "CSV auswählen",
          multiple = FALSE, icon = shiny::icon("file-csv"), class = "btn-outline-secondary")
      )
    ),
    shiny::div(class = "upload-divider", "oder"),
    shiny::div(
      class = "demo-cards",
      shiny::div(
        class = "demo-card",
        onclick = paste0("Shiny.setInputValue('", ns("load_demo"), "', 'demo', {priority:'event'})"),
        shiny::div(class = "demo-icon", shiny::icon("flask")),
        shiny::div(class = "demo-name", "Demo-Datensatz"),
        shiny::div(class = "demo-desc", "Beispiel-Polar-CSV laden")
      )
    )
  )
}

#' Import loaded view: Quelle + gefundene CSVs + Spaltenzuweisung
#' @noRd
import_loaded_ui <- function(ns) {
  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::div(class = "d-flex gap-2 mb-2",
        shinyFiles::shinyDirButton(ns("prob_dir"), "Ordner", "Ordner auswählen",
          icon = shiny::icon("folder"), class = "btn-primary btn-sm"),
        shinyFiles::shinyFilesButton(ns("single_csv"), "CSV", "CSV auswählen",
          multiple = FALSE, icon = shiny::icon("file-csv"), class = "btn-outline-secondary btn-sm")
      ),
      shiny::tags$label(class = "control-label", "Gefundene CSV-Dateien"),
      shiny::uiOutput(ns("file_list_import"))
    ),
    shiny::column(
      width = 9,
      shiny::div(
        class = "card mb-3",
        shiny::div(class = "card-header",
          shiny::icon("table"), " Spalten zuweisen (Excel-Ansicht)"),
        shiny::div(class = "card-body",
          assign_controls_ui(ns, suffix = ""),
          shiny::div(class = "import-table-wrap", DT::DTOutput(ns("import_tbl"))))
      )
    )
  )
}

#' Analyse module UI — Analyse tab
#' @noRd
mod_analyse_main_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::textInput(ns("proband"), shiny::tagList(shiny::icon("user"), " Proband"), ""),
      shiny::tags$label(class = "control-label", "Dateien"),
      shiny::uiOutput(ns("file_list_main")),

      shiny::tags$details(
        open = NA, class = "sidebar-section",
        shiny::tags$summary(shiny::icon("layer-group"), " Zonenmodell"),
        shiny::selectInput(ns("model"), NULL, choices = ZONE_MODELS),
        shiny::uiOutput(ns("model_inputs"))
      ),
      shiny::tags$details(
        class = "sidebar-section",
        shiny::tags$summary(shiny::icon("file-export"), " Export-Ziel & Zeit"),
        shiny::textInput(ns("target"), "Ergebnis-Excel", ""),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(ns("date"), "Date", "")),
          shiny::column(6, shiny::textInput(ns("uhrzeit"), "Uhrzeit", ""))
        )
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::div(
        class = "d-flex justify-content-between align-items-center mb-2",
        shiny::tags$h5(class = "mb-0", shiny::icon("chart-line"), " Trim & Zonen"),
        shiny::actionButton(ns("save"), "In Excel speichern",
                            icon = shiny::icon("floppy-disk"), class = "btn-success")
      ),
      plotly::plotlyOutput(ns("plot"), height = "400px"),
      shiny::uiOutput(ns("zone_msg")),
      plotly::plotlyOutput(ns("zone_hist"), height = "240px"),

      shiny::div(
        class = "card mt-3 assign-collapse",
        shiny::div(
          class = "card-header assign-header",
          shiny::actionLink(ns("assign_toggle"), shiny::tagList(
            shiny::icon("table"), " Spalten zuweisen",
            shiny::uiOutput(ns("assign_caret"), inline = TRUE)))
        ),
        shinyjs::hidden(shiny::div(
          id = ns("assign_body"), class = "card-body",
          assign_controls_ui(ns, suffix = "_a"),
          shiny::div(class = "import-table-wrap", DT::DTOutput(ns("import_tbl_a")))
        ))
      )
    )
  )
}

#' Shared assign controls (arm radio + auto-detect + summary). suffix "" or "_a".
#' @noRd
assign_controls_ui <- function(ns, suffix = "") {
  shiny::tagList(
    shiny::div(
      class = "d-flex align-items-center flex-wrap gap-2 mb-2",
      shiny::tags$span(class = "me-2 fw-bold", "Kopfzelle klicken → zuweisen als:"),
      shiny::radioButtons(
        ns(paste0("arm", suffix)), NULL, inline = TRUE,
        choiceNames = list(
          shiny::HTML("<span class='swatch-blue'></span> time (blau)"),
          shiny::HTML("<span class='swatch-red'></span> hr (rot)")
        ),
        choiceValues = c("time", "hr")
      ),
      shiny::actionButton(ns(paste0("autodetect", suffix)), "Auto-erkennen",
                          icon = shiny::icon("wand-magic-sparkles"),
                          class = "btn-outline-primary btn-sm")
    ),
    shiny::uiOutput(ns(paste0("map_summary", suffix)))
  )
}

# ── HR-Trim – lokaler Start ────────────────────────────────────────────────
#
# Diese App läuft LOKAL (Dateisystem-Zugriff für Ordnerwahl + Excel-Export).
# Starten:
#   - In RStudio: dieses app.R öffnen und "Run App" klicken, ODER
#   - shiny::runApp()   (im Ordner "HR Trim")
#
# Der gewählte Ordner wird direkt gelesen und die Ergebnis-Excel wird
# fortlaufend in den übergeordneten Ordner geschrieben.
# ───────────────────────────────────────────────────────────────────────────

# app.R lädt die R/-Dateien selbst; Shiny soll sie nicht zusätzlich auto-sourcen
options(shiny.autoload.r = FALSE)

library(shiny)
library(bslib)
library(shinyjs)
library(plotly)
library(DT)
library(shinyFiles)
library(fs)
library(openxlsx)

r_files <- c(
  "R/utils_import.R",
  "R/utils_zones.R",
  "R/utils_export.R",
  "R/mod_analyse_ui.R",
  "R/mod_analyse_server.R",
  "R/app_ui.R",
  "R/app_server.R"
)
for (f in r_files) if (file.exists(f)) source(f, local = FALSE)

shinyApp(ui = app_ui, server = app_server)

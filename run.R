args <- commandArgs(T)
req <- c(
  "shiny",
  "shinyjs",
  "shinydashboard",
  "shinyWidgets",
  "RODBC",
  "data.table",
  "DT",
  "leaflet",
  "sp",
  "RColorBrewer",
  "mapview",
  "plyr",
  "dplyr",
  "plotly"
)
missing_packages <- req[!(req %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org", dependencies = T)
  if ("mapview" %in% missing_packages) {
    webshot::install_phantomjs()
  }
}
if (!is.na(args[1])) {
  shiny::runApp(file.path(dirname(args[1]), "eTBS Visualiser"), launch.browser = T)
} else {
  if (rstudioapi::isAvailable()) {
    shiny::runApp(file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), "eTBS Visualiser"), launch.browser = T)
  } else {
    stop("Please source run.R in RStudio, or use run.bat to launch the application.")
  }
}


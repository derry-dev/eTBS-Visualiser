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
  shiny_dir <- file.path(dirname(args[1]), "eTBS Visualiser")
} else {
  if (rstudioapi::isAvailable()) {
  shiny_dir <- file.path(dirname(dirname(rstudioapi::getSourceEditorContext()$path)), "eTBS Visualiser")
  } else {
	stop("Please source run.R in RStudio, or use run.bat to launch the application.")
  }
}

if (exists("shiny_dir")) {
  chrome <- paste0("\"", shiny_dir, "\\chrome\\GoogleChromePortable.exe\"")
  system(paste(chrome, "--app=http://127.0.0.1:8000/ -incognito"), wait = F)
  shiny::runApp(shiny_dir, host = "127.0.0.1", port = 8000)
}

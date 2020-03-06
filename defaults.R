db_defaults <- list(
  driver = "SQL Server",
  server = "DESKTOP-U2P5V4F",
  database = c(
    "LVNL_UTMA_Validation",
    "NavCan_UTMA_Validation_V003a",
    "NavCan_UTMA_Validation_DB3",
    "NavCan_UTMA_Validation_DB2",
    "eTBS_UTMA_Validation_V002a",
    "RDS_UTMA_Validation"
  ),
  username = "ruser",
  password = "Th!nkruser"
)

load_packages <- function(path) {
  req <- scan(path, character(), quiet = T)
  if (length(req) > 0) {
    missing_packages <- req[!(req %in% installed.packages()[,"Package"])]
    if (length(missing_packages) > 0) {
      install.packages(missing_packages, repos = "https://cloud.r-project.org", dependencies = T)
      if ("mapview" %in% missing_packages) {
        webshot::install_phantomjs()
      }
    }
  }
  suppressPackageStartupMessages(invisible(lapply(req, library, character.only = T)))
}

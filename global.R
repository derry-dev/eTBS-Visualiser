# ----------------------------------------------------------------------- #
# Libraries ---------------------------------------------------------------
# ----------------------------------------------------------------------- #

rm(list = ls())

req <- scan(file.path(getwd(), "req.txt"), character(), quiet = T)
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

source("defaults.R", local = T)

lss_types <- fread(file.path("data", "ORD Configuration.csv"))

# ----------------------------------------------------------------------- #
# General Functions -------------------------------------------------------
# ----------------------------------------------------------------------- #

'%!in%' <- function(x,y) {!('%in%'(x,y))}

# ----------------------------------------------------------------------- #
# Database Connection -----------------------------------------------------
# ----------------------------------------------------------------------- #

connection_dialogue <- function() {
  modalDialog(
    div(
      class = "centered",
      h4("Connect to Database")
    ),
    selectizeInput("db_driver", "Driver Name", c(db_defaults$driver, ""), options = list(create = T), width="100%"),
    selectizeInput("db_server", "Server Name", c(db_defaults$server, ""), options = list(create = T), width="100%"),
    selectizeInput("db_database", "Database Name", c(db_defaults$database), options = list(create = T), width="100%"),
    textInput("db_username", "Username", db_defaults$username, width="100%"),
    passwordInput("db_password", "Password", db_defaults$password, width="100%"),
    div(
      class = "centered",
      actionButton("db_connect", "Connect", icon("database")),
      uiOutput("db_status")
    ),
    size = "s",
    footer = NULL,
    easyClose = T
  )
}

get_db_connection <- function(str_driver, str_server, str_database, str_uid, str_pwd) {
  connection_string <- sprintf(
    "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
    str_driver, str_server, str_database, str_uid, str_pwd
  )
  return(odbcDriverConnect(connection=connection_string))
}

# ----------------------------------------------------------------------- #
# Debug dialogue ----------------------------------------------------------
# ----------------------------------------------------------------------- #

debug_dialogue <- function() {
  modalDialog(
    div(
      class = "centered",
      h4("session$clientData")
    ),
    verbatimTextOutput("clientdataText"),
    size = "m",
    footer = NULL,
    easyClose = T
  )
}

# ----------------------------------------------------------------------- #
# Debug dialogue ----------------------------------------------------------
# ----------------------------------------------------------------------- #

db_stats_dialogue <- function() {
  modalDialog(
    div(
      class = "centered",
      h4("Database Statistics")
    ),
    DT::dataTableOutput("db_stats_table"),
    size = "m",
    footer = NULL,
    easyClose = T
  )
}

# ----------------------------------------------------------------------- #
# ORD Calibration Functions -----------------------------------------------
# ----------------------------------------------------------------------- #

calc_landing_adjustment <- function(landing_type, headwind) {
  return(
    if (landing_type %in% c(0, 10, 11, 12)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(1, 2, 3, 4, 5)) {
      sapply(headwind / 3, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 15, 15, hw_adj)))
    } else if (landing_type %in% c(6)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 0, 0, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(7)) {
      sapply(headwind, function(hw_adj) 10)
    } else if (landing_type %in% c(8)) {
      sapply(headwind, function(hw_adj) 0)
    } else if (landing_type %in% c(9)) {
      sapply(headwind, function(hw_adj) ifelse(hw_adj > 20, 15, ifelse(hw_adj > 10, 10, 5)))
    }
  )
}

airspeed_model_break <- function(x, a, a1, b, n1, n2) {
  if (n1 < 1) n1 <- 1
  if (n2 < n1 | abs(n2 - n1) < 0.1) n2 <- n1
  return(
    if (x < 1) {
      a
    } else if (x >= 1 & x < n1) {
      a1
    } else if (x >= n1 & x <= n2 & abs(n2 - n1) < 0.1) {
      a1
    } else if (x >= n1 & x <= n2) {
      a1 + (x - n1) * (b - a1) / (n2 - n1)
    } else {
      b
    }
  )
}

airspeed_model_vector_break <- function(x, a, a1, b, n1, n2){
  sapply(x, airspeed_model_break, a = a, a1 = a1, b = b, n1 = n1, n2 = n2, simplify = T)
}

airspeed_model_break_simplified <- function(x, a, a1) {
  return(
    if (x < 1) {
      a
    } else {
      a1
    }
  )
}

airspeed_model_vector_break_simplified <- function(x, a, a1) {
  sapply(x, airspeed_model_break_simplified, a = a, a1 = a1, simplify = T)
}

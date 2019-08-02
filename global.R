# ----------------------------------------------------------------------- #
# Libraries ---------------------------------------------------------------
# ----------------------------------------------------------------------- #

rm(list = ls())

# install.packages(
#   c(
#     "shiny", "shinyjs", "shinydashboard", "shinyWidgets", "RODBC", "data.table", "DT",
#     "leaflet", "sp", "RColorBrewer", "mapview", "plyr", "dplyr", "plotly"
#   ),
#   repos = "https://cloud.r-project.org",
#   dependencies = T
# )
# webshot::install_phantomjs()

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(RODBC)
library(data.table)
library(DT)
library(leaflet)
library(sp)
library(RColorBrewer)
library(mapview)
library(plyr)
library(dplyr)
library(plotly)

source("defaults.R", local = T)

# ref_data <- fread(file.path(getwd(), "data", "01a ORD Configuration Output Type v1.3.csv"))

# ----------------------------------------------------------------------- #
# SQL Queries -------------------------------------------------------------
# ----------------------------------------------------------------------- #

query_vw_ORD_Calibration_View_Flights <- "
  SELECT DISTINCT FP_Date
      ,Leader_Callsign
      ,Leader_Aircraft_Type
      ,Leader_RECAT_Wake_Turbulence_Category
      ,Follower_Callsign
      ,Follower_Aircraft_Type
      ,Follower_RECAT_Wake_Turbulence_Category
      ,RECAT_Wake_Separation_Minimum
      ,RECAT_ROT_Spacing_Minumum
      ,Leader_4DME_Time
      ,Follower_0DME_Time
      ,Follower_0DME_RTT
      ,Follower_Threshold_Surface_Headwind
      ,Follower_Threshold_Surface_Wind_Speed
      ,Follower_Threshold_Surface_Wind_Heading
      ,Forecast_Follower_TBS_Wind_Effect
      ,Delivered_4DME_Separation
      ,Landing_Runway
  FROM vw_ORD_Calibration_View
"

# eTBS Performance Model Data Output view
query_vw_eTBS_Performance_Model <- "
  SET DATEFORMAT dmy
  SELECT * FROM vw_eTBS_Performance_Model
  ORDER BY CAST(FP_Date AS datetime), Leader_4DME_Time
"

# All Pair Reference Data
query_vw_All_Pair_Reference_Data <- "
  SET DATEFORMAT dmy
  SELECT * FROM vw_All_Pair_Reference_Data
  ORDER BY CAST(FP_Date AS datetime), Leader_4DME_Time
"
query_vw_All_Pair_Radar_Track_Point <- "
  SELECT * FROM vw_All_Pair_Radar_Track_Point
"

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
    textInput("db_driver", "Driver Name", db_defaults$driver, width="100%"),
    textInput("db_server", "Server Name", db_defaults$server, width="100%"),
    textInput("db_database", "Database Name", db_defaults$database, width="100%"),
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
# ORD Calibration Functions -----------------------------------------------
# ----------------------------------------------------------------------- #

calc_landing_adjustment <-function(landing_type, headwind){
  return(
    if (landing_type %in% c(0)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(1, 2, 3, 4, 5)) {
      sapply(headwind / 3, function(hw_adj) ifelse(hw_adj < 5, 5, ifelse(hw_adj > 15, 15, hw_adj)))
    } else if (landing_type %in% c(6, 8)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 0, 0, ifelse(hw_adj > 20, 20, hw_adj)))
    } else if (landing_type %in% c(7)) {
      sapply(headwind / 2, function(hw_adj) ifelse(hw_adj < 10, 10, ifelse(hw_adj > 20, 20, hw_adj)))
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

airspeed_model_break_2 <- function(x, a, a1) {
  return(
    if (x < 1) {
      a
    } else {
      a1
    }
  )
}

airspeed_model_vector_break_2 <- function(x, a, a1) {
  sapply(x, airspeed_model_break_2, a = a, a1 = a1, simplify = T)
}

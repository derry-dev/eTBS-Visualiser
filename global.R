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

# ----------------------------------------------------------------------- #
# SQL Queries -------------------------------------------------------------
# ----------------------------------------------------------------------- #

# con_debug <- odbcDriverConnect(connection="Driver={SQL Server};Server={DESKTOP-U2P5V4F};Database={eTBS_UTMA_Validation_V002a};Uid={vbuser};Pwd={Th!nkvbuser};")
# test <- sqlQuery(con_debug, "SELECT * FROM vw_ORD_Calibration_View
# WHERE FP_Date IN ('01/01/19') AND Follower_Callsign IN ('AAL100')") %>% as.data.table()

# Get list of all databases, schemas and tables
query_table_list <- "
  SET NOCOUNT ON
  DECLARE @AllTables table (\"Database\" nvarchar(4000), \"Schema\" nvarchar(4000), \"Table\" nvarchar(4000))
  INSERT INTO @AllTables (\"Database\", \"Schema\", \"Table\")
  EXEC sp_msforeachdb 'select \"?\", s.name, t.name from [?].sys.tables t inner join sys.schemas s on t.schema_id=s.schema_id'
  SET NOCOUNT OFF
  SELECT * FROM @AllTables ORDER BY 1
"

# vw_ORD_Validation_View: This view generates the ORD Validation View output defined in UTMA_Validation_Tool_Requirements document
query_vw_ORD_Validation_View <- "
  SELECT * FROM vw_ORD_Validation_View
"

# vw_ORD_Calibration_View:  This view generates the ORD Calibration View output defined in UTMA_Validation_Tool_Requirements document
query_vw_ORD_Calibration_View <- "
  SELECT * FROM vw_ORD_Calibration_View
"

query_vw_ORD_Calibration_View_Date <- "
  SELECT DISTINCT FP_Date AS Date FROM vw_ORD_Calibration_View
"

query_vw_ORD_Calibration_View_Callsigns <- "
  SELECT DISTINCT FP_Date, Follower_Callsign AS Callsign FROM vw_ORD_Calibration_View
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

# Flight Plan (PLT MAP)
query_flightplan <- "
  SELECT * FROM tbl_Flight_Plan
  LEFT JOIN (
    SELECT Flight_Plan_ID AS Flight_Plan_ID_2, Time_At_4DME, Time_At_1DME FROM tbl_Flight_Plan_Derived
  ) AS t ON Flight_Plan_ID = Flight_Plan_ID_2
"

# Tracks (PLT MAP)
query_tracks <- "
  SELECT * FROM tbl_Radar_Track_Point
  LEFT JOIN (
    SELECT Radar_Track_Point_ID AS Radar_Track_Point_ID_2, Corrected_Mode_C, Range_To_Threshold, Range_To_ILS, Path_Leg
    FROM tbl_Radar_Track_Point_Derived
  ) AS t ON Radar_Track_Point_ID = Radar_Track_Point_ID_2
"

# Volumes (PLT MAP)
query_volumes <- "
  SELECT * FROM tbl_Polygon
  LEFT JOIN (
    SELECT Volume_Name AS Volume_Name_2, Min_Altitude, Max_Altitude FROM tbl_Volume
  ) AS t ON Volume_Name = Volume_Name_2
"

# Legs (PLT MAP)
query_legs <- "
  SELECT * FROM tbl_Path_Leg
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
    if (x < 1){
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

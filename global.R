rm(list = ls())

# install.packages(
#   c(
#     "shiny", "shinydashboard", "shinyWidgets", "RODBC", "data.table",
#     "DT", "leaflet", "sp", "RColorBrewer", "mapview", "magrittr", "plotly"
#   ),
#   repos = "https://cloud.r-project.org",
#   dependencies = T
# )
# webshot::install_phantomjs()

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(RODBC)
library(data.table)
library(DT)
library(leaflet)
library(sp)
library(RColorBrewer)
library(mapview)
library(magrittr)
library(plotly)

# ----------------------------------------------------------------------- #
# Functions ---------------------------------------------------------------
# ----------------------------------------------------------------------- #

'%!in%' <- function(x,y){!('%in%'(x,y))}

get_db_connection <- function(str_driver, str_server, str_database, str_uid, str_pwd) {
  connection_string <- sprintf(
    "Driver={%s};Server={%s};Database={%s};Uid={%s};Pwd={%s};",
    str_driver, str_server, str_database, str_uid, str_pwd
  )
  return(odbcDriverConnect(connection=connection_string))
}

# ----------------------------------------------------------------------- #
# SQL Queries -------------------------------------------------------------
# ----------------------------------------------------------------------- #

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
  SET DATEFORMAT dmy
  SELECT * FROM vw_ORD_Validation_View
  ORDER BY CAST(FP_Date AS datetime), Prediction_Time
"

# vw_ORD_Calibration_View:  This view generates the ORD Calibration View output defined in UTMA_Validation_Tool_Requirements document
query_vw_ORD_Calibration_View <- "
  SET DATEFORMAT dmy
  SELECT * FROM vw_ORD_Calibration_View
  ORDER BY CAST(FP_Date AS datetime), Track_Time
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

# Flight Plan
query_flightplan <- "
  SELECT * FROM tbl_Flight_Plan
  LEFT JOIN (
    SELECT Flight_Plan_ID AS Flight_Plan_ID_2, Time_At_4DME, Time_At_1DME FROM tbl_Flight_Plan_Derived
  ) AS t ON Flight_Plan_ID = Flight_Plan_ID_2
"

# Tracks
query_tracks <- "
  SELECT * FROM tbl_Radar_Track_Point
  LEFT JOIN (
    SELECT Radar_Track_Point_ID AS Radar_Track_Point_ID_2, Corrected_Mode_C, Range_To_Threshold, Range_To_ILS, Path_Leg
    FROM tbl_Radar_Track_Point_Derived
  ) AS t ON Radar_Track_Point_ID = Radar_Track_Point_ID_2
"

# Volumes
query_volumes <- "
  SELECT * FROM tbl_Polygon
  LEFT JOIN (
    SELECT Volume_Name AS Volume_Name_2, Min_Altitude, Max_Altitude FROM tbl_Volume
  ) AS t ON Volume_Name = Volume_Name_2
"

# Legs
query_legs <- "
  SELECT * FROM tbl_Path_Leg
"

# ----------------------------------------------------------------------- #
# DEBUG -------------------------------------------------------------------
# ----------------------------------------------------------------------- #

# con_debug <- odbcDriverConnect(connection="Driver={SQL Server};Server={DESKTOP-U2P5V4F};Database={};Uid={vbuser};Pwd={Th!nkvbuser};")
# test <- sqlQuery(con_debug, )

library(RODBC)
library(here)
library(data.table)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sp)
library(RColorBrewer)

'%!in%' <- function(x,y){!('%in%'(x,y))}

data_path <- paste0(here(), "/data/")
data_files <- list.files(data_path, pattern = "*.csv")
dat <- list()
for (x in data_files) {
  dat[[gsub(".csv", "", x)]] <- fread(paste0(data_path,x), encoding = "UTF-8")
}
names(dat) <- gsub(".csv", "", data_files)

title <- "eTBS Visualiser"
plt_date_choices <- dat$tracks$Track_Date %>% unique() %>% as.vector()
plt_rwy_choices <- dat$flightplan$Landing_Runway %>% unique() %>% as.vector()
plt_leg_choices <- c(dat$legs$Path_Leg_Name, "NA") %>% unique() %>% as.vector()
plt_vol_choices <- dat$volumes$Volume_Name %>% unique() %>% as.vector()

get_db_connection <- function(driver, server, database) {
  connection_string <- paste0("Driver={",driver,"};server=",server,";database=",database,";trusted_connection=yes;")
  con <- odbcDriverConnect(connection=connection_string)
  return(con)
}

import_all <- function(db_connection) {
  # Import from database ----------------------------------------------------
  con <- db_connection
  
  # For troubleshooting purposes
  # con <- odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};server=DESKTOP-F25RUHL;database=UTMA_Validation;trusted_connection=yes;")
  
  dat <- list()
  
  dat$flightplan <- "
  SELECT
    t1.Flight_Plan_ID,
    FP_Date,
    FP_Time,
    Callsign,
    SSR_Code,
    Aircraft_Type,
    Wake_Vortex,
    Origin,
    Destination,
    SID,
    Landing_Runway,
    Departure_Runway,
    Departure_Roll_Time,
    Departure_Liftoff_Time,
    DROT,
    Time_At_4DME,
    Time_At_1DME
  FROM tbl_Flight_Plan AS t1
  LEFT JOIN (
    SELECT
      Flight_Plan_ID,
      Time_At_4DME,
      Time_At_1DME
    FROM tbl_Flight_Plan_Derived
  ) AS t2 ON t1.Flight_Plan_ID = t2.Flight_Plan_ID
  " %>% sqlQuery(con,.) %>% as.data.table()
  
  dat$volumes <- "
  SELECT
    t1.Volume_Name,
    Volume_Type,
    Runway_Name,
    Point_Sequence,
    Latitude,
    Longitude,
    Min_Altitude,
    Max_Altitude
  FROM tbl_Polygon AS t1
  LEFT JOIN (
    SELECT
      Volume_Name,
      Runway_Name,
      Volume_Type,
      Min_Altitude,
      Max_Altitude
    FROM tbl_Volume
  ) AS t2 ON t1.Volume_Name = t2.Volume_Name
  " %>% sqlQuery(con,.) %>% as.data.table()
  
  dat$legs <- "SELECT * FROM tbl_Path_Leg" %>% sqlQuery(con,.) %>% as.data.table()
  
  dat$tracks <- "
  SELECT
    t1.Radar_Track_Point_ID,
    Flight_Plan_ID,
    Track_Date,
    Track_Time,
    Callsign,
    SSR_Code,
    Lat,
    Lon,
    Mode_C,
    Corrected_Mode_C,
    Range_To_Threshold,
    Range_To_ILS,
    Path_Leg
  FROM tbl_Radar_Track_Point AS t1
  LEFT JOIN (
    SELECT
      Radar_Track_Point_ID,
      Corrected_Mode_C,
      Range_To_Threshold,
      Range_To_ILS,
      Path_Leg
    FROM tbl_Radar_Track_Point_Derived
  ) AS t2 ON t1.Radar_Track_Point_ID = t2.Radar_Track_Point_ID
  " %>% sqlQuery(con,.) %>% as.data.table()
  
  # Get time of day column from seconds after midnight
  dat$tracks$Track_Time_New <- format(as.POSIXct('1900-1-1')+dat$tracks$Track_Time, "%H:%M:%S")
  
  # Export to CSV -----------------------------------------------------------
  setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path)))
  
  for (x in names(dat)) {
    con <- file(paste0(getwd(),"/data/",x,".csv"), encoding="UTF-8")
    write.csv(dat[[x]], file=con, row.names=F)
  }
  
  closeAllConnections()
  
}
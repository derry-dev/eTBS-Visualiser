library(RODBC)
library(magrittr)
library(data.table)

# Import from database ----------------------------------------------------
con <- odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};server=DESKTOP-F25RUHL;database=UTMA_Validation;trusted_connection=yes;")

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
    Point_X,
    Point_Y,
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

odbcClose(con)

# Get time of day
dat$tracks$Track_Time_New <- format(as.POSIXct('1900-1-1')+dat$tracks$Track_Time, "%H:%M:%S")

# Export to CSV -----------------------------------------------------------
setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path)))

for (x in names(dat)) {
  con <- file(paste0(getwd(),"/data/",x,".csv"), encoding="UTF-8")
  write.csv(dat[[x]], file=con, row.names=F)
}

closeAllConnections()

library(here)
library(data.table)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
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
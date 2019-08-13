# eTBS Visualiser

## User Guide

### Running the Visualiser

In order to use the eTBS visualiser on your local PC, you must first [install R](https://cloud.r-project.org/).

#### Option 1: Batch Script

In the eTBS visualiser folder, double click the run.bat file. This script will start the visualiser after locating the R executable and installing the missing packages required.

#### Option 2: R console session

Open the R console and run the command below to install the required R packages for the visualiser to function correctly:

```r
install.packages(
  c(
    "shiny", "shinyjs", "shinydashboard", "shinyWidgets", "RODBC", "data.table", "DT",
    "leaflet", "sp", "RColorBrewer", "mapview", "plyr", "dplyr", "plotly"
  ),
  repos = "https://cloud.r-project.org",
  dependencies = T
)
webshot::install_phantomjs()
```
Then in the R console, run the following commands:

```r
# Replace the file path with the directory containing the eTBS Visualiser folder
setwd("C:/Users/USER NAME/Documents/")

shiny::runApp("eTBS Visualiser", launch.browser = TRUE)
```

#### Option 3: RStudio

Make sure [RStudio](https://www.rstudio.com/products/rstudio/download/#download) is installed, then open any one of the files **global.R**, **ui.R** or **server.R**, install the required R packages listed in **Option 2** and click the run icon on the top right-hand side of the source window.

#### Notes

It is recommended to have an up-to-date web browser such as Google Chrome, Mozilla Firefox set as the default web browser on your system, full support of functionalities are not guaranteed on other browsers such as Opera, Apple Safari, Microsoft Edge, Internet Explorer and you may encounter issues such as missing UI elements if you attempt to use the visualiser.

### Editing the Visualiser

If you encounter any issues while using the visualiser or would like to contribute to the addition of new features, RStudio is recommended for development of the visualiser. Please see the developer if you require a version controlled copy of the visualiser project and create a pull request if you wish to merge your commits with the master branch.

---

## Version Notes

13/08/2019

* Fixed incorrect callsign for FP ID on PLT map
* Changed PLT map centering to use adaptation data table

09/08/2019

* Added batch file for easy launching of visualiser
* Added animation to PLT map

05/08/2019

* Fixed non-default starting ORD tabs not working correctly

31/07/2019

* Reworked ORD Tab Input Logic + Bug Fixes
* Added Groundspeed Data to IAS Profile

30/07/2019

* Added Flight Filter for ORD Calibration by AC Type
* Added ORD Calibration Run Buttons

29/07/2019

* Added ORD Calibration by AC Type

26/07/2019

* Enabled ORD Calibration View NLS Model Plot
* Added simplified models on NLS model failure
* Server.R commenting
* Minor layout fixes

18/07/2019

* Fixed PLT screenshot not working
* Fixed PLT screenshot map tile issue
* Fixed PLT screenshot location/zoom issue
* Added PLT map leg legend

16/07/2019

* Added PLT marker & polygon customisation
* Restored additional PLT map tiles
* Known issue: PLT screenshot tool not working

10/07/2019

* Added ORD Calibration View NLS modelling
* PLT Map and General Bug Fixes
* New loading animation

09/07/2019

* Added ORD functions
* Fixed volumes not displaying in PLT map
* Enabled PLT map track filtering by path leg
* Enabled real-time updating of PLT map based on selection
* Changed default PLT map tile to CartoDB.DarkMatter
* Fixed PLT map markers/volumes not appearing in screenshot

25/06/2019

* Complete Rework
* Data is no longer saved locally (queried directly from database)
* Added Database Dashboard with Query Tool
* Adapted PLT Map to new eTBS data
* Improved CSS

14/02/2019

* Incorporated database import script into dashboard

13/02/2019

* Imported database files to local csv
* Added working map for PLT analysis track plotting
* Corrected map and dashboard layout problems

12/02/2019

* Added leaflet map to PLT analysis
* Set up initial dashboard template
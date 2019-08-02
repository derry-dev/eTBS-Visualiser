# eTBS Visualiser

## User Guide

### Running the Visualiser

In order to use the eTBS visualiser on your local PC, you must first [install R](https://cloud.r-project.org/).

Once R is installed, open the R console and run the command below to install the required R packages for the visualiser to function correctly:

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

After the packages are installed, there are two ways to launch the visualiser:

1. [Install RStudio](https://www.rstudio.com/products/rstudio/download/#download), open any of the files **global.R**, **ui.R** or **server.R** in RStudio, install the required R packages and click the run icon on the top right-hand side of the source window.

2. In the R console, run the following commands:

```r
# Replace the file path with the directory containing the eTBS Visualiser folder
setwd("C:/Users/USER NAME/Documents/")

shiny::runApp("eTBS Visualiser", launch.browser = TRUE)
```

It is recommended to use a modern web browser such as Google Chrome, Mozilla Firefox or Opera, you may encounter issues such as missing UI elements if you attempt to use the visualiser using older browsers such as Microsoft Edge or Internet Explorer.

### Editing the Visualiser

If you encounter any issues while using the visualiser or would like to contribute to the addition of new features, RStudio is recommended for development of the visualiser. Please see the developer if you require a version controlled copy of the visualiser project and create a pull request if you wish to merge your commits with the master branch.

---

## Version Notes

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
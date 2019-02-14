# eTBS Visualiser

## User Guide

### Running the Visualiser

In order to use the eTBS visualiser on your local PC, you must first [install R](https://cloud.r-project.org/).

Once R is installed, open the R console and run the command below to install the required R packages for the visualiser to function correctly:

```r
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "leaflet", "RColorBrewer", "data.table", "here", "magrittr", "RODBC"))
```

After the packages are installed, run the following commands to launch the visualiser:

```r
# Replace the file path with the directory containing the eTBS Visualiser folder
setwd("C:/Users/USER NAME/Documents/")

shiny::runApp("eTBS Visualiser", launch.browser = TRUE)
```

Alternatively, you can [install RStudio](https://www.rstudio.com/products/rstudio/download/#download), open any of the files **global.R**, **ui.R** or **server.R** in RStudio, install the required R packages and click the run icon on the top right-hand side of the source window.

### Editing the Visualiser

RStudio is recommended for development of the visualiser. It is not recommended to move any files within the **eTBS Visualiser** folder as this will break functionality.

---

## Version Notes

14/02/2019

* Incorporated database import script into dashboard

13/02/2019

* Imported database files to local csv
* Added working map for PLT analysis track plotting
* Corrected map and dashboard layout problems

12/02/2019

* Added leaflet map to PLT analysis
* Set up initial dashboard template
# eTBS Visualiser

## 1. User Guide

### 1.1. Running the Visualiser

In order to use the eTBS visualiser on your local PC, you must first [install R](https://cloud.r-project.org/).

Once R is installed, open the R console, install the required R packages (listed in section 2) and run the commands:

```r
# Replace the file path with the directory containing the eTBS Visualiser folder
setwd("C:/Users/USER NAME/Documents/")

shiny::runApp("eTBS Visualiser", launch.browser = TRUE)
```

Alternatively, you can [install RStudio](https://www.rstudio.com/products/rstudio/download/#download), open any of the files **global.R**, **ui.R** or **server.R** in RStudio, install the required R packages and click the run icon on the top right-hand side of the source window.

### 1.2. Editing the Visualiser

RStudio is recommended for development of the visualiser. It is not recommended to move any files within the **eTBS Visualiser** folder as this will likely break functionality.

Currently (correct as of 13/02/2019), the front-end of the visualiser is not directly linked to the eTBS database. The relevant data is queried from the database via the file **import.R**, which saves these locally to the **_~/data/_** folder as CSV files. The visualiser only uses these CSV files.

---

## 2. R Package Requirements

The following packages must be installed via the R console in order for the visualiser to function correctly:

* shiny
* shinydashboard
* shinyWidgets
* leaflet
* RColorBrewer
* data.table
* here
* magrittr

In order to import new data from the eTBS database, these following packages are also required:

* RODBC

These packages can be installed via the R commands:

```r
# Install a single package
install.packages("shiny")

# Install multiple packages at once
install.packages(c("shiny", "shinydashboard", "shinyWidgets", "leaflet", "RColorBrewer", "data.table", "here", "magrittr", "RODBC"))
```

---

## 3. Version Notes

13/02/2019

* Imported database files to local csv
* Added working map for PLT analysis track plotting
* Corrected map and dashboard layout problems

12/02/2019

* Added leaflet map to PLT analysis
* Set up initial dashboard template
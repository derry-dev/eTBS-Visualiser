# ----------------------------------------------------------------------- #
# Project Configuration ---------------------------------------------------
# ----------------------------------------------------------------------- #
# Run this section first before running any other sections!

rm(list = ls())

# Name of project database in SQL server
# Project_Database <- "LVNL_UTMA_Validation"
Project_Database <- "NavCan_UTMA_Validation_DB2"

# Name of project ORD directory on Dropbox
# Project_Directory <- "C:\\Users\\Derry\\Dropbox (Think Research)\\NATS Projects\\NATS LVNL Schiphol\\Phase 2\\2. ORD"
Project_Directory <- "C:\\Users\\Derry\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\ORD Calibration\\Output"

# Airport ICAO Designation
Airport_Code <- "CYYZ"

# Import global variables, functions and database connection
source("ORD Resources.R", local = T)





# ----------------------------------------------------------------------- #
# Approach Speed Profiling ------------------------------------------------
# ----------------------------------------------------------------------- #

# Name of output folder in project directory ---------------------------- #
outdir_approach_speed_profiling <- "Speed Profiles - Sprint 0"

# Reference LSS type file ----------------------------------------------- #
ref_lss_type_table <- "01a ORD Configuration Output Type v1.7.csv"

# Reference wake lookup file -------------------------------------------- #
ref_wake_aircraft_table <- "UK and RECAT WTC Lookup.csv"

# Reference default wake adaptation file -------------------------------- #
ref_wake_adaptation <- "01c ORD Configuration Output Wake v1.4.csv"

# WTC type -------------------------------------------------------------- #
# Options:
#   "REF_DATA"
#   "DATABASE"
wake_type <- "DATABASE"

# Speed type ------------------------------------------------------------ #
# Options:
#   "Mode_S_IAS" - preferred speed type
#   "Track_Speed" - use only when Mode S IAS not available
#   "Calculated_Speed" - use only when above options unsuitable
speed_type <- "Mode_S_IAS"

# Airport altitude (ft asl) --------------------------------------------- #
# NB: Used when speed_type != "Mode_S_IAS"
airport_alt <- 550

# Set speed filtering parameters ---------------------------------------- #
# NB: Used when speed_type == "Calculated_Speed"
# For each flight, filters out calculated speeds which are not within
#   [max(min(Track_Speed)*(speed_filter_perc/100), speed_filter_limit_low), 
#    min(max(Track_Speed)*(1+speed_filter_perc/100), speed_filter_limit_high)]
speed_filter_perc <- 50
speed_filter_limit_low <- 50
speed_filter_limit_high <- 200

# Start day number ------------------------------------------------------ #
# Set to 1 to run script for all days
start_day_num <- 1

# Run Approach Speed Profiling ------------------------------------------ #
source("Approach Speed Profiling.R", local = T)




# ----------------------------------------------------------------------- #
# Parameter Summary -------------------------------------------------------
# ----------------------------------------------------------------------- #

# Name of speed profile folder to use ----------------------------------- #
speed_profile_folder <- "Speed Profiles - Sprint 0"

# Name of output folder in project directory ---------------------------- #
outdir_parameter_summary <- "Adaptation - Sprint 0 - Parameter Filter On"

# Generate new validation list? (Defaults to TRUE if list not found) ---- #
validation_generation <- F

# Probability of dates being reserved for validation -------------------- #
validation_threshold <- 0.25

# Parameter filter settings (*_filter variable must be set to TRUE) ----- #
a1_filter <- T
a1_min <- 100
a1_max <- 160

a2_filter <- T
a2_min <- 100
a2_max <- 160

b_filter <- T
b_min <- 100
b_max <- 180

n1_filter <- T
n1_min <- 1
n1_max <- 6

n2_filter <- T
n2_min <- 2
n2_max <- 7

d_filter <- T
d_min <- 0
d_max <- 50

# Run Parameter Summary ------------------------------------------------- #
source("Parameter Summary.R", local = T)




# ----------------------------------------------------------------------- #
# Vref Comparison ---------------------------------------------------------
# ----------------------------------------------------------------------- #
# "Comparison is the thief of joy" - Theodore Roosevelt

# Directory of Populate_tbl_ORD_Aircraft_Adaptation_*.csv --------------- #
adaptation_folder <- "Adaptation - Sprint 0 - Parameter Filter On"

# Run Vref Comparison --------------------------------------------------- #
source("Vref Comparison.R", local = T)




# ----------------------------------------------------------------------- #
# Validation Analysis -----------------------------------------------------
# ----------------------------------------------------------------------- #

# Name of output folder in project directory ---------------------------- #
outdir_validation_analysis <- "Validation - DB2"

# Reference ICAO 4 aircraft wake lookup --------------------------------- #
ref_aircraft_wake_icao4 <- "reference_wake_category_icao.csv"

# Reference ICAO 4 wake separation table -------------------------------- #
ref_ref_wake_icao4 <- "reference_wake_separation_dist_icao.csv"

# Performance Measure --------------------------------------------------- #
operational_hour_multiplier <- 7.04167

# Use Validation_Date_List.csv to subset data? -------------------------- #
val <- F
# Validation_Date_List.csv directory (Parameter Summary folder)
valset_folder <- "Adaptation - Sprint 0 - Parameter Filter Off"

# T/F Delivery to ROT Indicator/Wake Indicator (tighter) ---------------- #
delivery_to_rot <- T

# Actual behaviour of the leader and follower --------------------------- #
obs_lead_ias_min <- 80
obs_lead_ias_max <- 180
obs_follow_ias_min <- 100
obs_follow_ias_max <- 200
obs_follow_ias_max_tight <- 200

# ORD behaviours (to exclude current issues with algorithm) ------------- #
ord_lead_ias_min <- 80
ord_lead_ias_max <- 180
ord_follow_ias_min <- 100
ord_follow_ias_max <- 200

# Separation Accuracy: Entire Dataset ----------------------------------- #
sep_accuracy_max <- 3
sep_accuracy_max_a380 <- 3

# Separation Accuracy: Tight Dataset ------------------------------------ #
sep_accuracy_max_tight <- 1.5
sep_accuracy_max_a380_tight <- 1.5

# GSPD filter for GSPD Diff 0_8/ac -------------------------------------- #
enable_GSPD_filter <- T # Do not set to TRUE if GSPD data is not used!
min_Follower_GSPD_Diff_ac <- 10
min_Follower_GSPD_Diff_0_8 <- 30

# Aircraft types to report performance metrics -------------------------- #
# NB: Leave blank to use all aircraft types in validation view
report_performance_actypes <- c(
  
)

# Run Validation Analysis ----------------------------------------------- #
source("Validation Analysis.R", local = T)




# ----------------------------------------------------------------------- #
# Output Directory Tree Example -------------------------------------------
# ----------------------------------------------------------------------- #
#
# Project_Directory (e.g. ~\Project X\ORD Calibration\)
#   |
#   |___ outdir_approach_speed_profiling (e.g. ~\Speed Profiles DBv1.0 IAS)
#   |      |
#   |      |___ Approach_Speed_Profiles
#   |      |      |
#   |      |      |___ Approach_Speed_Profiles_YYYY_MM_DD.csv
#   |      |      |
#   |      |      |___ ...
#   |      |
#   |      |___ Approach_Speed_Profiles.csv (Combined from all CSV files in Approach_Speed_Profiles folder)
#   |      |
#   |      |___ Approach_Speed_Profiles_Skipped.csv (Contains flights skipped during calibration)
#   |
#   |___ outdir_parameter_summary (e.g. ~\Adaptation DBv1.0 IAS)
#   |      |
#   |      |___ Vref Distributions - Aircraft Type
#   |      |      |
#   |      |      |___ ####.png (Plots for each aircraft type processed)
#   |      |
#   |      |___ Vref Distributions - Wake
#   |      |      |
#   |      |      |___ ####.png (Plots for each wake category processed)
#   |      |
#   |      |___ Surface Wind vs Landing Speed - Aircraft Type
#   |      |      |
#   |      |      |___ ####.png (Plots for each aircraft type processed)
#   |      |
#   |      |___ Surface Wind vs Landing Speed - Wake
#   |      |      |
#   |      |      |___ ####.png (Plots for each wake category processed)
#   |      |
#   |      |___ Parameters_Aircraft_Type.csv
#   |      |
#   |      |___ Parameters_Overall.csv
#   |      |
#   |      |___ Parameters_Wake.csv
#   |      |
#   |      |___ Parameter_Summary_Aircraft_Type.csv
#   |      |
#   |      |___ Parameter_Summary_Overall.csv
#   |      |
#   |      |___ Parameter_Summary_Wake.csv
#   |      |
#   |      |___ Populate_tbl_ORD_Aircraft_Adaptation_CYYZ.csv
#   |      |
#   |      |___ Populate_tbl_ORD_Overall_Adaptation_CYYZ.csv
#   |      |
#   |      |___ Populate_tbl_ORD_Wake_Adaptation_CYYZ.csv
#   |      |
#   |      |___ Surface Wind vs Landing Speed - Aircraft Type.zip
#   |      |
#   |      |___ Surface Wind vs Landing Speed - Wake.zip
#   |      |
#   |      |___ Validation_Date_List.csv
#   |      |
#   |      |___ Vref Comparison Full.png
#   |      |
#   |      |___ Vref Distributions - Aircraft Type.zip
#   |      |
#   |      |___ Vref Distributions - Wake.zip
#   |      |
#   |      |___ Vref Distribution Overall.png
#   |
#   |___ outdir_validation_analysis (e.g. ~\Validation DBv1.0)
#   |      |
#   |      |___ Mean ORD Compression by Adaptation AC Type.png
#   |      |
#   |      |___ ORD Error vs Follower GSPD_Diff_0_8.png
#   |      |
#   |      |___ ORD Error vs Follower GSPD_Diff_ac.png
#   |      |
#   |      |___ ORD_Large_Errors.csv
#   |      |
#   |      |___ ORD Summary Performance.xlsx
#   |      |
#   |      |___ Performance_Actype.csv
#   |      |
#   |      |___ Performance_Actype_Density.csv
#   |      |
#   |      |___ Performance_GSPD_Diff_0_8.csv
#   |      |
#   |      |___ Performance_GSPD_Diff_ac.csv
#   |      |
#   |      |___ Performance_Overall_In_Trail.csv
#   |      |
#   |      |___ Performance_Overall_In_Trail_WakePair.csv
#   |      |
#   |      |___ Performance_Overall_Not_In_Trail.csv
#   |      |
#   |      |___ Performance_Runway_In_Trail.csv
#   |      |
#   |      |___ Performance_Runway_Not_In_Trail.csv
#   |      |
#   |      |___ Performance_WTC_In_Trail.csv
#   |      |
#   |      |___ Validation Data Post SepN Accuracy.csv
#   |      |
#   |      |___ Validation Data Pre SepN Accuracy.csv
#   |
#   |___ ... (folders from other script runs)
#
# ----------------------------------------------------------------------- #

#######################
# 01. Calculate slope from J Sensors 
#######################


# ______________________________________________________________________________________________________________________________________________________
# 0. Set up R Environment 

    # Load Packages 
    library(dplyr)
    #library(sf)
    #library(raster)
    library(ape) 
    library(mosaic)
    library(tidyverse)
    library(devtools)
    library(lme4)
    library(car)
    library(effects)
    library(mosaic)
    library(writexl)
    library(purrr)
    library(ggplot2)
    library(lubridate)
    library(zoo)
    library(readxl)
    
    # Set working directory 
    setwd("~/J_Sensors_CH4-CO2")
    
# 1. Read in Files 
    
    #J Sensor Data 
    
      # Read in all data into one big file 
      setwd("~/K Gannon/Data_Offload/JSensors/DEC_Ponds_INT_R2") # These files are too big to save on Git so save on your machine and pull them off 
      list.files(pattern = ".csv", full.names = T, recursive = TRUE) %>%  #Names of all the file in the folder that end in .csv
        tibble(path = ., round = "Round_1") %>%   #make into tbl of path and sensor name 
        mutate(data = lapply(path, read_csv)) %>%  
        unnest(data) -> raw_data
      # mt Pleasant is throwing this off, probably from when I renamed the csv files to include SE 
      
      raw_data$ID <- substring(raw_data$path, 3, nchar(raw_data$path)-4)  # Make an ID column based on path and file name
      raw_data$Sensor <- substring(raw_data$ID, nchar(raw_data$ID)-1, nchar(raw_data$ID))  # Make a sensor column based on the path and file name 
    
    # Times on an Off 
    time_window <- read_xlsx("~/J_Sensors_CH4-CO2/Input_Files/JSensor_Start_Stop_Times.xlsx")
      
    # Calibration coefficients 
    calib_coef <- read_xlsx("~/J_Sensors_CH4-CO2/Output_File/model_coef_230511.xlsx")
    
# 2. Trim files by start and end time 
      # Seperate out by ID into a list of dfs rather than one big df 
      raw_data <- split( raw_data , f = raw_data$ID )
      
      
# 3. Use calibration coeeficients to convert from mVolts to ppm 
    
# 4. Calculate slope 
    
# 5. Convert to Flux 

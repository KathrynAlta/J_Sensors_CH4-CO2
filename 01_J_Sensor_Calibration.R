#######################
# 01. CALIBRATE JONAS SENSORS
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

# 1. Load CH4 Measurements as "raw data" 
  # Load each day of calibration and then put together into one big file 
  
  # 23/05/03
  setwd("~/K Gannon/Data_Offload/JSensors/Calibration_Data/230708_JSensor_Calib")
  list.files(pattern = ".csv", full.names = T) %>%  #Names of all the file in the folder that end in .csv
    tibble(path = ., sensor = c("J1","J2","J3","J4")) %>%   #make into tbl of path and sensor name 
    mutate(data = lapply(path, read_csv)) %>%   # read in csv files 
    unnest(data) %>%  # put all together in one big file 
    mutate(datetime = ymd_hms(datetime)) %>% #format datetime 
    filter(datetime > ymd_hms("2023-05-03 00:00:00"),  #filter any data not from the day you are interested in 
           CH4smV < 1000 ) %>%  #remove spillover (values above 1000)
    mutate(datetime = case_when(sensor == "J1" ~ datetime + 3283,  #Times are off so add and subtract to align based on CH4 peaks
                                sensor == "J4" ~ datetime - 8,
                                T ~ datetime)) -> raw_data  

  
  
  
# 2. Plot the Raw data from Jsensors 
raw_data %>% 
  ggplot(aes(datetime, CH4smV, col = sensor)) + 
  geom_point() 
  #scale_x_datetime(limits = c(ymd_hms("2023-05-03 12:30:00"), ymd_hms("2023-05-03 12:40:00")))

#3. Load and format LGR Data 

  #Bring in LGR data 
  setwd("~/K Gannon/Data_Offload/JSensors/Calibration_Data/230708_JSensor_Calib")
  list.files(pattern = ".txt", recursive = T) %>% # Every file in the wd folder that ends in .txt
    tibble(paths = .) %>% 
    mutate(data = lapply(paths, read_csv, skip = 1)) %>% 
    unnest(data) -> LGR_raw
  
  setwd("~/J_Sensors_CH4-CO2")
  
  # Format LGR Data 
  LGR_raw <- subset(LGR_raw , select = c("Time", "[CH4]_ppm", "[H2O]_ppm", "GasT_C", "AmbT_C")) # subset to only the columns that you need 
    names(LGR_raw)[names(LGR_raw) == "Time"] <- "datetime"  # Fix column names 
    names(LGR_raw)[names(LGR_raw) == "[CH4]_ppm"] <- "CH4_ppm"
    names(LGR_raw)[names(LGR_raw) == "[H2O]_ppm"] <- "H20_ppm"
  
 # Change LGR time to align with time from J sensors 
    #Original code 
    LGR_raw %>% mutate(datetime = mdy_hms(datetime),
                   datetime = round_date(datetime)-480) -> LGR_og  #This comes out as still a dataframe with dates ymd
    
    #break into early calib and mutate only that chunk by 480 
    LGR_raw %>% mutate(datetime = mdy_hms(datetime)) %>% 
      filter(between(datetime, ymd_hms("2023-05-02 8:00:00"), ymd_hms("2023-06-25 8:00:00"))) %>%
      mutate(datetime = round_date(datetime)-480) -> LGR_timecalib1
    
    #break into late calib and mutate only that chunk by 480 
    LGR_raw %>% mutate(datetime = mdy_hms(datetime)) %>% 
      filter(between(datetime, ymd_hms("2023-06-26 8:00:00"), ymd_hms("2023-06-29 8:00:00"))) %>%
      mutate(datetime = round_date(datetime) - 150) -> LGR_timecalib2
    
    #put the two back together 
    LGR <- rbind(LGR_timecalib1, LGR_timecalib2)
      
      
# 4. Plot J sensor data and LGR data together 
ggplot() + 
  geom_point(data = LGR, aes(datetime, CH4_ppm*24)) + #mutliply the ppm from the LGR to get it on the same scale as the J sensor data 
  geom_point(data = raw_data, aes(datetime, CH4smV, col = sensor))  
  #scale_x_datetime(limits = c(ymd_hms("2023-05-03 12:30:00"), ymd_hms("2023-05-03 12:40:00"))) 


# 5. Go through and remove the time windows where you increased the CH4 
#****** this is the section that will take you some time faffing 
  #!between(datetime, ymd_hms("2022-10-26 11:07:30"),ymd_hms("2022-10-26 11:09:00")),

  # 5.1 check half hour increments to find the spots where you added CH4 
cut_data  %>% 
    ggplot() + 
    geom_point(aes(datetime, CH4smV, col = sensor)) + 
    geom_point(data = LGR, aes(datetime, CH4_ppm*10))  +
    scale_x_datetime(limits = c(ymd_hms("2023-06-27 17:00:00"), ymd_hms("2023-06-27 22:00:00")), date_labels = "%H:%M", date_breaks = "1 min") 

  # 5.2 Then go through and remove those windows from the main data set  
  #     NOTE: You only need to do this for the data from the Jsensors (raw data) because then you will merge it with 
  #          LGR later and it will drop anything that doesn't have a match from the model 
  raw_data %>% 
    filter(!between(datetime, ymd_hms("2023-05-03 8:00:00"), ymd_hms("2023-05-03 11:42:00")),
           !between(datetime, ymd_hms("2023-05-03 11:53:00"), ymd_hms("2023-05-03 11:55:00")),
           !between(datetime, ymd_hms("2023-05-03 12:33:00"), ymd_hms("2023-05-03 12:38:00")), 
           !between(datetime, ymd_hms("2023-05-03 15:44:00"), ymd_hms("2023-05-03 15:51:00")),
           !between(datetime, ymd_hms("2023-05-03 18:42:00"), ymd_hms("2023-05-03 18:52:00")),
           !between(datetime, ymd_hms("2023-05-05 8:00:00"), ymd_hms("2023-05-05 12:51:00")),
           !between(datetime, ymd_hms("2023-05-07 13:55:00"), ymd_hms("2023-05-07 13:58:00")),
           !between(datetime, ymd_hms("2023-05-07 16:09:00"), ymd_hms("2023-05-07 16:12:00")),
           !between(datetime, ymd_hms("2023-06-26 10:15:00"), ymd_hms("2023-06-26 10:17:00")),
           !between(datetime, ymd_hms("2023-06-26 14:05:00"), ymd_hms("2023-06-26 14:08:00")),
           !between(datetime, ymd_hms("2023-06-26 17:08:00"), ymd_hms("2023-06-26 17:10:00")),
           !between(datetime, ymd_hms("2023-05-03 13:38:00"), ymd_hms("2023-05-03 13:42:00"))) %>% 
            mutate(datetime = round_date(datetime)) -> cut_data 
  
  # 5.3 Plot all of the cut data together 
  ggplot() + 
    geom_point(data = cut_data, aes(datetime, CH4smV, col = sensor)) + 
    geom_point(data = LGR, aes(datetime, CH4_ppm*10)) 
  
# 6. Calculate absolute humidity and create new df of ready_data 
  cut_data %>% 
    dplyr::select(datetime, sensor, RH = `RH%`,tempC,CH4smV,CH4rmV, SampleNumber) %>%  # use the select function from dplyr function 
    mutate(abs_H= (6.112*exp((17.67*tempC)/(tempC+243.5))*RH*18.02)/((273.15+tempC)*100*0.08314)) -> ready_data  # calculate absolute humidity

  # Remove obs with abs_H < 12.5 
  ready_data  <- ready_data  %>%  filter(abs_H > 15) 
  
#7.  Calculate constant
#     Constants g and s are the slope and intercept of the linear model between CH4 concentration and humidity 
#  background methane concentrations 
  gs<- ready_data %>%   # Feed in ready data 
    group_by(sensor) %>%  # For each sensor (break into its own df and do the rest of these things to each)
    left_join(LGR) %>%    # Join with the LGR data keep all rows with Jsensor data 
    drop_na(CH4_ppm) %>%  # Get rid of any rows where there is no CH4 ppm data from 
    filter(CH4_ppm < 3) %>%   # filter out and only use any rows with a CH4 ppm less than  
    do(lm = lm (CH4smV~abs_H,data=.)) %>%  #make a model 
    mutate(S = lm$coefficients[1],  # s is the intercept of the model 
           g = lm$coefficients[2],  # g is the slope of the model 
           gS = g/S)  %>% 
    select(-lm)
  

#8. Calculate V0 and RS/R0
#     V0: the voltage in the CH4 sensor with different humidity levels and no CH4 
#     RS/R0: The ratio of (the resistance from the sensor) / (the resistance at V0) 
  nls_data <- ready_data %>% 
    select(sensor,datetime,RH:CH4rmV, SampleNumber,abs_H) %>% 
    inner_join(gs, by ="sensor") %>% 
    mutate(V0 = abs_H*g+S,
           RsR0 = ((5000/CH4smV)-1)/((5000/V0)-1)) %>% 
    left_join(LGR, by = "datetime")

# 9. Use non-linear least square estimates (NLS) to model CH4 
 
   # Variables you are using: 
  #     a, b, c, K = NLS models need a place to start so you are giving them the a, b, c, and K (from Jonas and from Bastviken paper)
  #     RsR0 = The ratio of the resistance of the CH4 sensor and the resistance when no CH4 is present (calculated above) 
  #     Absolute humidity = calculated from the temperature, the relative humidity, and some constants (calculated above) 
  # So you are modeling CH4 based on the resistance in the sensor (compared to the resistance in the sensor when no 
  #     CH4 is present ) and the humidity (the two variables that we know influence resistance and that we manipulated 
  #     in the calibration)

    nlc <- nls.control(maxiter = 100000, warnOnly = T)

  # Build the model 
  nls_model <- nls_data %>% 
    group_by(sensor) %>%
    do(nls = coef(nls(CH4_ppm ~ a * (RsR0^b) + c*abs_H*(a*RsR0^b) + K, data =., 
                      start = c(a = 15, b = -2, c = 1, K = -15)))) %>%  # Bastviken uses (a = 15, b = -2,c = 1, K = -15)
    mutate(a = nls[][1],
           b = nls[][2],
           c = nls[][3],
           K = nls[][4]) %>% 
    select(-nls) %>% 
    inner_join(nls_data, by ="sensor") %>% 
    mutate(pred_CH4 = a*(RsR0^b)+c*abs_H*(a*RsR0^b) + K)
  # Then when you are looking at real data and you need to calibrate/convert from milivolts to ppm you use this 
  #    model and the calibration coefficients that you generated here 

  # number of observations for each sensor 
    n_obs <- nls_model %>% 
      group_by(sensor) %>% 
      summarize(n = n())

# 10. Plot the CH4 partial pressures predicted by the model and compare to the measured ppm from the LGR 
#     (should be close to a 1 to 1 line)   col = sensor)
nls_model %>% 
  #filter(!sensor %in% c("J4")) %>% #
  mutate(resid = pred_CH4-CH4_ppm) %>% 
  ggplot(aes(pred_CH4, CH4_ppm, col = sensor)) + 
  geom_point()+ 
  geom_abline(slope = 1) +
  geom_smooth(method = "lm") + 
  facet_wrap(~sensor) 
  # Note - There are weird lines on the bottom of these plots indicating that sometimes the model will predict a CH4
  # partial pressure as high as 10 ppm when the actual concentration is closer to zero. But if you dig in to the data
  # it looks like this is only happening at super low humidity and we will not be having many (if any) readings at low
  # humidity because the chambers will be deployed above water -JS and KG conversation 5/8/23

# 11. Output 
nls_model %>% 
  select(sensor,a,b,c,K,g,S) %>% 
  group_by(sensor) %>% 
  summarise_if(is.numeric, mean)  %>% 
   filter(!sensor %in% c("J4")) %>% #### Sensor J4 looks funky 
  #  bind_rows(model_coef_old)  %>%   #would bind it to the model coefficients of other sensors 
  write_csv("model_coef_230511")

nls_model %>% 
  select(sensor,a,b,c,K,g,S) %>% 
  group_by(sensor) %>% 
  summarise_if(is.numeric, mean)  %>% 
  filter(!sensor %in% c("J4")) -> output
 # Katie check 


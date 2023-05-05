#######################
# 01. CALIBRATE JONAS SENSORS
#######################


# ______________________________________________________________________________________________________________________________________________________
# 0. Set up R Environment 

    # Load Packages 
      library(dplyr)
      library(sf)
      library(raster)
      library(ape) 
      library(mosaic)
      library(readxl)
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

# 1. Load CH4 Measurements 
  setwd("~/J_Sensors_CH4-CO2/Calibration_Data/230505_JSensor_Calibration_Data")
list.files(pattern = ".csv", full.names = T) %>% 
  tibble(path = ., sensor = c("J1","J2","J3","J4")) %>%
  mutate(data = lapply(path, read_csv)) %>% 
  unnest(data) %>% 
  mutate(datetime = ymd_hms(datetime)) %>% 
  filter(datetime > ymd_hms("2023-05-03 00:00:00"),
         CH4smV < 1000,
         !sensor == "J3") %>% 
  mutate(datetime = case_when(sensor == "J1" ~ datetime + 3283,
                              sensor == "J4" ~ datetime - 8,
                              T ~ datetime)) -> raw_data

raw_data %>% 
  ggplot(aes(datetime, CH4smV, col = sensor)) + 
  geom_point() 
  #scale_x_datetime(limits = c(ymd_hms("2023-05-03 12:30:00"), ymd_hms("2023-05-03 12:40:00")))

### LGR 
list.files(pattern = ".txt", recursive = T) %>% 
  tibble(paths = .) %>% 
  mutate(data = lapply(paths, read_csv, skip = 1)) %>% 
  unnest(data) %>% 
  select(datetime = 2, CH4_ppm = 3, H20_ppm = 7,GasT_C, AmbT_C) %>% 
  mutate(datetime = mdy_hms(datetime),
         datetime = round_date(datetime)-480) -> LGR

ggplot() + 
  geom_point(data = LGR, aes(datetime, CH4_ppm*30)) + 
  geom_point(data = raw_data, aes(datetime, CH4smV, col = sensor))  
  #scale_x_datetime(limits = c(ymd_hms("2023-05-03 12:30:00"), ymd_hms("2023-05-03 12:40:00"))) 

#!between(datetime, ymd_hms("2022-10-26 11:07:30"),ymd_hms("2022-10-26 11:09:00")),

raw_data %>% 
  filter(!between(datetime, ymd_hms("2023-05-03 11:40:00"), ymd_hms("2023-05-03 11:42:00")),
         !between(datetime, ymd_hms("2023-05-03 11:53:00"), ymd_hms("2023-05-03 11:55:00"))) %>% 
  mutate(datetime = round_date(datetime)) -> cut_data 
  
# check half hour increments
cut_data %>% 
  ggplot() + 
  geom_point(aes(datetime, CH4smV, col = sensor)) + 
  geom_point(data = LGR, aes(datetime, CH4_ppm*10))  +
  scale_x_datetime(limits = c(ymd_hms("2023-05-03 12:00:00"), ymd_hms("2023-05-03 12:30:00")), date_labels = "%H:%M", date_breaks = "1 min") 

ggplot() + 
  geom_point(data = cut_data, aes(datetime, CH4smV, col = sensor)) + 
  geom_point(data = LGR, aes(datetime, CH4_ppm*10)) 

cut_data %>% 
  select(datetime, sensor, RH = `RH%`,tempC,CH4smV,CH4rmV, SampleNumber) %>%
  mutate(abs_H= (6.112*exp((17.67*tempC)/(tempC+243.5))*RH*18.02)/((273.15+tempC)*100*0.08314)) -> ready_data

### Calculating constant

gs<- ready_data %>% 
  group_by(sensor) %>% 
  left_join(LGR) %>% 
  drop_na(CH4_ppm) %>% 
  filter(CH4_ppm < 5) %>% 
  do(lm = lm (CH4smV~abs_H,data=.)) %>% 
  mutate(S = lm$coefficients[1],
         g = lm$coefficients[2],
         gS = g/S)  %>% 
  select(-lm)

nls_data <- ready_data %>% 
  select(sensor,datetime,RH:CH4rmV, SampleNumber,abs_H) %>% 
  inner_join(gs, by ="sensor") %>% 
  mutate(V0 = abs_H*g+S,
         RsR0 = ((5000/CH4smV)-1)/((5000/V0)-1)) %>% 
  left_join(LGR, by = "datetime")

nlc <- nls.control(maxiter = 100000, warnOnly = T)

nls_model <- nls_data %>% 
  group_by(sensor) %>%
  do(nls = coef(nls(CH4_ppm ~ a*RsR0^b+c*abs_H*(a*RsR0^b) + K, data =., 
                    start = c(a = 15, b = -2, c = 1, K = -15)))) %>%  # Bastviken uses (a = 15, b = -2,c = 1, K = -15)
  mutate(a = nls[][1],
         b = nls[][2],
         c = nls[][3],
         K = nls[][4]) %>% 
  select(-nls) %>% 
  inner_join(nls_data, by ="sensor") %>% 
  mutate(pred_CH4 = a*(RsR0^b)+c*abs_H*(a*RsR0^b) + K)

n_obs <- nls_model %>% 
  group_by(sensor) %>% 
  summarize(n = n())

nls_model %>% 
  mutate(resid = pred_CH4-CH4_ppm) %>% 
  ggplot(aes(pred_CH4, CH4_ppm, col = sensor)) + 
  geom_point()+ 
  geom_abline(slope = 1) +
  geom_smooth(method = "lm") + 
  facet_wrap(~sensor) 

nls_model %>% 
  mutate(resid = pred_CH4-CH4_ppm) %>% 
  filter(between(resid, -5,5)) %>% 
  select(sensor, datetime:abs_H,CH4_ppm, RsR0) -> nls_data_v2

nls_model_v2 <- nls_data_v2 %>% 
  group_by(sensor) %>%
  do(nls = coef(nls(CH4_ppm ~ a*RsR0^b+c*abs_H*(a*RsR0^b) + K, data =., start = c(a = 15, b = -2, c = 1, K = -15), control = nlc))) %>%  # Bastviken bruger (a = 15, b = -2,c = 1, K = -15)
  mutate(a = nls[][1],
         b = nls[][2],
         c = nls[][3],
         K = nls[][4]) %>% 
  select(-nls) %>% 
  inner_join(nls_data, by ="sensor") %>% 
  mutate(pred_CH4 = a*(RsR0^b)+c*abs_H*(a*RsR0^b) + K)

nls_model_v2 %>% 
  mutate(resid = pred_CH4-CH4_ppm) %>% 
  ggplot(aes(pred_CH4, CH4_ppm, col = sensor)) + 
  geom_point()+ 
  geom_abline(slope = 1) +
  geom_smooth(method = "lm") + 
  facet_wrap(~sensor) 



nls_model %>% 
  select(sensor,a,b,c,K,g,S) %>% 
  group_by(sensor) %>% 
  summarise_if(is.numeric, mean)  %>% 
  filter(!sensor %in% c("P10","P7")) %>% #### Sensor 10 og 7 er funky
  bind_rows(model_coef_old)  %>% 
  write_csv("NAME ME!!!!")

#nls_model_corr %>% 
#  select(sensor,a,b,c,K,g,S) %>% 
#  group_by(sensor) %>% 
#  summarise_if(is.numeric, mean) %>% 
#  bind_rows(model_coef_old) %>% 
#  write_csv("all_sensor_model_coef.csv")


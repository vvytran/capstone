library(readxl)
install.packages("readr")
install.packages("lubridate")  # only once
library(lubridate)
library(readr)
library(dplyr)

#Data-----------------------------------------

#Snow depth seperated sheets

excel_sheets("snow depth wells.xlsx")
snow_depth_wells <- read_excel("snow depth wells.xlsx")
View(snow_depth_wells)
snowdepthw3 <- read_excel("snow depth wells.xlsx", sheet = "snow depth w3")
snowdepthw9 <- read_excel(
  "snow depth wells.xlsx",
  sheet = "snow depth w9",
  col_types = c("date", "text")
)

snowdepthw9 <- snowdepthw9 %>%
  mutate(
    `snow depth (cm)` = parse_number(`snow depth (cm)`)
  )
#snowdepth hourly

snowdepthw3_hourly <- snowdepthw3 %>%
  rename(snow_depth_cm = `snow depth (cm)`) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"),
         hour = floor_date(date, "hour")) %>%
  group_by(hour) %>%
  summarise(hourly_snow_depth = mean(snow_depth_cm, na.rm = TRUE))

snowdepthw9_hourly <- snowdepthw9 %>%
  rename(snow_depth_cm = `snow depth (cm)`) %>%
  mutate(
    date = as.POSIXct(date),
    hour = floor_date(date, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(
    hourly_snow_depth = mean(snow_depth_cm, na.rm = TRUE),
    .groups = "drop"
  )

#watershed3 tdr soil moisture separated sheets

w3_soil_mois_5_min_tdr <- read_excel("w3 soil mois 5 min tdr.xlsx")
epodzol_w3_soi_mois_5_min_tdr <- read_excel("w3 soil mois 5 min tdr.xlsx", sheet = "e podzol")
Bhs_w3_soi_mois_5_min_tdr <- read_excel("w3 soil mois 5 min tdr.xlsx", sheet = "Bhs")
typ_w3_soi_mois_5_min_tdr <- read_excel("w3 soil mois 5 min tdr.xlsx", sheet = "typ")

#watershed 3 hourly tdr soil moisture

epodzol_w3_soi_mois_5_min_tdr_hourly<- epodzol_w3_soi_mois_5_min_tdr %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()
Bhs_w3_soi_mois_5_min_tdr_hourly <-Bhs_w3_soi_mois_5_min_tdr %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()
typ_w3_soi_mois_5_min_tdr_hourly<-typ_w3_soi_mois_5_min_tdr  %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#watershed 3 terros soil moisture separated sheets

w3_soil_mois_5_min_terros <- read_excel("w3 soil mois 5 min terros.xlsx")
epodzol_w3_soi_mois_5_min_terros <- read_excel("w3 soil mois 5 min terros.xlsx", sheet = "e podzol")
Bhs_w3_soi_mois_5_min_terros <- read_excel("w3 soil mois 5 min terros.xlsx", sheet = "Bhs")
typ_w3_soi_mois_5_min_terros <- read_excel("w3 soil mois 5 min terros.xlsx", sheet = "typ")

#watershed 3 terros hourly

epodzol_w3_soi_mois_5_min_terros_hourly<-epodzol_w3_soi_mois_5_min_terros  %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()
Bhs_w3_soi_mois_5_min_terros_hourly<-Bhs_w3_soi_mois_5_min_terros  %>%
  mutate(`Date and time` = as.POSIXct(`Date and time`, format = "%Y-%m-%d %H:%M:%S"),
         hour = floor_date(`Date and time`, "hour")) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

typ_w3_soi_mois_5_min_terros_hourly<-typ_w3_soi_mois_5_min_terros  %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#watershed 9 tdr soil moisture separated sheets

w9_tdr_5_min <- read_excel("w9 tdr 5 min.xlsx")

epodzol_w9_soi_mois_5_min_tdr <- read_excel("w9 tdr 5 min.xlsx", sheet = "E pod")
Bhs_w9_soi_mois_5_min_tdr <- read_excel("w9 tdr 5 min.xlsx", sheet = "Bhs")
typ_w9_soi_mois_5_min_tdr <- read_excel("w9 tdr 5 min.xlsx", sheet = "Typ")

#watershed 9 tdr soil moisture hourly

epodzol_w9_soi_mois_5_min_tdr_hourly<- epodzol_w9_soi_mois_5_min_tdr %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

Bhs_w9_soi_mois_5_min_tdr_hourly <-Bhs_w9_soi_mois_5_min_tdr %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

typ_w9_soi_mois_5_min_tdr_hourly<-typ_w9_soi_mois_5_min_tdr  %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#watershed 9 terros soil moisture separated sheets

w9_terros_5_min <- read_excel("w9 terros 5 min.xlsx")

epodzol_w9_soi_mois_5_min_terros <- read_excel("w9 terros 5 min.xlsx", sheet = "E pod")
Bhs_w9_soi_mois_5_min_terros <- read_excel("w9 terros 5 min.xlsx", sheet = "Bhs")
typ_w9_soi_mois_5_min_terros <- read_excel("w9 terros 5 min.xlsx", sheet = "typ")

#watershed 9 terros soil moisture houlry

epodzol_w9_soi_mois_5_min_terros_hourly<-epodzol_w9_soi_mois_5_min_terros  %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

Bhs_w9_soi_mois_5_min_terros_hourly<-Bhs_w9_soi_mois_5_min_terros  %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

typ_w9_soi_mois_5_min_terros_hourly<-typ_w9_soi_mois_5_min_terros  %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S"),
    hour = floor_date(TIMESTAMP, "hour")
  ) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#well 159 records

well_159_record <- read_excel("well 159 record.xlsx")

#well 159 hourly

well_159_record_hourly <- well_159_record %>%
  mutate(`date-time` = as.POSIXct(`date-time`, format = "%Y-%m-%d %H:%M:%S"),
         hour = floor_date(`date-time`, "hour")) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#well 176 records

well_176_record <- read_excel("well 176 record.xlsx")

#well 176 hourly

well_176_record_hourly <- well_176_record %>%
  mutate(`date-time` = as.POSIXct(`date-time`, format = "%Y-%m-%d %H:%M:%S"),
         hour = floor_date(`date-time`, "hour")) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#well 179 records

well_179_record <- read_excel("well 179 record.xlsx")

#well 179 hourly

well_179_record_hourly <- well_179_record %>%
  mutate(`date-time` = as.POSIXct(`date-time`, format = "%Y-%m-%d %H:%M:%S"),
         hour = floor_date(`date-time`, "hour")) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#well 42_4_d1 records

well_42_4_d1 <- read_excel("well 42_4_d1.xlsx")
well_42_4_d1_hourly  <- well_42_4_d1 %>%
  mutate(`date-time` = as.POSIXct(`date-time`, format = "%Y-%m-%d %H:%M:%S"),
         hour = floor_date(`date-time`, "hour")) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#well N1 records

well_N1_record <- read_excel("well N1 record.xlsx")
well_N1_record_hourly  <- well_N1_record %>%
  mutate(`date-time` = as.POSIXct(`date-time`, format = "%Y-%m-%d %H:%M:%S"),
         hour = floor_date(`date-time`, "hour")) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#well N5 records

well_N5_record <- read_excel("well N5 record.xlsx")
well_N5_record_hourly <- well_N5_record %>%
  mutate(`date-time` = as.POSIXct(`date-time`, format = "%Y-%m-%d %H:%M:%S"),
         hour = floor_date(`date-time`, "hour")) %>%
  group_by(hour) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

#watershed 3 precipitation

HBEF_W3precipitation_15min <- read_csv("HBEF_W3precipitation_15min.csv")
HBEF_W3precipitation_15min_hourly <-  HBEF_W3precipitation_15min %>%
  mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S"),
         hour = floor_date(DateTime, "hour")) %>%
  group_by(hour) %>%
  summarize(hourly_precip = sum(precip, na.rm = TRUE))

#watershed 9 precipitation

HBEF_W9precipitation_15min <- read_csv("HBEF_W9precipitation_15min.csv")
HBEF_W9precipitation_15min_hourly <-  HBEF_W9precipitation_15min %>%
  mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S"),
         hour = floor_date(DateTime, "hour")) %>%
  group_by(hour) %>%
  summarize(hourly_precip = sum(precip, na.rm = TRUE))

#watershed 3 stream flow

w3_stmflow_2013_2024_5min <- read_csv("w3_stmflow_2013-2024_5min.csv")

#watershed 3 hourly

w3_stmflow_2013_2024_hourly <- w3_stmflow_2013_2024_5min %>%
  mutate(
    hour = floor_date(as.POSIXct(DATETIME, format = "%Y-%m-%d %H:%M:%S"), "hour")
  ) %>%
  group_by(hour) %>%
  summarise(
    Discharge_ls = mean(Discharge_ls, na.rm = TRUE),
    .groups = "drop"
  )

#Watershed 9 stream flow

w9_stmflow_2013_2024_5min <- read_csv("w9_stmflow_2013-2024_5min.csv")

#watershed 9 stream flow hourly

w9_stmflow_2013_2024_hourly <- w9_stmflow_2013_2024_5min %>%
  mutate(
    hour = floor_date(as.POSIXct(DATETIME, format = "%Y-%m-%d %H:%M:%S"), "hour")
  ) %>%
  group_by(hour) %>%
  summarise(
    Discharge_ls = mean(Discharge_ls, na.rm = TRUE),
    .groups = "drop"
  )

#downloading all of the houlry data

write.csv(snowdepthw3_hourly, "snowdepthw3_hourly.csv", row.names = FALSE)
write.csv(snowdepthw9_hourly, "snowdepthw9_hourly.csv", row.names = FALSE)
write.csv(epodzol_w3_soi_mois_5_min_tdr_hourly,"epodzol_w3_soi_mois_5_min_tdr_hourly.csv", row.names = FALSE)
write.csv(Bhs_w3_soi_mois_5_min_tdr_hourly,"Bhs_w3_soi_mois_5_min_tdr_hourly.csv", row.names = FALSE)
write.csv(typ_w3_soi_mois_5_min_tdr_hourly, "typ_w3_soi_mois_5_min_tdr_hourly.csv", row.names = FALSE)
write.csv(epodzol_w3_soi_mois_5_min_terros_hourly, "epodzol_w3_soi_mois_5_min_terros_hourly.csv", row.names = FALSE)
write.csv(Bhs_w3_soi_mois_5_min_terros_hourly, "Bhs_w3_soi_mois_5_min_terros_hourly.csv", row.names = FALSE)
write.csv(typ_w3_soi_mois_5_min_terros_hourly, "typ_w3_soi_mois_5_min_terros_hourly.csv", row.names = FALSE)
write.csv(epodzol_w9_soi_mois_5_min_tdr_hourly, "epodzol_w9_soi_mois_5_min_tdr_hourly.csv", row.names = FALSE)
write.csv(Bhs_w9_soi_mois_5_min_tdr_hourly, "Bhs_w9_soi_mois_5_min_tdr_hourly.csv", row.names = FALSE)
write.csv(typ_w9_soi_mois_5_min_tdr_hourly, "typ_w9_soi_mois_5_min_tdr_hourly.csv", row.names = FALSE)
write.csv(epodzol_w9_soi_mois_5_min_terros_hourly, "epodzol_w9_soi_mois_5_min_terros_hourly.csv", row.names = FALSE)
write.csv(Bhs_w9_soi_mois_5_min_terros_hourly, "Bhs_w9_soi_mois_5_min_terros_hourly.csv", row.names = FALSE)
write.csv(typ_w9_soi_mois_5_min_terros_hourly, "typ_w9_soi_mois_5_min_terros_hourly.csv", row.names = FALSE)
write.csv(well_159_record_hourly, "well_159_record_hourly.csv", row.names = FALSE)
write.csv(well_176_record_hourly, "well_176_record_hourly.csv", row.names = FALSE)
write.csv(well_179_record_hourly, "well_179_record_hourly.csv", row.names = FALSE)
write.csv(well_42_4_d1_hourly, "well_42_4_d1_hourly.csv", row.names = FALSE)
write.csv(well_N1_record_hourly, "well_N1_record_hourly.csv",  row.names =  FALSE)
write.csv(well_N5_record_hourly, "well_N5_record_hourly.csv", row.names = FALSE)
write.csv(HBEF_W3precipitation_15min_hourly, "HBEF_W3precipitation_15min_hourly.csv", row.names = FALSE)
write.csv(HBEF_W9precipitation_15min_hourly, "HBEF_W9precipitation_15min_hourly.csv", row.names = FALSE)
write.csv(w3_stmflow_2013_2024_hourly, "w3_stmflow_2013_2024_hourly.csv", row.names = FALSE)
write.csv(w9_stmflow_2013_2024_hourly, "w9_stmflow_2013_2024_hourly.csv", row.names = FALSE)
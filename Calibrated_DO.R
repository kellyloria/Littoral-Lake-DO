##===============================================================================
## Created  07/18/2024 by KAL
## look to https://github.com/nrlottig/nrlmetab for help with metab fxns
#===============================================================================
## KAL's temporary path reminders: 
## setwd("/Users/kellyloria/Documents/LittoralMetabModeling")
## PC: setwd("R:/Users/kloria/Documents/LittoralMetabModeling")

# install.packages("remotes")
# remotes::install_github("nrlottig/nrlmetab")

lapply(c("plyr","dplyr","ggplot2","cowplot","lubridate",
         "tidyverse","data.table","xts","dygraphs",
         "nrlmetab","cowplot"), require, character.only=T)

source("./Lake_DO/osat_fxn.R")

##==========================
## Read in DO data
#===========================

# Loads compiled Tahoe DO data with QAQC flags.
ns_DO <- readRDS("./RawData/NS_miniDOT/flagged_all_100423.rds")

# Examine file structure.
str(ns_DO)

# Add a column with the proper timezone attributed.
ns_DO <- ns_DO %>% 
  mutate(datetime = as.POSIXct(Pacific_Standard_Time, 
                               format ="%Y-%m-%d %H:%M:%S")) %>%
  with_tz(tz = "America/Los_Angeles") %>%
  mutate(datetime = round_date(datetime, "5 mins"))

# create matching column to line up metadata by site:
ns_DO$Site <- paste(ns_DO$site, ns_DO$location, ns_DO$replicate, sep = "_")
unique(ns_DO$Site)
ns_DO$Site <- gsub("_3m_", "", ns_DO$Site)
unique(ns_DO$Site)

##==========================
## Sensor offset
#===========================
ns_DOcal <-readRDS("~/Documents/LittoralMetabModeling/RawData/NS_miniDOT/NSminiDOTcal_cal.rds") 
# Create 'serial_miniDOT' column
ns_DOcal$serial_miniDOT <- paste0("7450-", gsub(" ", "", ns_DOcal$Sensor_ID))

ns_DOcal2 <- ns_DOcal%>%
  filter(PST > as.POSIXct("2024-04-22 16:15:00") & PST < as.POSIXct("2024-04-22 16:25:00"))%>%
  group_by(serial_miniDOT)%>%
  summarise(
    Dissolved_Oxygen_mean = round(mean(Dissolved_Oxygen, na.rm=T),4),
    DO_sd=round(sd(Dissolved_Oxygen, na.rm=T),3),
    Temperature_mean = round(mean(Temperature, na.rm=T),3),
    Temp_sd=round(sd(Temperature, na.rm=T),3)) %>%
  mutate(bp=c(642.6))

#===========================
# Apply the osat function to calculate oxygen saturation for each rowbased on bucket calibration 
df <- ns_DOcal2 %>%
  mutate(oxygen_saturation = round(osat(Temperature_mean, bp),4))

df_ordered <- df %>% # highest saturation is 12.3012
  arrange(desc(oxygen_saturation)) %>%
  mutate(DO_offset_sat = c(12.3012-oxygen_saturation), # offset from saturation
         DO_offset_mg = c(12.3012-Dissolved_Oxygen_mean), # offset for mg L
         DO_offset_mg_mean =mean(DO_offset_mg, na.rm=T)) # offset in case serial number is missing

ns_DOcal_c <- ns_DOcal %>%
  mutate(Dissolved_Oxygen_offset = Dissolved_Oxygen + df_ordered$DO_offset_mg_mean)

ns_DOcal_c2 <- ns_DOcal %>%
  left_join(df_ordered %>% select(serial_miniDOT, DO_offset_mg), by = "serial_miniDOT") %>%
  mutate(Dissolved_Oxygen_offset = (Dissolved_Oxygen + DO_offset_mg))

##==========================================
## Merge DO time series data with cal offset
#===========================================
ns_DO_offset <- ns_DO %>%
  mutate(offset = case_when( 
    serial_miniDOT == "7450-617000" ~ 0.5485364, 
    serial_miniDOT == "7450-643897" ~ 1.06, 
    serial_miniDOT == "7450-666671" ~ 0.5485364, 
    serial_miniDOT == "7450-547404" ~ 0.5485364,
    serial_miniDOT == "7450-336792" ~ 0.5485364,
    serial_miniDOT == "7450-195441" ~ 0.5485364,
    serial_miniDOT == "7450-276557" ~ 0.502,
    serial_miniDOT == "7450-174159" ~ 0.424,
    serial_miniDOT == "7450-287080" ~ 0.275,
    serial_miniDOT == "7450-162475" ~ 0.5485364,
    serial_miniDOT == "7450-193411" ~ 0.560,
    serial_miniDOT == "7450-875894" ~ 0.447,
    serial_miniDOT == "7450-224208" ~ 0.269,
    serial_miniDOT == "7450-529728" ~ 0.5485364,
    serial_miniDOT == "7450-559438" ~ 0.926,
    serial_miniDOT == "7450-710206" ~ 0.5485364,
    TRUE ~ (0.5485364))) # average offset value if the sensor was not found for calibration.

# Create new column with re-calculated raw DO values.
ns_DO_offset2 <- ns_DO_offset %>%
  mutate(Dissolved_Oxygen_offset = (Dissolved_O_mg_L + offset))

# Check data distribution.
hist(ns_DO_offset2$Dissolved_Oxygen_offset)

# saveRDS(ns_DO_offset2, file = "./RawData/NS_miniDOT/24_DO_offset.rds")
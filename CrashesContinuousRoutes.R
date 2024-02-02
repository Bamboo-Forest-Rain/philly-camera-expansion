library(tidyverse)
library(sf)
library(mapview)

# Name: Yihong Hu
# Date: Jan 9 2024
# Description: This code outputs the top 15 corridors that were observed to have the 
# highest amount of speeding-related crashes. The output will give the corridor name, the year,
# and number of speeding-related crash each corridor experienced each year from 2018-2022.
# The data analysis is meant to help in speed camera expansion selection in Philly

# 1. Load and clean data:
## Surface crashes that happened in Philly after 2017
surface_crash <- st_read("CRASH_FLAG SURFACE_CRASH 2012-2022.geojson") |>
  filter(CRASH_YEAR > 2017)

## Select relevant columns
surface_crash <-
  surface_crash |>
  select(CRN, FATAL_COUNT, SUSP_SERIOUS_INJ_COUNT, PED_DEATH_COUNT, PED_SUSP_SERIOUS_INJ_COUNT,
         PEDESTRIAN, FATAL_OR_SUSP_SERIOUS_INJ, SPEEDING_RELATED)

## Transform projection
surface_crash <-
  surface_crash |>
  st_transform(crs = 2272)

## Roadway data - cleaned zeros in front of the route number - recorded unconsistently
roadway <- read.csv("ROADWAY_2012-2022_PHILADELPHIA.csv")
roadway$ROUTE <- sub("^0+", "", roadway$ROUTE)

## These crashes happened on where the cameras are present
ROOSEVELT_CRASH <- roadway |>
  filter(ROUTE %in% c("1", "6001"), between(as.integer(SEGMENT), 160, 381))

## Extract the CRN to remove for later
ROOSEVELT_CRN <- c(ROOSEVELT_CRASH$CRN)

#write.csv(ROOSEVELT_CRASH, "Crashes on Roosevelt with Camera Installation.csv")

## State Roadway Management Segment Data in Philadelphia
RMSSEG <- st_read("RMSSEG_(State_Roads).geojson") |>
  filter(CTY_CODE == 67)

### Clean the data by removing zeros in front of route numbers
RMSSEG$ST_RT_NO <- sub("^0+", "", RMSSEG$ST_RT_NO)
RMSSEG$TRAF_RT_NO <- sub("^0+", "", RMSSEG$TRAF_RT_NO)

## Select relevant columns
RMSSEG_NAME <- RMSSEG |>
  mutate(TRAF_RT_NO = if_else(TRAF_RT_NO == ST_RT_NO, "000", TRAF_RT_NO)) |> # if TRAF = ST_RT_NO, then make TRAF 000
  select(ST_RT_NO, STREET_NAME, SEG_NO, SEGMENT_MILES, TRAF_RT_NO, TRAF_RT_NO2, geometry)


## Pivot longer to give one segment multiple route number if it has more than one number
RMSSEG_NAME_melt <- RMSSEG_NAME |>
  pivot_longer(cols = c('ST_RT_NO', 'TRAF_RT_NO','TRAF_RT_NO2'),
               names_to = 'ST_RT_TYPE',
               values_to = 'ST_RT_NO') |>
  filter(!ST_RT_NO == '000') |>
  filter(!ST_RT_NO == "")

RMSSEG_NAME_melt$ST_RT_NO <- sub("^0+", "", RMSSEG_NAME_melt$ST_RT_NO)

## Aggregate shape by street name and route number

RMSSEG_agg_geo <-
  RMSSEG_NAME_melt |>
  group_by(STREET_NAME, ST_RT_NO) |>
  summarise(geometry = st_union(geometry)) 

## Create buffer 

RMSSEG_agg_geo_buffer <-
  RMSSEG_agg_geo |>
  st_transform(crs = 2272) |>
  st_buffer(50)

## Added section to completely clean the crash dots marked on Roosevelt Blvd spatially ##

ROO_BUFFER <-
  RMSSEG_NAME_melt |>
  filter(ST_RT_NO %in% c("1", "6001"), between(as.integer(SEG_NO), 160, 381)) |> 
  st_transform(crs = 2272) |>
  st_buffer(50) |>
  summarise(geometry = st_union(geometry)) 

ROO_CRASH <-
  surface_crash |>
  st_join(ROO_BUFFER, left = FALSE, join = st_intersects)

ROO_CRN_BUFFER <- c(ROO_CRASH$CRN)

################################## 2. Join with crash data

RMSSEG_crash <-
  st_join(surface_crash, RMSSEG_agg_geo_buffer, join = st_intersects, left = FALSE)

RMSSEG_crash <-
  RMSSEG_crash |>
  arrange(ST_RT_NO, CRN, STREET_NAME) |>
  distinct(CRN, ST_RT_NO, .keep_all = T) |>
  filter(!CRN %in% ROOSEVELT_CRN) |> # Exclude crashes happened on
  filter(!CRN %in% ROO_CRN_BUFFER)   # sections where cameras were installed on Roosevelt

## Calculate columns for ranking
CRASH_AGG <-
  RMSSEG_crash |>
  group_by(STREET_NAME, ST_RT_NO) |>
  summarize(ALL_CRASH = n(),
            SPD_FATAL_COUNT = sum(FATAL_COUNT[SPEEDING_RELATED == 1]),
            SPD_SERIOUS_INJ_COUNT = sum(SUSP_SERIOUS_INJ_COUNT[SPEEDING_RELATED == 1]),
            SPD_CRASH = sum(SPEEDING_RELATED),
            KSI_CRASH = sum(FATAL_OR_SUSP_SERIOUS_INJ),
            PED_CRASH = sum(PEDESTRIAN),
            PED_FATAL_COUNT = sum(PED_DEATH_COUNT),
            PED_SERIOUS_INJ_COUNT = sum(PED_SUSP_SERIOUS_INJ_COUNT)) |>
  mutate(SPD_KSI_COUNT = SPD_FATAL_COUNT + SPD_SERIOUS_INJ_COUNT,
         PED_KSI_COUNT = PED_FATAL_COUNT + PED_SERIOUS_INJ_COUNT) |>
  select(-SPD_FATAL_COUNT, -SPD_SERIOUS_INJ_COUNT, -PED_FATAL_COUNT, -PED_SERIOUS_INJ_COUNT) 
  
CRASH_AGG <- data.frame(CRASH_AGG)

CRASH_AGG_ROUTE <-
  CRASH_AGG |>
  group_by(ST_RT_NO) |>
  mutate   (ROUTE_SPD_CRASH = sum(SPD_CRASH),
            ROUTE_SPD_KSI_COUNT = sum(SPD_KSI_COUNT),
            ROUTE_PED_CRASH = sum(PED_CRASH),
            ROUTE_PED_KSI_COUNT = sum(PED_KSI_COUNT),
            ROUTE_KSI_CRASHES = sum(KSI_CRASH),
            ROUTE_ALL_CRASHES = sum(ALL_CRASH)) |>
  arrange(desc(ROUTE_SPD_KSI_COUNT), ST_RT_NO) 
  

CRASH_AGG_ROUTE_ST_TOP15 <-
  CRASH_AGG |>
  group_by(ST_RT_NO) |>
  summarize (ROUTE_SPD_CRASH = sum(SPD_CRASH),
            ROUTE_SPD_KSI_COUNT = sum(SPD_KSI_COUNT),
            ROUTE_PED_CRASH = sum(PED_CRASH),
            ROUTE_PED_KSI_COUNT = sum(PED_KSI_COUNT),
            ROUTE_KSI_CRASHES = sum(KSI_CRASH),
            ROUTE_ALL_CRASHES = sum(ALL_CRASH)) |>
  arrange(desc(ROUTE_SPD_KSI_COUNT), ST_RT_NO) |>
  slice_max(ROUTE_SPD_KSI_COUNT, n=15)

top15_ROUTE <- c(CRASH_AGG_ROUTE_ST_TOP15$ST_RT_NO)

CRASH_AGG_ROUTE_TOP15 <- CRASH_AGG_ROUTE |>
  filter(ST_RT_NO %in% top15_ROUTE)

#st_write(CRASH_AGG_ROUTE_TOP15, "SpeedingCrashbyRouteTop15.geojson", append = T)  
#write_csv(st_drop_geometry(CRASH_AGG_ROUTE_TOP15), "SpeedingCrashbyRouteTop15.csv")

#################Find milage, but exclude parallel sections - eg. Roosevelt has four segments at the same section#########

RMSSEG_NAME_melt$SEG_NO <- sub("^0+", "", RMSSEG_NAME_melt$SEG_NO)

# Exclude all SEG_NO with odd numbers - odd numbers are parallel with even numbers
SEG_EVEN_NUM <- RMSSEG_NAME_melt |> 
  filter(ST_RT_NO %in% top15_ROUTE) |>
  filter(!as.numeric(SEG_NO) %% 2 == 1)

# Then there are still segments that are parallel to each other on Roosevelt, mark these ones
DUP_SEG <- SEG_EVEN_NUM |>   
  group_by(STREET_NAME, SEG_NO, ST_RT_NO) %>% 
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>% 
  mutate(is_duplicated = dup_id > 1)

# Remove duplicated sections and aggregate miles by route numbers
REMOVE_DUP <- DUP_SEG |>
  filter(!dup_id == 2) |>
  filter(!(SEG_NO == 150 & STREET_NAME == "ROOSEVELT BL")) |>
  group_by(ST_RT_NO) |>
  summarise(ROUTE_MILES = sum(SEGMENT_MILES))

# Join back with the crash data 
CRASH_AGG_ROUTE_TOP15 <- 
  CRASH_AGG_ROUTE_TOP15 |>
  left_join(st_drop_geometry(REMOVE_DUP), by = "ST_RT_NO")|>
  st_as_sf()

#st_write(CRASH_AGG_ROUTE_TOP15, "SpeedingCrashbyRouteTop15_v3.geojson", append = T)  
#write_csv(st_drop_geometry(CRASH_AGG_ROUTE_TOP15), "SpeedingCrashbyRouteTop15_3.csv")

################# Adding some routes from PennDOT ####################################

PennDOT_ROUTE <- c(73, 3010, 2014, 2008, 3015, 2001)

CRASH_AGG_ROUTE_TOP15_PENNDOT <- CRASH_AGG_ROUTE |>
  filter(ST_RT_NO %in% top15_ROUTE | ST_RT_NO %in% PennDOT_ROUTE)

SEG_EVEN_NUM <- RMSSEG_NAME_melt |> 
  filter(ST_RT_NO %in% top15_ROUTE | ST_RT_NO %in% PennDOT_ROUTE) |>
  filter(!(as.numeric(SEG_NO) %% 2 == 1 & STREET_NAME != "LEVICK ST"))

## Then there are still segments that are parallel to each other on Roosevelt, mark these ones
DUP_SEG <- SEG_EVEN_NUM |>   
  group_by(STREET_NAME, SEG_NO, ST_RT_NO) %>% 
  mutate(num_dups = n(), 
         dup_id = row_number()) %>% 
  ungroup() %>% 
  mutate(is_duplicated = dup_id > 1)

## Remove duplicated sections and aggregate miles by route numbers
### There are two different segements with the same name. i.e. Roosevelt Blvd and SEG_NO = 180
### One needs to be removed

SEG_NO_EXCLUDE <- c(160:179)
SEG_NO_EXCLUDE2 <- c(181:381)

### 
REMOVE_DUP <- DUP_SEG |>
  filter(!dup_id == 2) |>
  filter(!(SEG_NO == 150 & STREET_NAME == "ROOSEVELT BL")) |>
  filter(!(STREET_NAME == "ROOSEVELT BL" & SEG_NO %in% SEG_NO_EXCLUDE)) |>
  filter(!(STREET_NAME == "ROOSEVELT BL" & SEG_NO %in% SEG_NO_EXCLUDE2)) |>
  filter(!(STREET_NAME == "ROOSEVELT BL" & is_duplicated == FALSE)) |> # This is the duplicated one that needs to be removed
  group_by(ST_RT_NO) |>
  summarise(ROUTE_MILES = sum(SEGMENT_MILES))

# Join back with the crash data 
CRASH_AGG_ROUTE_TOP15_PENNDOT <- 
  CRASH_AGG_ROUTE_TOP15_PENNDOT |>
  left_join(st_drop_geometry(REMOVE_DUP), by = "ST_RT_NO")|>
  st_as_sf()

#st_write(CRASH_AGG_ROUTE_TOP15_PENNDOT, "SpeedingCrashbyRouteTop15_v5_(addedPENNDOTRoutes).geojson")  
#write_csv(st_drop_geometry(CRASH_AGG_ROUTE_TOP15_PENNDOT), "SpeedingCrashbyRouteTop15_v5_(addedPENNDOTRoutes).csv")

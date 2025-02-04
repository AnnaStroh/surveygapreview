### Sampling effort analysis for Irish Anglerfish and Megrim Survey (IAMS)
## Author: Anna Stroh
## Last modified: June 14, 2024
### -------------------------------------

library(icesDatras)
library(dplyr)
library(ggplot2)
library(gridExtra) # multiple ggplot maps on one page
library(sf) # all things spatial
library(maps) # for base R type maps
library(mapdata) # map data
library(purrr) # working with vectors and functions to produce lists
library(readxl)
library(lubridate)
library(data.table)
library(reshape)
library(reshape2)
library(stringr)

file_path <- "C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Data"
file_path2 <- "C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Plotting canvases"

# Extract survey data -----------------------------------------------------

years_iams <- getSurveyYearList("IE-IAMS")
years_iams_sub <- years_iams[years_iams < 2023]
hh_iams1 <- list()
for(y in years_iams_sub){ # quarter 1
  print(y)
  tmp1 <- getHHdata(survey = "IE-IAMS", year = y, quarter = 1)
  hh_iams1[[paste(y)]] <- tmp1
  rm(tmp1)
}
df_iams1 <- do.call(rbind, hh_iams1)

years_sub <- years_iams[years_iams > 2016 & years_iams < 2023] 
hh_iams2 <- list()
for(y in years_sub){ # quarter 2
  print(y)
  tmp2 <- getHHdata(survey = "IE-IAMS", year = y, quarter = 2)
  hh_iams2[[paste(y)]] <- tmp2
  rm(tmp2)
}
df_iams2 <- do.call(rbind, hh_iams2)

df_iams <- rbind(df_iams1, df_iams2)

# Data clean-up
head(df_iams)
hh_iams <- df_iams |> 
  select(Year, StNo, HaulNo, 
         StatRec, HaulLat, HaulLong, ShootLat, ShootLong)
row.names(hh_iams) <- NULL

# Calculate haul difference per stratum -----------------------------

# Extract counts of achieved hauls per statistical ICES rectangle
iams <- hh_iams |>
  group_by(Year, StatRec) |>
  summarise(CompletedHauls = n()) |>
  dplyr::rename(fldICESRectangle = StatRec) #allows merging with other dataframe
head(iams)

# MI internal data to merge DATRAS station location to sampling stratum
ie_planned_st <- read_excel(paste0(file_path, "/", "IGFS_IAMS_PlannedStns_20230925.xlsx"), 
                            sheet = "StationData") # only Irish surveys

rec_in_stratum <- ie_planned_st |> # subset and process internal files
  filter(CruiseSeries == "IAMSQ1",
         fldValidityCode == "V") |>
  mutate(Year = year(fldDateTimeShot)) |> 
  select(Year, fldICESRectangle, fldStratum)

# Sum achieved hauls for each survey stratum (observed)
iams_datras <- iams |>
  left_join(rec_in_stratum) |>
  group_by(Year, fldStratum) |>
  summarise(HaulsPerStratum = n()) |>
  tidyr::drop_na() # exclude hauls in ICES recs with no record in stratum reference data

# Extract stratum targets from MI internal files (expected)
iams_targets <- read.csv(paste0(file_path, "/", "target.csv"))

iams_exp <- iams_targets |>
  melt(id.vars = "Name", variable.name = "Year") |>
  mutate(Year = gsub("X", "", Year)) |>
  filter(!row_number() %in% c(1:12)) |>
  tidyr::drop_na() |>
  dplyr::rename(fldStratum = Name,
                Target = value) #previously "ExpHaulN"
head(iams_exp)

# Merge observed and expected hauls + include geometry
iams_exp_obs <- iams_datras |>
  merge(iams_exp) |>
  mutate(DifToMin = HaulsPerStratum - Target,
         PercDev = (DifToMin/Target)*100) |>
  select(Year, fldStratum, 
         HaulsPerStratum, Target, DifToMin, PercDev) |>
  arrange(Year, fldStratum) |>
  dplyr::rename(Name = fldStratum)
head(iams_exp_obs)

iams_strata <- read_sf(paste0(file_path2, "/", "MM_strata_v3a.shp")) |>
  st_transform(2157) # try irenet projection


iams_str <- iams_strata |>
  merge(iams_exp_obs) |>
  select(Year, Name, 
         HaulsPerStratum, Target, DifToMin, PercDev,
         geometry) |>
  dplyr::rename(Stratum = Name) |>
  #st_transform(2157) |> # try irenet projection
  #st_transform(32629) |>
  arrange(Year, Stratum)
head(iams_str)

output_path = "C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Data/Created files/"
st_write(iams_str, paste0(output_path, "surveyeffort_iams_2016-2022.shp"), append = F)

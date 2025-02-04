### Sampling effort analysis for Irish Groundfish Survey (IGFS)
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
library(readxl)
library(lubridate)
library(data.table)
library(stringr)

file_path <- "C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Data"
file_path2 <- "C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Plotting canvases"

# Extract survey data -------------------------------------------------

years_igfs <- getSurveyYearList("IE-IGFS")
years_igfs_sub <- years_igfs[years_igfs < 2023]
hh_igfs_t <- list()
for(y in years_igfs_sub){
  print(y)
  tmp <- getHHdata(survey = "IE-IGFS", year = y, quarter = 4)
  hh_igfs_t[[paste(y)]] <- tmp
  rm(tmp)
}
df_igfs <- do.call(rbind, hh_igfs_t)

# Data clean-up
head(df_igfs)
hh_igfs <- df_igfs |> 
  select(Year, StNo, HaulNo, 
         StatRec, HaulLat, HaulLong, ShootLat, ShootLong)
row.names(hh_igfs) <- NULL

# Calculate haul difference per stratum -----------------------------

# Extract counts of achieved hauls per statistical ICES rectangle
igfs <- hh_igfs |>
  group_by(Year, StatRec) |>
  summarise(CompletedHauls = n()) |>
  dplyr::rename(fldICESRectangle = StatRec) #allows merging with other dataframe
head(igfs)

# MI internal data to merge DATRAS station location to sampling stratum
ie_planned_st <- read_excel(paste0(file_path, "/", "IGFS_IAMS_PlannedStns_20230925_updateddave.xlsx"), 
                            sheet = "StationData") # only Irish surveys

rec_in_stratum <- ie_planned_st |> # subset and process internal files
  filter(CruiseSeries == "IGFSQ4",
         fldValidityCode == "V",
         !grepl("VIIa", fldStratum)) |> #remove Irish Sea strata
  mutate(Year = year(fldDateTimeShot),
         fldStratum = gsub("VIa", "VIa_",
                           gsub("VIIb", "VIIb_",
                                gsub("VIIg", "VIIg_",
                                     gsub("VIIj", "VIIj_", fldStratum))))) |> #regex to match stratum names in shapefile
  select(Year, fldICESRectangle, fldStratum)

# Sum achieved hauls for each survey stratum (observed)
igfs_datras <- igfs |>
  left_join(rec_in_stratum) |>
  group_by(Year, fldStratum) |>
  summarise(HaulsPerStratum = n()) |>
  tidyr::drop_na() |> # exclude hauls in ICES recs with no record in stratum reference data 
  dplyr::rename(Primary = fldStratum)

# Haul (stratum) target in stratum shapefile!
# Merge observed counts with haul targets in shapefile
igfs_strata <- read_sf(paste0(file_path2, "/", "IGFS_Strata_final.shp"))
igfs_strata

# Problem: stratum VIIb_Slope missing in 2003, 2004 and 2011
# Solution: fill missing data with 0 sampling in this stratum for those years
igfs_strata2 <- igfs_strata |>
  filter(Primary != "VIIg_Deep") # no recorded samples for stratum VIIg_Deep for entire dataset - exclude 

# Create dataframe for all combinations of stratum and year
all_str_igfs <- expand.grid(Year = 2003:2022,
                            Primary = unique(igfs_strata2$Primary)) 
igfs_with_miss <- all_str_igfs |>
  merge(igfs_datras, all.x = TRUE, by = c("Year", "Primary")) |>
  mutate(HaulsPerStratum = ifelse(is.na(HaulsPerStratum), 0, HaulsPerStratum))

# Calculate haul difference between completed and target hauls
igfs_exp_obs <- igfs_strata |>
  merge(igfs_with_miss) |>
  mutate(DifToMin = HaulsPerStratum - Stn_Target,
         PercDev = (DifToMin/Stn_Target)*100) |>
  st_transform(2157) |> # irenet projection
  #st_transform(32629) |>
  select(Year, Primary, AreaKm2, 
         HaulsPerStratum, Stn_Target, DifToMin, PercDev,
         geometry) |>
  arrange(Year, Primary)

output_path = "C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Data/Created files/"
st_write(igfs_exp_obs, paste0(output_path, "surveyeffort_igfs_2003-2022.shp"), append = F)


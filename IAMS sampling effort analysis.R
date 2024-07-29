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
ie_planned_st <- read_excel("/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/NEA survey targets/IGFS_IAMS_PlannedStns_20230925.xlsx", 
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
iams_targets <- read.csv("/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/NEA survey targets/target.csv")

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

iams_strata <- read_sf("/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/IGFS and IAMS survey strata shapefiles/MM_strata_v3a.shp")

iams_str <- iams_strata |>
  merge(iams_exp_obs) |>
  select(Year, Name, 
         HaulsPerStratum, Target, DifToMin, PercDev,
         geometry) |>
  dplyr::rename(Stratum = Name) |>
  st_transform(32629) |>
  arrange(Year, Stratum)
head(iams_str)


# Visualisation -----------------------------------------------------------

ie_canvas <- read_sf("/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/ie survey canvas.shp")
ie_canvas_utm <- st_transform(ie_canvas, 32629)

iams1 <- ggplot() + 
  geom_sf(aes(fill = PercDev), data = iams_str, alpha = 0.7, colour = "black", linewidth = 0.15) +
  geom_sf(data = ie_canvas_utm, fill = "olivedrab3", colour = "black", linewidth = 0.08) +
  scale_fill_gradientn("\nRelative \nsampling \neffort (%)", 
                       values=scales::rescale(c(-100, -50, 0, 50, 100)), 
                       colors = c("dodgerblue4", "dodgerblue2", "white", "firebrick1", "firebrick3")) +
  coord_sf(xlim = c(81939.52, 660253.6), ylim = c(5315891, 6428933)) +
  scale_x_continuous(breaks = c(-14, -11, -8), 
                     labels = c("14°W", "11°W", "8°W")) + 
  facet_wrap(~ Year) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8, angle = 40),
        axis.text.y = element_text(size=8),
        axis.title = element_text(size=9),
        strip.text = element_text(size = 9.5, face = "bold"),
        legend.title = element_text(size=10,  face = "bold"), 
        legend.text = element_text(size=9),
        legend.background = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1, 0.1)) +
  xlab("Longitude") +
  ylab("Latitude")

# Average relative sampling effort ----------------------------------------

targets <- iams_str |>
  st_drop_geometry() |>
  group_by(Stratum) |>
  summarise(MeanTarget = mean(Target)) |>
  select(Stratum, MeanTarget) 

iams_mean_str <- iams_str |>
  group_by(Stratum) |>
  summarise(MeanHauls = mean(HaulsPerStratum),
            MeanDif = mean(DifToMin)) |> # stratum mean of observed hauls and haul difference
  merge(targets) |> #merge in targets
  mutate(MeanPercDev = (MeanDif/MeanTarget)*100) |>
  st_transform(32629) |>
  select(Stratum, MeanHauls, MeanTarget, MeanDif, MeanPercDev, geometry)

iams2 <- ggplot() + 
  geom_sf(aes(fill = MeanPercDev), data = iams_mean_str, alpha = 1, colour = "black", linewidth = 0.15) +
  geom_sf(data = ie_canvas_utm, fill = "olivedrab3", colour = "black", linewidth = 0.08) +
  scale_fill_gradient2("\nAverage \nrelative \nsampling \neffort (%)",
                       low = "dodgerblue2",
                       mid = "white",
                       high = "firebrick1",
                       midpoint = 0) +
  coord_sf(xlim = c(81939.52, 660253.6), ylim = c(5315891, 6428933)) +
  scale_x_continuous(breaks = c(-14, -11, -8), 
                     labels = c("14°W", "11°W", "8°W")) + 
  xlab("Longitude") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size=8, angle = 40),
        axis.text.y = element_text(size=8),
        axis.title = element_text(size=9),
        legend.title = element_text(size=10,  face = "bold"), 
        legend.text = element_text(size=9))

# Patchwork IAMS plots ----------------------------------------------------

library(patchwork)
iams1 + iams2 + 
  plot_layout(widths = c(2, 1))


# Invalid hauls -----------------------------------------------------------

upd_survey <- read_excel("C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Data/IGFS_IAMS_PlannedStns_20230925_updateddave.xlsx",
                         sheet = "StationData")
names(upd_survey)

upd_iams <- upd_survey |>
  select(CruiseSeries, fldDateTimeShot, fldValidityCode) |>
  filter(CruiseSeries == 'IAMSQ1', 
         fldValidityCode == 'I') |>
  group_by(CruiseSeries) |>
  summarise(n = n())





### Sampling effort analysis for Int Bottom Trawl Survey (IBTS)
## Author: Anna Stroh
## Last modified: June 16, 2024
### -------------------------------------

library(icesDatras)
library(sf)
library(mapdata)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)


# Manually creating survey manual-dependent extents -----------------------

# BACKGROUND: This needed to be done manually by evaluating publicly available IBTS survey manuals.
# The ICES statistical rectangles listed here in each vector are rectangle that are not covered in the spatial sampling extent by the given manual. 
ns <- read_sf("C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Data/ICES_Statistical_Rectangles_Eco.shp") |>
  filter(Ecoregion %in% c("Greater North Sea")) |>
  #filter(Area_27 %in% c("4.a", "4.b", "4.c", "7.d", 
                        #"3.a.20", "3.a.21", "3.c.22")) |>
  select(ICESNAME, SOUTH, WEST, NORTH, EAST, geometry) |>
  dplyr::rename(StatRec = ICESNAME)

ggplot(data = ns) +
  geom_sf()

ns_df <- rbind( # dataframe as preparation for grid
  data.frame(lat = rep(ns$NORTH, 2), long = c(ns$WEST, ns$EAST), StatRec = rep(ns$StatRec, 2), Area = rep(ns$Area_27, 2)),
  data.frame(lat = rep(ns$SOUTH, 2), long = c(ns$EAST, ns$WEST), StatRec = rep(ns$StatRec, 2), Area = rep(ns$Area_27, 2)))

# Survey manual cycle 1998-1999 (same spatial extent for both quarter 1 nad quarter 3 surveys)
recs1 <- c("52E5", "51E5","50E5","49E5","48E5","47E5","46E5", "29E5", "28E5",
           "52E6", "51E6", "50E6", "49E6", "48E6", "47E6",  "46E6",  "41E6", "40E6", "29E6", "28E6",
           "52E7", "51E7", "50E7", "40E7", "43E7", "49E7", "30E7", "29E7", "28E7",
           "52E8", "30E8", "29E8", "28E8",
           "52E9", "30E9", "29E9", "28E9", "27E9",
           "52F0", "31F0", "30F0", "29F0", "28F0", "27F0",
           "52F1", "31F1", "30F1", "29F1", "28F1",
           "52F2",
           "52F3", "51F3",
           "52F4","51F4","50F4","49F4","48F4","47F4","46F4",
           "52F5","51F5","50F5","49F5","48F5","47F5","46F5","45F5",
           "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","44F6",
           "52F7", "51F7", "50F7", "49F7","48F7", "45F7", "44F7",
           "45F8", "42F8", "41F8", "40F8",
           "46F9", "40F9","39F9","38F9",
           "47G0","48G0", "41G0", "40G0","39G0","38G0","37G0",
           "47G1", "40G1","39G1","38G1", "37G1", "36G1",
           "40G2","39G2","38G2","37G2",
           "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3",
           #now also include the recs that cover the Skagerrak/Kattegat area that need to be removed for 98-99
           "44F8" , "43F8", "45F9", "44F9", "43F9", "46G0", "45G0", "44G0", "43G0", 
           "42G0", "41G0", "45G1", "44G1", "43G1", "42G1", "41G1", "43G2", "42G2", "41G2") #1996-1999
df1_clean_temp <- subset(ns, !StatRec %in% unique(recs1))
df1_clean <- as.data.frame(cbind(Year = c(rep("1998", nrow(df1_clean_temp)), 
                                          rep("1999", nrow(df1_clean_temp))), 
                                 StatRec = rep(df1_clean_temp$StatRec, 2))) #limit to extent of targeted ns timeseries 
clean_1 <- merge(df1_clean, ns_df)
clean_1$Manual <- rep("1998-1999", nrow(clean_1))

# Survey manual cycle 2000-2005
recs1.q1 <- c("52E5", "51E5","50E5","49E5","48E5","47E5","46E5", "29E5", "28E5",
              "52E6", "51E6", "50E6", "49E6", "48E6", "41E6", "40E6", "29E6", "28E6",
              "52E7", "51E7", "50E7", "49E7", "40E7", "43E7", "49E7", "30E7", "29E7", "28E7",
              "52E8", "30E8", "29E8", "28E8",
              "52E9", "30E9", "29E9", "28E9", "27E9",
              "52F0", "31F0", "30F0", "29F0", "28F0", "27F0",
              "52F1", "31F1", "30F1", "29F1", "28F1", "27F1",
              "52F2",
              "52F3", "51F3",
              "52F4","51F4","50F4","49F4","48F4","47F4","46F4",
              "52F5","51F5","50F5","49F5","48F5","47F5","46F5","45F5",
              "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","44F6",
              "52F7", "51F7", "50F7", "49F7","48F7", "45F7", "44F7",
              "45F8", "46F8", "42F8", "41F8", "40F8",
              "46F9", "47F9", "43F9", "42F9", "41F9", "40F9", "39F9","38F9",
              "47G0", "48G0", "40G0","39G0","38G0","37G0",
              "47G1", "46G1", "40G1","39G1","38G1", "37G1", "36G1",
              "40G2","39G2","38G2","37G2",
              "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3") #1999-2005
df1_q1_clean_temp <- subset(ns, !StatRec %in% unique(recs1.q1))
df1_q1_clean <- as.data.frame(cbind(Year = c(rep("2000", nrow(df1_q1_clean_temp)), 
                                             rep("2001", nrow(df1_q1_clean_temp)),
                                             rep("2002", nrow(df1_q1_clean_temp)),
                                             rep("2003", nrow(df1_q1_clean_temp)),
                                             rep("2004", nrow(df1_q1_clean_temp)),
                                             rep("2005", nrow(df1_q1_clean_temp))), 
                                    StatRec = rep(df1_q1_clean_temp$StatRec, 6))) #limit to extent of targeted ns timeseries 
clean_1_q1 <- merge(df1_q1_clean, ns_df)
clean_1_q1$Manual <- rep("2000-2005", nrow(clean_1_q1))

recs1.q3 <- c("52E5", "51E5","50E5", "49E5", "48E5", "47E5", "46E5", "45E5", "29E5", "28E5",
              "52E6", "51E6", "50E6", "49E6", "48E6", "47E6",  "46E6",  "41E6", "40E6", "29E6", "28E6",
              "52E7", "51E7", "50E7", "40E7", "43E7", "49E7", "30E7", "29E7", "28E7",
              "52E8", "39E8", "38E8", "30E8", "29E8", "28E8", "27E8",
              "37E9", "36E9", "30E9", "29E9", "28E9", "27E9",
              "34F0", "32F0", "31F0", "30F0", "29F0", "28F0", "27F0",
              "34F1", "33F1", "31F1", "30F1", "29F1", "28F1",
              "52F2", "31F2",
              "52F3", "51F3", "31F3",
              "52F4", "51F4", "50F4", "49F4", "48F4", "47F4", "46F4", "32F4", "31F4",
              "52F5","51F5","50F5","49F5","48F5","47F5","46F5", "35F5",
              "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","35F6",
              "52F7", "51F7", "50F7", "49F7", "48F7", "45F7", "44F7", "43F7", "35F7",
              "46F8", "45F8", "42F8", "41F8", "40F8", "39F8", "38F8", "37F8", "36F8", "35F8",
              "47F9", "46F9", "42F9", "41F9", "40F9", "39F9", "38F9", "37F9",
              "48G0", "47G0", "42G0", "41G0", "40G0","39G0","38G0","37G0",
              "47G1", "46G1", "40G1","39G1","38G1", "37G1", "36G1",
              "39G2","38G2","37G2",
              "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3") #1999-2005
df1_q3_clean_temp <- subset(ns, !StatRec %in% unique(recs1.q3))
df1_q3_clean <- as.data.frame(cbind(Year = c(rep("2000", nrow(df1_q3_clean_temp)), 
                                             rep("2001", nrow(df1_q3_clean_temp)),
                                             rep("2002", nrow(df1_q3_clean_temp)),
                                             rep("2003", nrow(df1_q3_clean_temp)),
                                             rep("2004", nrow(df1_q3_clean_temp)),
                                             rep("2005", nrow(df1_q3_clean_temp))), 
                                    StatRec = rep(df1_q3_clean_temp$StatRec, 6))) #limit to extent of targeted ns timeseries 
clean_0_q3 <- merge(df1_q3_clean, ns_df)
clean_0_q3$Manual <- rep("2000-2005", nrow(clean_0_q3))

# Survey manual cycle 2006-2012
recs2.q1 <- c("52E5", "51E5", "50E5", "49E5", "48E5", "47E5", "46E5", "29E5", "28E5", 
              "52E6", "51E6", "50E6",  "41E6", "40E6", "29E6", "28E6",
              "52E7", "51E7", "43E7", "40E7", "39E7", "38E7", "30E7", "29E7", "28E7",
              "52E8", "30E8", "29E8", "28E8",
              "52E9", "30E9", "29E9", "28E9", "27E9",
              "52F0", "27F0",
              "52F1", "52F2", 
              "52F3", "51F3",
              "52F4", "51F4", "50F4", "49F4", "48F4", "47F4", "46F4", 
              "52F5", "51F5", "50F5", "49F5", "48F5", "47F5", "46F5", "45F5",
              "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","44F6",
              "52F7", "51F7", "50F7", "49F7","48F7","45F7", "44F7", 
              "48F8", "47F8", "46F8", "45F8",
              "48F9", "47F9", "46F9", "40F9", "39F9", "38F9", "37F9",
              "48G0", "47G0", "40G0", "39G0", "38G0", "37G0",
              "48G1", "47G1", "46G1", "40G1", "39G1", "38G1", "37G1", "36G1",
              "44G2", "40G2", "39G2", "38G2", "37G2", 
              "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3") #2006-2012
df2_q1_clean_temp <- subset(ns, !StatRec %in% unique(recs2.q1))
df2_q1_clean <- as.data.frame(cbind(Year = c(rep("2006", nrow(df2_q1_clean_temp)),
                                             rep("2007", nrow(df2_q1_clean_temp)),
                                             rep("2008", nrow(df2_q1_clean_temp)),
                                             rep("2009", nrow(df2_q1_clean_temp)),
                                             rep("2010", nrow(df2_q1_clean_temp)),
                                             rep("2011", nrow(df2_q1_clean_temp)),
                                             rep("2012", nrow(df2_q1_clean_temp))), 
                                    StatRec = rep(df2_q1_clean_temp$StatRec, 7)))
clean_2_q1 <- merge(df2_q1_clean, ns_df)
clean_2_q1$Manual <- rep("2006-2012", nrow(clean_2_q1))

recs2.q3 <- c("52E5", "51E5", "50E5", "49E5", "48E5", "47E5", "46E5", "45E5", "29E5", "28E5", 
              "52E6", "51E6", "50E6", "29E6", "28E6",
              "52E7", "51E7", "30E7", "29E7", "28E7",
              "52E8", "30E8", "39E8", "38E8", "29E8", "28E8", "27E8",
              "52E9", "30E9", "37E8", "36E8", "30E9", "29E9", "28E9", "27E9",
              "52F0", "35F0", "30F0", "29F0", "28F0", "27F0",
              "52F1", "34F1", "33F1", "31F1", "30F1", "29F1", "28F1", "27F1", 
              "52F2", "31F2", 
              "52F3", "51F3", "31F3",
              "52F4", "51F4", "50F4", "49F4", "48F4", "47F4", "46F4", 
              "52F5", "51F5", "50F5", "49F5", "48F5", "47F5", "46F5", "35F5", "34F5", 
              "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","44F6", "35F6",
              "52F7", "51F7", "50F7", "49F7","48F7","45F7", "44F7", "36F7",
              "45F8", "42F8", "41F8", "40F8", "39F8", "38F8", "37F8", "36F8",
              "46F9", "45F9", "40F9", "39F9",
              "47G0", "42G0", "41G0", "40G0", "39G0", "38G0", "37G0",
              "40G1", "39G1", "38G1", "37G1",
              "39G2", "38G2", "37G2",
              "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3") #2006-2012
df2_q3_clean_temp <- subset(ns, !StatRec %in% unique(recs2.q3))
df2_q3_clean <- as.data.frame(cbind(Year = c(rep("2006", nrow(df2_q3_clean_temp)),
                                             rep("2007", nrow(df2_q3_clean_temp)),
                                             rep("2008", nrow(df2_q3_clean_temp)),
                                             rep("2009", nrow(df2_q3_clean_temp)),
                                             rep("2010", nrow(df2_q3_clean_temp)),
                                             rep("2011", nrow(df2_q3_clean_temp)),
                                             rep("2012", nrow(df2_q3_clean_temp))), 
                                    StatRec = rep(df2_q3_clean_temp$StatRec, 7)))
clean_2_q3 <- merge(df2_q3_clean, ns_df)
clean_2_q3$Manual <- rep("2006-2012", nrow(clean_2_q3))

# Survey manual cycle 2013-2019
recs3.q1 <- c("52E5", "51E5", "50E5", "49E5", "48E5", "47E5", "46E5", "45E5", "29E5", "28E5", 
              "52E6", "51E6", "50E6",
              "52E7", "51E7", "43E7", "40E7",
              "52E8", "30E8", "29E8", "28E8",
              "52E9", "30E9", "29E9", "28E9", "27E9",
              "52F0", "34F0", "33F0", "32F0", "31F0", "27F0",
              "52F1", "34F1", "33F1", "31F1", "30F1", "29F1", "28F1", "27F1", 
              "52F2", 
              "52F3", "51F3", "31F3",
              "52F4", "51F4", "50F4", "49F4", "48F4", "47F4", "46F4", "32F4", "31F4",
              "52F5", "51F5", "50F5", "49F5", "48F5", "47F5", "46F5", "45F5", "35F5", "34F5", 
              "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","44F6", "35F6",
              "52F7", "51F7", "50F7", "49F7","48F7","47F7", "46F7", "45F7", "44F7", "35F7",
              "46F8", "45F8", "42F8", "41F8", "40F8", "36F8", 
              "47F9", "46F9", "43F9", "42F9", "41F9", "40F9", "39F9", "38F9",
              "48G0", "47G0", "40G0", "39G0", "38G0", "37G0", 
              "47G1", "46G1", "40G1", "39G1", "38G1", "37G1", "36G1",
              "40G2", "39G2", "38G2", "37G2",
              "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3") #2013-2019
df3_q1_clean_temp <- subset(ns, !StatRec %in% unique(recs3.q1))
df3_q1_clean <- as.data.frame(cbind(Year = c(rep("2013", nrow(df3_q1_clean_temp)),
                                             rep("2014", nrow(df3_q1_clean_temp)),
                                             rep("2015", nrow(df3_q1_clean_temp)),
                                             rep("2016", nrow(df3_q1_clean_temp)),
                                             rep("2017", nrow(df3_q1_clean_temp)),
                                             rep("2018", nrow(df3_q1_clean_temp)),
                                             rep("2019", nrow(df3_q1_clean_temp))), 
                                    StatRec = rep(df3_q1_clean_temp$StatRec, 7)))
clean_3_q1 <- merge(df3_q1_clean, ns_df)
clean_3_q1$Manual <- rep("2013-2019", nrow(clean_3_q1))

recs3.q3 <- c("52E5", "51E5", "50E5", "49E5", "48E5", "47E5", "46E5", "45E5", "44E5", "29E5", "28E5",
              "52E6", "51E6", "50E6", "41E6", "40E6",
              "52E7", "51E7", "43E7", "40E7",
              "52E8", "30E8", "29E8", "39E8", "38E8", "28E8",
              "52E9", "37E9", "36E9", "30E9", "29E9", "28E9", "27E9",
              "52F0", "34F0", "33F0", "32F0", "31F0", "30F0", "29F0", "28F0", "27F0",
              "52F1", "34F1", "33F1", "31F1", "30F1", "29F1", "28F1",
              "52F2", "31F2", 
              "52F3", "51F3", "31F3",
              "52F4", "51F4", "50F4", "49F4", "48F4", "47F4", "46F4", "32F4", "31F4",
              "52F5", "51F5", "50F5", "49F5", "48F5", "47F5", "46F5", "35F5", "34F5", "33F5",
              "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","44F6", "35F6",
              "52F7", "51F7", "50F7", "49F7","48F7", "45F7", "44F7", "36F7", "35F7",
              "45F8", "42F8", "41F8", "40F8", "39F8", "38F8", "37F8", "36F8", "35F8",
              "47F9", "46F9", "42F9", "41F9", "40F9", "39F9", "38F9", "37F9", "36F9",
              "48G0", "47G0", "42G0", "40G0", "39G0", "38G0", "37G0",
              "47G1", "46G1", "40G1", "39G1", "38G1", "37G1", "36G1",
              "40G2", "39G2", "38G2", "37G2",
              "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3") #2013-2019
df3_q3_clean_temp <- subset(ns, !StatRec %in% unique(recs3.q3))
df3_q3_clean <- as.data.frame(cbind(Year = c(rep("2013", nrow(df3_q3_clean_temp)),
                                             rep("2014", nrow(df3_q3_clean_temp)),
                                             rep("2015", nrow(df3_q3_clean_temp)),
                                             rep("2016", nrow(df3_q3_clean_temp)),
                                             rep("2017", nrow(df3_q3_clean_temp)),
                                             rep("2018", nrow(df3_q3_clean_temp)),
                                             rep("2019", nrow(df3_q3_clean_temp))), 
                                    StatRec = rep(df3_q3_clean_temp$StatRec, 7)))
clean_3_q3 <- merge(df3_q3_clean, ns_df)
clean_3_q3$Manual <- rep("2013-2019", nrow(clean_3_q3))

# Survey manual cycle 2020-2023
recs4.q1 <- c("52E5", "51E5", "50E5", "49E5", "48E5", "47E5", "46E5", "45E5", "44E5", "29E5", "28E5",
              "52E6", "51E6", "50E6", "41E6", "40E6",
              "52E7", "51E7", "43E7", "40E7",
              "52E8", "30E8", "29E8", "38E8", "28E8",
              "36E9", "30E9", "29E9", "28E9", "27E9",
              "34F0", "33F0", "32F0", "31F0", "27F0",
              "28F1",
              "52F2", 
              "52F3", "51F3", "31F3",
              "52F4", "51F4", "50F4", "49F4", "48F4", "47F4", "46F4", "32F4", "31F4",
              "52F5", "51F5", "50F5", "49F5", "48F5", "47F5", "46F5", "45F5", "35F5", "34F5",
              "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","44F6", "35F6",
              "52F7", "51F7", "50F7", "49F7","48F7", "45F7", "44F7", "35F7",
              "46F8", "45F8", "42F8", "41F8", "40F8", "36F8", "35F8",
              "47F9", "46F9", "42F9", "41F9", "40F9", "39F9", "38F9", "37F9", "36F9",
              "48G0", "47G0", "42G0", "40G0", "39G0", "38G0", "37G0",
              "47G1", "46G1", "40G1", "39G1", "38G1", "37G1", "36G1",
              "40G2", "39G2", "38G2", "37G2",
              "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3") # 2020 onwards
df4_q1_clean_temp <- subset(ns, !StatRec %in% unique(recs4.q1))
df4_q1_clean <- as.data.frame(cbind(Year = c(rep("2020", nrow(df4_q1_clean_temp)), 
                                             rep("2021", nrow(df4_q1_clean_temp)), 
                                             rep("2022", nrow(df4_q1_clean_temp)),
                                             rep("2023", nrow(df4_q1_clean_temp))), 
                                    StatRec = rep(df4_q1_clean_temp$StatRec, 4)))
clean_4_q1 <- merge(df4_q1_clean, ns_df)
clean_4_q1$Manual <- rep("2020-2023", nrow(clean_4_q1))

recs4.q3 <- c("52E5", "51E5", "50E5", "49E5", "48E5", "47E5", "46E5", "45E5", "44E5", "29E5", "28E5",
              "52E6", "51E6", "50E6", "41E6", "40E6",
              "52E7", "51E7", "43E7", "40E7",
              "52E8", "30E8", "29E8", "38E8", "28E8",
              "37E9", "36E9", "30E9", "29E9", "28E9", "27E9",
              "34F0", "33F0", "32F0", "31F0", "30F0", "29F0", "28F0", "27F0",
              "34F1", "33F1", "31F1", "30F1", "29F1", "28F1",
              "52F2", "31F2",
              "52F3", "51F3", "31F3",
              "52F4", "51F4", "50F4", "49F4", "48F4", "47F4", "46F4", "32F4", "31F4",
              "52F5", "51F5", "50F5", "49F5", "48F5", "47F5", "46F5", "45F5", "35F5", "34F5", "33F5",
              "52F6", "51F6", "50F6", "49F6","48F6","47F6","46F6", "45F6","44F6", "35F6",
              "52F7", "51F7", "50F7", "49F7","48F7", "45F7", "44F7", "36F7", "35F7",
              "46F8", "45F8", "42F8", "41F8", "40F8", "39F8", "38F8", "37F8", "36F8", "35F8",
              "47F9", "46F9", "42F9", "41F9", "40F9", "39F9", "38F9", "37F9", "36F9",
              "48G0", "47G0", "42G0", "40G0", "39G0", "38G0", "37G0", "36G0",
              "47G1", "46G1", "40G1", "39G1", "38G1", "37G1", "36G1",
              "40G2", "39G2", "38G2", "37G2",
              "44G3", "43G3", "42G3", "41G3", "40G3", "39G3", "38G3", "37G3") # 2020 onwards
df4_q3_clean_temp <- subset(ns, !StatRec %in% unique(recs4.q3))
df4_q3_clean <- as.data.frame(cbind(Year = c(rep("2020", nrow(df4_q3_clean_temp)), 
                                             rep("2021", nrow(df4_q3_clean_temp)), 
                                             rep("2022", nrow(df4_q3_clean_temp)),
                                             rep("2023", nrow(df4_q3_clean_temp))), 
                                    StatRec = rep(df4_q3_clean_temp$StatRec, 4)))
clean_4_q3 <- merge(df4_q3_clean, ns_df)
clean_4_q3$Manual <- rep("2020-2023", nrow(clean_4_q3))

#polygons of recs to be sampled for each year
omitted_q1 <- rbind(clean_1, clean_1_q1, clean_2_q1, clean_3_q1, clean_4_q1) # quarter 1
omitted_q3 <- rbind(clean_1, clean_0_q3, clean_2_q3, clean_3_q3, clean_4_q3) # quarter 3

# now we have two survey quarter-specific dataframes that describe with rectangle were not included
# in the IBTS sampling extent at various manual cycles

# Extract survey data -----------------------------------------------------

years <- getSurveyYearList("NS-IBTS")
years_sub <- years[years > 1997 & years < 2024] # subset to desired range - exclude 2024 for now 

# for quarter 1 data
hh_q1 <- list()
for(y in years_sub){
  print(y)
  tmp <- getHHdata(survey = "NS-IBTS", year = y, quarter = 1)
  hh_q1[[paste(y)]] <- tmp
  rm(tmp)
}
df_q1 <- do.call(rbind, hh_q1)

# for quarter 3 data
hh_q3 <- list()
for(y in years_sub){
  print(y)
  tmp <- getHHdata(survey = "NS-IBTS", year = y, quarter = 3)
  hh_q3[[paste(y)]] <- tmp
  rm(tmp)
}
df_q3 <- do.call(rbind, hh_q3)

# data clean-up q1
#head(df_q1)
hh_q1 <- df_q1 |> 
  filter(HaulVal == "V" & StNo != "-9") |>
  select(Year, StNo, HaulNo, 
         StatRec, HaulLat, HaulLong, ShootLat, ShootLong)
row.names(hh_q1) <- NULL

# data clean-up q3
#head(df_q3)
hh_q3 <- df_q3 |> 
  filter(HaulVal == "V" & StNo != "-9") |>
  select(Year, StNo, HaulNo, 
         StatRec, HaulLat, HaulLong, ShootLat, ShootLong)
row.names(hh_q3) <- NULL

# Calculate haul difference per stratum -----------------------------

#quarter 1
q1_datras <- hh_q1 |>
  group_by(Year, StatRec) |>
  summarise(NoHaulsPerRec = n()) |>
  mutate(DifToMin = NoHaulsPerRec - 2) # 2 minimum hauls per statrec as per sampling protocol

#quarter 3
q3_datras <- hh_q3 |>
  group_by(Year, StatRec) |>
  summarise(NoHaulsPerRec = n()) |>
  mutate(DifToMin = NoHaulsPerRec - 2) # 2 minimum hauls per statrec as per sampling protocol


# Account for unsampled ICES rectangles and calculate haul difference - Quarter 1 --------

# Calculate haul difference for each ICES rectangle over years
q1_with_miss <- omitted_q1 |>
  merge(q1_datras, all.x = TRUE, by = c("Year", "StatRec")) |>
  select(Year, StatRec, Area, NoHaulsPerRec, DifToMin, Manual) |>
  distinct() |>
  mutate(NoHaulsPerRec = ifelse(is.na(NoHaulsPerRec), 0, NoHaulsPerRec),
         DifToMin = NoHaulsPerRec - 2,
         PercDev = (DifToMin/2)*100)
  
path = "/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/Created data files/North Sea/"
q1_with_miss_sf <- ns |> # Make the final dataframe an sf object
  merge(q1_with_miss) |>
  select(Manual, Year, StatRec, Area, 
         NoHaulsPerRec, DifToMin, PercDev, geometry) |>
  arrange(Year, StatRec) |>
  st_write(paste0(path, "surveyeffort_q1_1998-2023.shp"), append = F)


# Calculate haul difference for each ICES rectangle over manual cycle
#q1 <- read_sf("/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/Created data files/North Sea/surveyeffort_q1_1998-2023.shp")

q1_mean <- q1 |>
  group_by(Manual, StatRec) |>
  summarise(MeanNHlsPrR = mean(NHlsPrR),
            MeanDif = mean(DifToMn), 
            MeanPercDev = (MeanDif/2)*100) |>
  st_transform(23032) # UTM 32N (appropriate for centre North Sea)


# Account for unsampled ICES rectangles and calculate haul difference - Quarter 3 --------

q3_with_miss <- omitted_q3 |>
  merge(q3_datras, all.x = TRUE, by = c("Year", "StatRec")) |>
  select(Year, StatRec, Area, NoHaulsPerRec, DifToMin, Manual) |>
  distinct() |>
  mutate(NoHaulsPerRec = ifelse(is.na(NoHaulsPerRec), 0, NoHaulsPerRec),
         DifToMin = NoHaulsPerRec - 2,
         PercDev = (DifToMin/2)*100)

q3_with_miss_sf <- ns |> # Make the final dataframe an sf object
  merge(q3_with_miss) |>
  select(Year, Manual, StatRec, Area, 
         NoHaulsPerRec, DifToMin, PercDev, geometry) |>
  arrange(Year, StatRec) |>
  st_write(paste0(path, "surveyeffort_q3_1998-2023.shp"), append = F)

# Calculate haul difference for each ICES rectangle over manual cycle
#q3 <- read_sf("/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/Created data files/North Sea/surveyeffort_q3_1998-2023.shp")

q3_mean <- q3 |>
  group_by(Manual, StatRec) |>
  summarise(MeanNHlsPrR = mean(NHlsPrR),
            MeanDif = mean(DifToMn), 
            MeanPercDev = (MeanDif/2)*100) |>
  st_transform(23032) # UTM 32N (appropriate for centre North Sea)

# Crop bathymetry to quarter-specific survey extent -----------------------

# DO THIS WITH NEW LAPTOP - TOO INTENSIVE FOR CURRENT LAPTOP #

# operation heavy on memory so read in final survey effort datasets and delete environment

# Load bathymetry
bathymetry <- read_sf("/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/Created data files/North Sea/Bathymetry/bathymetry shallower than 10m.shp") |>
  st_transform(23032)

st_intersection_faster <- function(x,y,...){ # from https://github.com/r-spatial/sf/issues/801
  #faster replacement for st_intersection(x, y,...)
  
  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {y[.,]}
  
  st_intersection(x, y_subset,...)
}

shallow_q1 <- st_intersection_faster(q1, bathymetry)

shallow_q1_sub <- st_intersects(df_ordered_year_sf, bathymetry)
shallow_q1 <- st_intersection(df_ordered_year_sf, bathymetry)


# Visualisation -----------------------------------------------------------

canvas <- read_sf("/Users/katharinaboth/Desktop/PhD/Chapter 1 Survey gaps/Quantitative analysis/North Sea map.shp") |>
  st_transform(23032)

q1_summary <- ggplot() +
  geom_sf(data = q1_mean, aes(fill = MeanPercDev), alpha = 0.7, colour = "black", linewidth = 0.05) +
  geom_sf(data = canvas, fill = "white", colour = "grey50", linewidth = 0.05) +
  geom_sf(data = bathymetry, fill = "black") +
  coord_sf(xlim = c(-346881.9, 771558), ylim = c(5506831, 6914779)) +
  scale_fill_gradientn("\nAverage \nrelative \nsampling \neffort (%)", 
                       values=scales::rescale(c(-100, -50, 0, 50, 100, 150)), 
                       colors = c("dodgerblue4", "dodgerblue2", "white", "firebrick1", "firebrick2", "firebrick3")) +
  facet_wrap(~ Manual) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=8),
        axis.title = element_text(size=9.5),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "none") +
  xlab("Longitude") +
  ylab("Latitude")

q3_summary <- ggplot() +
  geom_sf(data = q3_mean, aes(fill = MeanPercDev), alpha = 0.7, colour = "black", linewidth = 0.05) +
  geom_sf(data = canvas, fill = "white", colour = "grey50", linewidth = 0.05) +
  geom_sf(data = bathymetry, fill = "black") +
  coord_sf(xlim = c(-346881.9, 771558), ylim = c(5506831, 6914779)) +
  scale_fill_gradientn("\nAverage \nrelative \nsampling \neffort (%)", 
                       values=scales::rescale(c(-100, -50, 0, 50, 100, 150)), 
                       colors = c("dodgerblue4", "dodgerblue2", "white", "firebrick1", "firebrick2", "firebrick3")) +
  facet_wrap(~ Manual) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=9.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=8),
        strip.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size=11.5, face = "bold"), 
        legend.text = element_text(size=11)) +
  xlab("Longitude")

# Patchwork plots for publication -----------------------------------------

library(patchwork)
p1 <- q1_summary + q3_summary + 
  plot_layout(guides = "collect")




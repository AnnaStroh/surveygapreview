### Accounting for storminess in IGFS - modelling
## Authors: AS
### Date created: February 1 2024, Last modified: Apr 19 2024
### --------------------

library(mgcv)
library(DHARMa)
library(mgcViz)
library(gratia)
library(patchwork)

library(ggplot2)
library(dplyr)
library(sp)
library(viridis)
library(sf)

# Binomial Mixed Effects Model -----------------------------------------------------------

path = "C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Data/"
model_df <- read.csv(paste0(path, 'gam_model_data.csv')) |>
  select(Year, StationId, Station.Sampled, 
         Primary, Av_ShtLt, Av_ShtLn, 
         DAvWh)



#model_sf <- st_as_sf(model_df, coords = c("Long", "Lat"), crs = 4326)
ggplot(data = model_df, aes(Station.Sampled)) +
  geom_histogram() +
  facet_wrap(~ Year)
  #geom_sf(data = model_sf, aes(colour = as.factor(Station.Sampled)), size = 1) +
  #scale_colour_viridis_d() +
  #facet_wrap(~ Year)
  

# re-order dataset
d <- model_df[order(model_df$StationId, model_df$Year), ]
d$Station.Sampled <- as.factor(d$Station.Sampled)
d$Primary <- as.factor(d$Primary)
d$fYear <- factor(paste0("y", d$Year))
#d$fStationId <- factor(paste0("s", d$StationId)) 
#head(d)
#str(d)

ggplot(data = d, aes(Station.Sampled)) +
  geom_histogram(stat = 'count') +
  facet_wrap(~ Year) +
  labs(title = 'N Stations sampled (1) or not sampled (0) by year')

# GAM
mg <-  gam(Station.Sampled ~ -1 +
             s(DAvWh) + 
             #s(DAvWh, k = 10, m = 1) + 
             Primary +
             #s(Year) +
             s(fYear, bs = "re") + 
             s(Av_ShtLn, Av_ShtLt, bs = "tp") + 
             ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
             #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
             #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
           data = d, family = binomial, 
           select = TRUE, method = "ML")
summary.gam(mg)
plot.gam(mg, residuals = TRUE, all.terms = TRUE)

d2 <- model_df[order(model_df$StationId), ]
d2_sub <- d2[complete.cases(d2), ]
mg0 <- gam(Station.Sampled ~ 1, data = d2_sub, family = binomial, method = "ML")
mg0_summary <- summary.gam(mg0)
logLik.gam(mg0)

# AIC and loglik
AIC(mg, mg0) 
logLik.gam(mg, mg0) #-3307.122 (df=140.9938)
anova.gam(mg, mg0, test = "Chisq")



# Alternative models ------------------------------------------------------

mg2 <-  gam(Station.Sampled ~ -1 + # removes insig spatial term
             s(DAvWh) + 
             #s(DAvWh, k = 10, m = 1) + 
             #Primary +
             #s(Year) +
             s(fYear, bs = "re") + 
             #s(Av_ShtLn, Av_ShtLt, bs = "tp") + 
             ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
           #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
           #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
           data = d, family = binomial, 
           select = TRUE, method = "ML")
summary.gam(mg2)
plot.gam(mg2, residuals = TRUE, all.terms = TRUE)

mg3 <-  gam(Station.Sampled ~ -1 + # reduces to spatial term
              s(DAvWh) + 
              #s(DAvWh, k = 10, m = 1) + 
              Primary +
              #s(Year) +
              s(fYear, bs = "re") + 
              #s(Av_ShtLn, Av_ShtLt, bs = "tp"), 
              ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
            #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
            #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
            data = d, family = binomial, 
            select = TRUE, method = "ML")
summary.gam(mg3)
plot.gam(mg3, residuals = TRUE, all.terms = TRUE)

mg4 <-  gam(Station.Sampled ~ -1 + # reduces to spatiotemporal term
              s(DAvWh) + 
              #s(DAvWh, k = 10, m = 1) + 
              Primary +
              #s(Year) +
              s(fYear, bs = "re") + 
              s(Av_ShtLn, Av_ShtLt, bs = "tp"),
              #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
            #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
            #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
            data = d, family = binomial, 
            select = TRUE, method = "ML")
summary.gam(mg4)
plot.gam(mg4, residuals = TRUE, all.terms = TRUE)


mg5 <-  gam(Station.Sampled ~ -1 + # reduces to wave height
              #s(DAvWh),
              #s(DAvWh, k = 10, m = 1) + 
              Primary +
              #s(Year) +
              s(fYear, bs = "re") + 
              s(Av_ShtLn, Av_ShtLt, bs = "tp") + 
              ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
            #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
            #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
            data = d, family = binomial, 
            select = TRUE, method = "ML")
summary.gam(mg5)
plot.gam(mg5, residuals = TRUE, all.terms = TRUE)

mg6 <-  gam(Station.Sampled ~ -1 + # reduces to temporal term
              s(DAvWh),
            #s(DAvWh, k = 10, m = 1) + 
            Primary +
            #s(Year) +
            #s(fYear, bs = "re"), 
            s(Av_ShtLn, Av_ShtLt, bs = "tp") + 
            ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
            #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
            #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
            data = d, family = binomial, 
            select = TRUE, method = "ML")
summary.gam(mg6)
plot.gam(mg6, residuals = TRUE, all.terms = TRUE)

mg7 <-  gam(Station.Sampled ~ -1 + # reduces to temporal term
            s(DAvWh) +
            #s(DAvWh, k = 10, m = 1) + 
            Primary +
              #s(Year) +
            s(fYear, bs = "re"), 
              #s(Av_ShtLn, Av_ShtLt, bs = "tp") + 
              #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
            #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
            #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
            data = d, family = binomial, 
            select = TRUE, method = "ML")
summary.gam(mg7)
plot.gam(mg7, residuals = TRUE, all.terms = TRUE)



# Testing fit after removing Stratum variable -----------------------------

mg_NoStrata <-  gam(Station.Sampled ~ -1 +
             s(DAvWh) + 
             #s(DAvWh, k = 10, m = 1) + 
             #Primary +
             #s(Year) +
             s(fYear, bs = "re") + 
             s(Av_ShtLn, Av_ShtLt, bs = "tp") + 
             ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
           #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
           #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
           data = d, family = binomial, 
           select = TRUE, method = "ML")
summary.gam(mg_NoStrata)
plot.gam(mg_NoStrata, residuals = TRUE, all.terms = TRUE)

mg_wh <-  gam(Station.Sampled ~ -1 +
                      s(DAvWh),
                      #s(DAvWh, k = 10, m = 1) + 
                      #Primary +
                      #s(Year) +
                      #s(fYear, bs = "re") + 
                      #s(Av_ShtLn, Av_ShtLt, bs = "tp") + 
                      #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
                    #s(Av_ShtLn, Av_ShtLt, bs = "tp", k = 20) + 
                    #ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", k = c(20, 15), d = c(2, 1)),
                    data = d, family = binomial, 
                    select = TRUE, method = "ML")
summary.gam(mg_wh)
plot.gam(mg_NoStrata, residuals = TRUE, all.terms = TRUE)


AIC(mg0, mg, mg_wh, mg_NoStrata) # excluding strata worsens fit

anova.gam(mg, mg_NoStrata, test = "Chisq", freq=TRUE)
anova.gam(mg0, mg_wh, test = "Chisq", freq=TRUE)
anova.gam(mg_wh, mg_NoStrata, test = "Chisq", freq=TRUE)

anova.gam(mg_wh)

# Model checking incl. dispersion and autocorrelation ---------------------

# Model check in DHARMa
simOutput <- simulateResiduals(fittedModel = mg)
plot(simOutput)

#dispersion
#plot(simOutput)

simOutput2 <- recalculateResiduals(simOutput, group = d$fYear) # aggregate over time bins
png("qq_quantiles.png", width = 170, height = 120, units = "mm", res = 400)
plot(simOutput2)
dev.off()
testDispersion(simOutput2) # two-side
testDispersion(simOutput2, alternative = 'greater', plot = TRUE)

d$fYPrimary <- factor(with(d, paste(Year, Primary, sep = ":")))
simOutput2.b <- recalculateResiduals(simOutput, group = d$fYPrimary) # aggregate over time bins
testDispersion(simOutput2.b)  
plot(simOutput2.b)

testOutliers(simOutput2, type = "binomial")

#test for temporal autocorrelation
#testTemporalAutocorrelation(simulationOutput = simOutput2, time = unique(d$fYear))
testTemporalAutocorrelation(simulationOutput = simOutput2, time = unique(d$fYear))

#test for spatial autocorrelation - global test
simOutput3 <- recalculateResiduals(simOutput, group = d$Primary)
groupLocations <- d %>% group_by(Primary) %>% summarise(across(c(Av_ShtLt, Av_ShtLn), mean))
testSpatialAutocorrelation(simOutput3, x = groupLocations$Av_ShtLn, y = groupLocations$Av_ShtLt)

#plotting standardised residuals
d2 <- model_df[order(model_df$StationId), ]
d2_sub <- d2[complete.cases(d2), ]


png("variable_residuals.png", width = 170, height = 150, units = "mm", res = 400)
par(mfrow = c(2, 2))
plotResiduals(simOutput) #against predicted value
plotResiduals(simOutput, form = d2_sub$Year, xlab = "Year", main = NULL, rank = F) #against survey year
plotResiduals(simOutput, as.factor(d2_sub$Primary), xlab = "Sampling stratum", main = NULL) #against sampling stratum
plotResiduals(simOutput, form = d2_sub$DAvWh, xlab = "Daily average wave height (m)", main = NULL, rank = F) #against weekly median wave height
#plotResiduals(simOutput, form = d2_sub$StationId, xlab = "StationId", main = NULL, rank = F) # against stationid
dev.off()

coefs <- coef(mg)
coefs_re <- coefs[grep("s\\(fStationId\\)", names(coefs))]
coefs_re
plot(coefs_re)

coefs <- coef(mg)
coefs_y <- coefs[grep("s\\(fYear\\)", names(coefs))]
coefs_y
plot(coefs_y, main = 'Coefficients of RE Year')

summary(mg)



# Further explore spatial and spatiotemporal variability ------------------

# from https://stat.ethz.ch/pipermail/r-help/2007-October/142743.html
## calculate proportions deviance explained...
(deviance(b1)-deviance(b))/deviance(b0) ## prop explained by s(x2)
(deviance(b2)-deviance(b))/deviance(b0) ## prop explained by s(x1)

# see also https://stackoverflow.com/questions/38516139/gam-r-variance-explained-by-variable
mg_summary <- summary.gam(mg)

dev_stratum <- (deviance(mg_NoStrata)-deviance(mg))/deviance(mg0)
dev_stratum # 3.188845e-07 - very small to be expected as insignificant
dev_stratum * mg_summary$dev.expl # 2.507786e-08

dev_spat <- (deviance(mg3)-deviance(mg))/deviance(mg0)
dev_spat # 3.188845e-07 - very small to be expected as insignificant
dev_spat * mg_summary$dev.expl # 2.507786e-08

dev_spattemp <- (deviance(mg4)-deviance(mg))/deviance(mg0)
dev_spattemp # 0.008888429 
dev_spattemp * mg_summary$dev.expl # 0.0006990079

# how much is driven by wave height and time?
dev_wh <- (deviance(mg5)-deviance(mg))/deviance(mg0)
dev_wh # 0.4038642
dev_wh * mg_summary$dev.expl #0.03176087

dev_time <- (deviance(mg6)-deviance(mg))/deviance(mg0)
dev_time # 0.4467855
dev_time * mg_summary$dev.expl #0.03513631

# wave height and time contributing plenty variance - how marginal is the role of space?

dev_nospace <- (deviance(mg7)-deviance(mg))/deviance(mg0)
dev_nospace # 0.008888861 - space has very marginal effect


# Model visualisation - predictions -----------------------------------------------------

## wave height
nwd <- data.frame(fYear = d$fYear, Year = d$Year, DAvWh = d$DAvWh, Primary = d$Primary, Av_ShtLn = mean(d$Av_ShtLn), Av_ShtLt = mean(d$Av_ShtLt))
p <- predict.gam(mg, 
                 type = "terms",
                 newdata = nwd,
                 se.fit = TRUE)
                 #exclude = c("Primary", "s(fYear)", "s(Av_ShtLn, Av_ShtLt)", "ti(Av_ShtLn, Av_ShtLt,Year)"))
wave_height <- data.frame(response = p$fit[,'s(DAvWh)'],
                          upr = p$fit[,'s(DAvWh)'] + 2 * p$se.fit[,'s(DAvWh)'],
                          lwr = p$fit[,'s(DAvWh)'] - 2 * p$se.fit[,'s(DAvWh)'])
wave_height$DAvWh <- d$DAvWh
wave_height$Station.Sampled <- as.integer(d$Station.Sampled)
#wave_height2 = cbind(wave_height, d)

p1 <- ggplot() +
  geom_ribbon(data = wave_height, aes(x=DAvWh, ymin=lwr, ymax=upr), fill = "#0072B2", alpha=0.25) +
  #geom_smooth(data = wave_height, aes(x=DAvWh, y=response)) +
  geom_line(data = wave_height, aes(x=DAvWh, y=response)) +
  geom_rug(data = wave_height, aes(x=DAvWh, y=Station.Sampled), sides = "b") +
  #geom_rug(data = wave_height, mapping=aes(x=DAvWh, y=Station.Sampled), sides = "b") +
  xlab("Daily average wave height (m)") + ylab("Log odds (y=1)") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black", size = 7, face = "bold.italic"),
        axis.title = element_text(colour = "black", size = 7, face = "bold"),
        plot.margin = margin(0, 0, 0, 0))

## random effects of year
nwd_yr <- data.frame(fYear = unique(d$fYear), Year = unique(d$Year), DAvWh = d$DAvWh[1], Primary = d$Primary[1], Av_ShtLn = mean(d$Av_ShtLn), Av_ShtLt = mean(d$Av_ShtLt))
p_yr <- predict(mg, 
                type = "link",
                newdata = nwd_yr,
                se.fit = TRUE, 
                exclude = c("Primary", "s(DAvWh)", "s(Av_ShtLn, Av_ShtLt)", "ti(Av_ShtLn, Av_ShtLt, Year)"))
re_year <- data.frame(response = p_yr$fit,
                      upr = p_yr$fit + 2 * p_yr$se.fit,
                      lwr = p_yr$fit - 2 * p_yr$se.fit)
re_year$Year <- unique(d$Year)


str(re_year) # year is integer
#re_year$Year <- as.character(re_year$Year)
#re_year$Station.Sampled <- as.character(re_year$Station.Sampled)

p2 <- ggplot() +
  geom_ribbon(data = re_year, aes(x=Year, ymin=lwr, ymax=upr), fill = "#0072B2", alpha=0.25) +
  geom_line(data = re_year, aes(x=Year, y=response)) +
  scale_x_continuous(breaks=2012:2022) +
  #geom_rug(data = re_year, mapping=aes(x=Year, y=Station.Sampled), sides = "b") +
  xlab("Year") + ylab("Log odds (y=1)") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 7, face = "bold.italic", angle = 65, vjust = 0.5),
        axis.text.y = element_text(colour = "black", size = 7, face = "bold.italic"),
        axis.title = element_text(colour = "black", size = 7, face = "bold"),
        plot.margin = margin(0, 0, 0, 0))

## stratum effects
nwd_str <- data.frame(Primary = levels(d$Primary), fYear = d$fYear[1], Year = d$Year[1], DAvWh = d$DAvWh[8], Av_ShtLn = mean(d$Av_ShtLn), Av_ShtLt = mean(d$Av_ShtLt))
p_str <- predict.gam(mg, 
                 type = "link",
                 newdata = nwd_str,
                 se.fit = TRUE, 
                 exclude = c("s(fYear)", "s(DAvWh)", "s(Av_ShtLn, Av_ShtLt)", "ti(Av_ShtLn, Av_ShtLt,Year)"))
eff_stratum <- data.frame(response = p_str$fit,
                          upr = p_str$fit + 2 * p_str$se.fit,
                          lwr = p_str$fit - 2 * p_str$se.fit)
eff_stratum$Primary <- nwd_str$Primary
model_summary <- summary.gam(mg)
eff_stratum$p_val <- as.matrix(model_summary$p.table)[,4] # extract p values
eff_stratum$p_val_sig <- c("***", "**", "*", "")[findInterval(eff_stratum$p_val, c(0.001, 0.01, 0.05)) + 1] # add significance levels for visualisation

p3 <- ggplot(eff_stratum) +
  geom_point(aes(x = Primary, y = response)) + 
  geom_segment(aes(x = Primary, y = lwr, xend = Primary, yend = upr)) +
  geom_text(aes(x = Primary, y = upr, label = p_val_sig), vjust = 0.4, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 1.1, hjust = 1, angle = 65, colour = "black", size = 7, face = "bold.italic"),
        axis.text.y = element_text(colour = "black", size = 7, face = "bold.italic"),
        axis.title = element_text(colour = "black", size = 7, face = "bold"), 
        plot.margin = margin(0, 0, 0, 0)) +
  xlab("Stratum") + ylab("Log odds (y=1)")


## spatial effects
#nwd_spat <- expand.grid(Long = seq(min(d$Long), max(d$Long), length = 100),
                      #Lat = seq(min(d$Lat), max(d$Lat), length = 100),
                      #fYear = d$fYear[6], Year = d$Year[1],
                      #Weekly.Median = 4, 
                      #Primary = levels(d$Primary)[2]) 

#p_spat <- predict(mg, newdata = nwd_spat, se.fit = TRUE, exclude = "Primary")

#nwd_spat$response <- plogis(p_spat$fit)
#nwd_spat$lwr <- plogis(p_spat$fit - 2 * p_spat$se.fit)
#nwd_spat$upr <- plogis(p_spat$fit + 2 * p_spat$se.fit)

#hpts <- chull(d[, c("Long", "Lat")])
#hpts <- c(hpts, hpts[1])
#plot(d[, c("Long", "Lat")])
#poly <- d[hpts, c("Long", "Lat")]

#idx <- point.in.polygon(point.x = nwd_spat$Long, point.y = nwd_spat$Lat, 
                        pol.x = poly$Long, pol.y = poly$Lat)
#nwd_spat <- nwd_spat[as.logical(idx),]

#world <- map_data("world")
#ggplot(nwd_spat, aes(x = Long, y = Lat)) +
  #geom_raster(aes(fill = response)) +
  #geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "lightblue") +
  #coord_cartesian(xlim = c(-13, -3), ylim = c(50, 57)) +
  #scale_fill_viridis()

## spatiotemporal effects
nwd_spattemp <- expand.grid(Av_ShtLn = seq(min(d$Av_ShtLn), max(d$Av_ShtLn), length = 100),
                            Av_ShtLt = seq(min(d$Av_ShtLt), max(d$Av_ShtLt), length = 100),
                            Year = 2012:2022,
                            Primary = levels(d$Primary))
p_spattemp <- predict.gam(mg, 
                      type ="link",
                      nwd_spattemp,
                      se.fit = TRUE, 
                      exclude = c("Primary", "s(fStationId)", "s(fYear)", "s(DAvWh)", "s(Av_ShtLn,Av_ShtLt)"),
                      newdata.guaranteed = TRUE) 

nwd_spattemp$response <- p_spattemp$fit
nwd_spattemp$lwr <- p_spattemp$fit - 2 * p_spattemp$se.fit
nwd_spattemp$upr <- p_spattemp$fit + 2 * p_spattemp$se.fit

hpts <- chull(d[, c("Av_ShtLn", "Av_ShtLt")])
hpts <- c(hpts, hpts[1])
plot(d[, c("Av_ShtLn", "Av_ShtLt")])
poly <- d[hpts, c("Av_ShtLn", "Av_ShtLt")]

idx_spattemp <- point.in.polygon(point.x = nwd_spattemp$Av_ShtLn, point.y = nwd_spattemp$Av_ShtLt, 
                        pol.x = poly$Av_ShtLn, pol.y = poly$Av_ShtLt)
nwd_spattemp <- nwd_spattemp[as.logical(idx_spattemp),]

world <- map_data("world")
ggplot(nwd_spattemp, aes(x = Av_ShtLn, y = Av_ShtLt)) +
  geom_raster(aes(fill = response)) +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = "lightblue") +
  coord_cartesian(xlim = c(-13, -3), ylim = c(50, 57)) +
  facet_wrap(~ Year) +
  scale_fill_viridis() 


### predictions are not accurate to the actual survey extent - clip to survey stratum
#partition into two dfs to increase efficiency of st_intersection
strata <- read_sf("C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Plotting canvases/IGFS_Strata_final.shp")

nwd_spattemp_sf <- st_as_sf(nwd_spattemp, coords = c("Av_ShtLn", "Av_ShtLt"), crs = 4326)
sf_use_s2(FALSE) # troubleshooting problems with st_join
points_in_pol <- st_join(nwd_spattemp_sf, strata) |>
  filter(!is.na(Primary.y)) |> # keep only points within survey extent
  select(Year, 
         response, lwr, upr,
         Primary.y, geometry) |>
  dplyr::rename(Stratum = Primary.y) |>
  st_transform(2157)
head(points_in_pol)


#plot clipped predictions over survey strata
canvas <- read_sf('C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Plotting canvases/ie survey canvas.shp') |>
  st_transform(2157)

strata_transformed <- st_transform(strata, 2157)

p4 <- ggplot() +
  geom_sf(data = points_in_pol, aes(colour = response), size = 0.7) +
  geom_sf(data = strata_transformed, fill = NA, colour = "white") +
  geom_sf(data = canvas, fill = "grey40") +
  #coord_sf(xlim = c(-13, -5), ylim = c(49.8, 56.8)) +
  scale_x_continuous(breaks = c(-12, -10, -8, -6), 
                     labels = c("12°W", "10°W", "8°W", "6°W")) +
  scale_colour_viridis() +
  guides(colour = guide_colorbar(barwidth = 0.3,
                               #height = 0.5,
                               #barheight = unit(1.5, "cm"),
                               frame.colour = "grey65", 
                               frame.linewidth = 0.2)) +
  labs(colour = "Log odds \n(y=1)") +
  facet_wrap(~ Year) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, vjust = 0.7, angle = 40),
        axis.text.y = element_text(size = 6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey65"),
        strip.text = element_text(size=7, face = "bold", margin = margin(1, 1, 1, 1)),
        legend.box.spacing = unit(4, "pt"),
        legend.title = element_text(size=7,  face = "bold"),
        legend.text = element_text(size=7, face = "bold"), 
        plot.margin = margin(0, 0, 0, 0))

# Patchwork plots together ------------------------------------------------

library(patchwork)
library(grid)
library(gtable)

gtable_plot1 <- ggplotGrob(p1) # make facet plot a gtable object
gtable_plot1$widths <- gtable_plot1$widths * 0.95 # remove white space (padding) around facet
gtable_plot1$heights <- gtable_plot1$heights * 0.95

gtable_plot2 <- ggplotGrob(p2) 
gtable_plot2$widths <- gtable_plot2$widths * 0.95 
gtable_plot2$heights <- gtable_plot2$heights * 0.95

gtable_plot3 <- ggplotGrob(p3) 
gtable_plot3$widths <- gtable_plot3$widths * 0.95 
gtable_plot3$heights <- gtable_plot3$heights * 0.95

gtable_plot4 <- ggplotGrob(p4)
gtable_plot4$widths <- gtable_plot4$widths * 0.95 
gtable_plot4$heights <- gtable_plot4$heights * 0.95

design = "
AAA
BBC
BBD
"

patch <- wrap_elements(gtable_plot1) + wrap_elements(gtable_plot4) + 
  wrap_elements(gtable_plot2) + wrap_elements(gtable_plot3) +
  plot_layout(design = design) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 7))
print(patch) # this one for publication

ggsave("gam_modelplot_adj.png", patch, 
       dpi = 800, width = 170, height = 175, units = "mm")

# CI for stratum effects --------------------------------------------------

model_summary <- summary.gam(mg)
para_terms <- model_summary$p.table
para_terms_m <- as.matrix(para_terms)
est_sd <- para_terms_m[, 1:2]
est <- para_terms_m[, 1]
sd <- para_terms_m[, 2]

ci <- function(b, e) {
  margin <- 2 * e
  lwr <- (b-margin)
  upr <- (b+margin)
  print(c(lwr, upr))
}

ci_para <- as.data.frame(mapply(ci, est, sd, SIMPLIFY = TRUE))
ci_para$Stratum <- rownames(est_sd)




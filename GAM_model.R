### 
## Spatiotemporal GAM for time, space and storminess on IGFS sampling probability
## Author: AS
### --------------------

library(mgcv)
library(DHARMa)
library(ggplot2)
library(viridis)
library(dplyr)
library(sp)
library(sf)

# Binomial Mixed Effects Model -----------------------------------------------------------

model_df <- read.csv('gam_model_data.csv')) |>
  select(Year, StationId, Station.Sampled, 
         Primary, Av_ShtLt, Av_ShtLn, 
         DAvWh)

# re-order dataset
d <- model_df[order(model_df$StationId, model_df$Year), ]
d$Station.Sampled <- as.factor(d$Station.Sampled)
d$Primary <- as.factor(d$Primary)
d$fYear <- factor(paste0("y", d$Year))

# GAM
mg <-  gam(Station.Sampled ~ -1 +
             s(DAvWh) + 
             Primary +
             s(fYear, bs = "re") + 
             s(Av_ShtLn, Av_ShtLt, bs = "tp") + 
             ti(Av_ShtLn, Av_ShtLt, Year, bs = "tp", d = c(2, 1)),
           data = d, family = binomial, 
           select = TRUE, method = "ML")
summary.gam(mg)
plot.gam(mg, residuals = TRUE, all.terms = TRUE)

# Null model
d2 <- model_df[order(model_df$StationId), ]
d2_sub <- d2[complete.cases(d2), ]
mg0 <- gam(Station.Sampled ~ 1, data = d2_sub, family = binomial, method = "REML")
summary.gam(mg0)
logLik.gam(mg0)

# Model checking incl. dispersion and autocorrelation ---------------------

# Model check in DHARMa
simOutput <- simulateResiduals(fittedModel = mg)
plot(simOutput)

simOutput2 <- recalculateResiduals(simOutput, group = d$fYear) # aggregate over time bins
plot(simOutput2)
testDispersion(simOutput2)

d$fYPrimary <- factor(with(d, paste(Year, Primary, sep = ":")))
simOutput2.b <- recalculateResiduals(simOutput, group = d$fYPrimary) # aggregate over time bins
testDispersion(simOutput2.b)  
plot(simOutput2.b)

testOutliers(simOutput2, type = "binomial")

#test for temporal autocorrelation
testTemporalAutocorrelation(simulationOutput = simOutput2, time = unique(d$fYear))

#test for spatial autocorrelation - global test
simOutput3 <- recalculateResiduals(simOutput, group = d$Primary)
groupLocations <- d %>% group_by(Primary) %>% summarise(across(c(Av_ShtLt, Av_ShtLn), mean))
testSpatialAutocorrelation(simOutput3, x = groupLocations$Av_ShtLn, y = groupLocations$Av_ShtLt)

#plotting standardised residuals
d2 <- model_df[order(model_df$StationId), ]
d2_sub <- d2[complete.cases(d2), ]

par(mfrow = c(2, 2))
plotResiduals(simOutput) #against predicted value
plotResiduals(simOutput, form = d2_sub$Year, xlab = "Year", main = NULL, rank = F) #against survey year
plotResiduals(simOutput, as.factor(d2_sub$Primary), xlab = "Sampling stratum", main = NULL) #against sampling stratum
plotResiduals(simOutput, form = d2_sub$DAvWh, xlab = "Daily average wave height (m)", main = NULL, rank = F) #against weekly median wave height
plotResiduals(simOutput, form = d2_sub$StationId, xlab = "StationId", main = NULL, rank = F) # against stationid
 
#check fit
coefs <- coef(mg)
coefs_re <- coefs[grep("s\\(fStationId\\)", names(coefs))]
coefs_re
plot(coefs_re)

coefs <- coef(mg)
coefs_y <- coefs[grep("s\\(fYear\\)", names(coefs))]
coefs_y
plot(coefs_y, main = 'Coefficients of RE Year')


# Model visualisation - predictions -----------------------------------------------------

## wave height
nwd <- data.frame(fYear = d$fYear, Year = d$Year, DAvWh = d$DAvWh, Primary = d$Primary, Av_ShtLn = mean(d$Av_ShtLn), Av_ShtLt = mean(d$Av_ShtLt))
p <- predict.gam(mg, 
                 type = "terms",
                 newdata = nwd,
                 se.fit = TRUE)
wave_height <- data.frame(response = p$fit[,'s(DAvWh)'],
                          upr = p$fit[,'s(DAvWh)'] + 2 * p$se.fit[,'s(DAvWh)'],
                          lwr = p$fit[,'s(DAvWh)'] - 2 * p$se.fit[,'s(DAvWh)'])
wave_height$DAvWh <- d$DAvWh
wave_height$Station.Sampled <- as.integer(d$Station.Sampled)

p1 <- ggplot() +
  geom_ribbon(data = wave_height, aes(x=DAvWh, ymin=lwr, ymax=upr), fill = "#0072B2", alpha=0.25) +
  #geom_smooth(data = wave_height, aes(x=DAvWh, y=response)) +
  geom_line(data = wave_height, aes(x=DAvWh, y=response)) +
  geom_rug(data = wave_height, aes(x=DAvWh, y=Station.Sampled), sides = "b") +
  #geom_rug(data = wave_height, mapping=aes(x=DAvWh, y=Station.Sampled), sides = "b") +
  xlab("Daily average wave height (m)") + ylab("Log odds (y=1)") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black", size = 11, face = "bold.italic"),
        axis.title = element_text(colour = "black", size = 12, face = "bold"))

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

re_year$Year <- as.character(re_year$Year)

p2 <- ggplot() +
  geom_ribbon(data = re_year, aes(x=Year, ymin=lwr, ymax=upr), fill = "#0072B2", alpha=0.25) +
  geom_line(data = re_year, aes(x=Year, y=response)) +
  scale_x_continuous(breaks=2012:2022) +
  #geom_rug(data = re_year, mapping=aes(x=Year, y=Station.Sampled), sides = "b") +
  xlab("Year") + ylab("Log odds (y=1)") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black", size = 11, face = "bold.italic"),
        axis.title = element_text(colour = "black", size = 12, face = "bold"))

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
  geom_text(aes(x = Primary, y = upr, label = p_val_sig), vjust = 0.15, size = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 65, colour = "black", size = 11, face = "bold.italic"),
        axis.text.y = element_text(colour = "black", size = 11, face = "bold"),
        axis.title = element_text(colour = "black", size = 12, face = "bold")) +
  xlab("Stratum") + ylab("Log odds (y=1)")

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


### predictions are not accurate to the actual survey extent - clip to survey strata
strata <- read_sf("IGFS_Strata_final.shp")

nwd_spattemp_sf <- st_as_sf(nwd_spattemp, coords = c("Av_ShtLn", "Av_ShtLt"), crs = 4326)
sf_use_s2(FALSE) # troubleshooting problems with st_join
points_in_pol <- st_join(nwd_spattemp_sf, strata) |>
  filter(!is.na(Primary.y)) |> # keep only points within survey extent
  select(Year, 
         response, lwr, upr,
         Primary.y, geometry) %>%
  rename(Stratum = Primary.y)
head(points_in_pol)


#plot clipped predictions over survey strata
canvas <- read_sf('ie survey canvas.shp')

p4 <- ggplot() +
  geom_sf(data = points_in_pol, aes(colour = response), size = 0.7) +
  geom_sf(data = strata, fill = NA, colour = "white") +
  geom_sf(data = canvas, fill = "grey40") +
  #coord_sf(xlim = c(-13, -5), ylim = c(49.8, 56.8)) +
  scale_x_continuous(breaks = c(-12, -10, -8, -6), 
                     labels = c("12°W", "10°W", "8°W", "6°W")) +
  scale_colour_viridis() +
  labs(colour = "Log odds (y=1)") +
  facet_wrap(~ Year) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.7, angle = 40),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey65"),
        strip.text = element_text(size=10, face = "bold"),
        legend.title = element_text(size=11,  face = "bold"),
        legend.text = element_text(size=10, face = "bold"))




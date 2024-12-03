###
## Visualise and summarise recorded survey data gaps
## Author: Anna Stroh
## Date: July 2024
###


library(dplyr)
library(ggplot2)
#library(viridis)
#library(viridisLite)


# Read and clean file ------------------------------------------------------------


record <- read.csv("DisruptionRecord_WorkFile.csv")
names(record)
str(record)

unique(record$DisruptorCat)
record$DisruptorCat <- gsub(2, "Spatial - short-term", record$DisruptorCat)
record$DisruptorCat <- gsub(1, "Spatial - long-term", record$DisruptorCat)
record$DisruptorCat <- gsub(3, "Random spatial processes", record$DisruptorCat)

# Remove NAs for category since the are representative of a lack of record of survey disruption
record <- record[!is.na(record$DisruptorCat), ] 

unique(record$Disruptor)
record$Disruptor <- gsub("gear", "vessel/gear", record$Disruptor)
record$Disruptor <- gsub("oil/gas", "gas/oil", record$Disruptor)
record$Disruptor <- gsub("legal", "legalities", record$Disruptor)




# Plotting ----------------------------------------------------------------

# histogram of survey coverage

ggplot(data = record, aes(SurveyCov)) +
  geom_histogram() # swewed

median(record$SurveyCov, na.rm = TRUE) #90.5


#### CATEGORY AND DISRUPTOR FREQUENCY 
cat <- record |>
  ggplot(aes(DisruptorCat)) +
  geom_histogram(stat = 'count')
cat # as expected not hugely helpful

dis_longterm <- record |>
  filter(DisruptorCat == 'Spatial - long-term') |>
  ggplot(aes(Disruptor)) +
  geom_histogram(stat = 'count')
dis_longterm

dis_random <- record |>
  filter(DisruptorCat == 'Random spatial processes', 
         Disruptor != 'military', 
         Disruptor != 'financial') |>
  ggplot(aes(Disruptor)) +
  geom_histogram(stat = 'count')
dis_random

dis_spat <- record |>
  filter(DisruptorCat == 'Spatial - short-term',
         Disruptor != 'military') |>
  ggplot(aes(Disruptor)) +
  geom_histogram(stat = 'count')
dis_spat

## quantify frequency
record_summary <- record |>
  select(SurveyYear, Survey, DisruptorCat, Disruptor) |>
  distinct() |>
  mutate(DisruptorCat = as.factor(DisruptorCat),
         Disruptor = as.factor(Disruptor)) |>
  group_by(DisruptorCat, Disruptor) |>
  summarise(Freq = n())
record_summary


#### STATIONS AFFECTED

short_term <- c('Random Spatial Processes', 'Spatial - short-term')

# number of stations affected (if specified in report)

no_st_rec <- c('', 'administration', 'pandemic', 'financial',
               'military', 'NA', 'staff')
#unique(sta_aff$Disruptor)

level_order <- c('vessel/gear', 'weather', 'fisheries',
                 'seabed obstacle', 'marine conservation', 'legalities', 
                 'administration', 'gas/oil', 'windfarm') 

#single_season <- c('weather', 'vessel/gear', 'fisheries') 

sta_aff <- record |>
  filter(!Disruptor %in% no_st_rec, # filter out entries without station record
         !is.na(NStaAff)) |>
  mutate(NStaAff = as.integer(NStaAff),
         DisruptorCat = as.factor(DisruptorCat),
         Disruptor = as.factor(Disruptor)) |>
  group_by(DisruptorCat, Disruptor) |>
  summarise(SumStaAff = sum(NStaAff))
sta_aff  

standalone <- ggplot(data = sta_aff, aes(factor(Disruptor, level = level_order), SumStaAff, fill = DisruptorCat)) +
  geom_col() +
  scale_fill_manual(values = c("Spatial - long-term" = '#95D840FF',
                               "Spatial - short-term" = '#FDE725FF',
                               "Random spatial processes" = '#404788FF')) +
  xlab('Survey disruptor') + ylab('N Stations affected') + 
  guides(fill = guide_legend(label.hjust = -0.5)) +
  labs(fill='Disruption\ncategory') +
  scale_y_continuous(limits = c(0,145), expand = c(0, 0)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 6, angle = 40, vjust = 0.89, hjust = 0.8),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 6),
        legend.title = element_text(size = 5.5, face = "bold"), 
        legend.text = element_text(size = 5, margin = margin(l = -0.16, r = 0.16, unit = "mm")),
        legend.key.size = unit(2.2, "mm"), 
        legend.position = "top")
standalone
ggsave("n station affected simple.png", standalone, 
       width = 85, height = 90, units = "mm", dpi = 400)










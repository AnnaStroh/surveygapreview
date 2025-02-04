### Plotting of Irish surveys for publication
## Author: Anna Stroh
## Last modified: Oct 23, 2024
### -------------------------------------

library(sf)
library(ggplot2)

output_path <- "C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Data/Created files"

### Load data
iams_data <- read_sf(paste0(output_path, "/", "surveyeffort_iams_2016-2022.shp"))
iams_aggr <- read_sf(paste0(output_path, "/", "surveyeffort_iams_aggr_2016-2022.shp"))

igfs_data <- read_sf(paste0(output_path, "/", "surveyeffort_igfs_2003-2022.shp"))
igfs_aggr <- read_sf(paste0(output_path, "/", "surveyeffort_igfs_aggr_2003-2022.shp"))
names(igfs_aggr)

### Graphs

# IAMS

ie_canvas <- read_sf("C:/Users/astroh/OneDrive - Marine Institute/Chapter 1/Plotting canvases/ie survey canvas.shp")
ie_canvas_utm <- st_transform(ie_canvas, 2157)
#ie_canvas_utm <- st_transform(ie_canvas, 32629)

iams1 <- ggplot() + 
  geom_sf(aes(fill = PercDev), data = iams_data, alpha = 0.7, colour = "black", linewidth = 0.15) +
  geom_sf(data = ie_canvas_utm, fill = "gray40", colour = "black", linewidth = 0.08) +
  scale_fill_gradientn("\nRelative \nsampling \neffort (%)", 
                       values=scales::rescale(c(-100, -50, 0, 50, 100)), 
                       colors = c("#440154", "#481567", "white", "#95D840", "#FDE725")) +
  #scale_fill_gradientn("\nRelative \nsampling \neffort (%)", 
  #  values=scales::rescale(c(-100, -50, 0, 50, 100)), 
  #  colors = c("dodgerblue4", "dodgerblue2", "white", "firebrick1", "firebrick3")) + 
  guides(fill = guide_colorbar(barwidth = 0.3, 
                               #barheight = 2.5,
                               frame.colour = "grey65", 
                               frame.linewidth = 0.05)) +
  #coord_sf(xlim = c(127994.7, 698140), ylim = c(142761.2, 865527.9)) + # sudden problem: extent not correct
  #coord_sf(xlim = c(81939.52, 660253.6), ylim = c(5315891, 6428933)) +
  scale_x_continuous(breaks = c(-14, -12, -10, -8), 
                     labels = c("14°W", "12°W", "10°W", "8°W")) + 
  facet_wrap(~ Year) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey65"),
        axis.text.x = element_text(size=5, angle = 40),
        axis.text.y = element_text(size=5),
        axis.title.x = element_blank(),
        axis.title = element_text(size=6),
        strip.text = element_text(size = 6, face = "bold", margin = margin(1, 1, 1, 1)),
        legend.box.spacing = unit(4, "pt"), 
        legend.title = element_text(size=7,  face = "bold"), 
        legend.text = element_text(size=7),
        legend.background = element_blank(),
        #legend.position = c(1, 0),
        #legend.justification = c(1.8, -0.25),
        plot.margin = margin(0, 0, 0, 0)) +
  xlab("Longitude") +
  ylab("Latitude")
print(iams1)

iams2 <- ggplot() + 
  geom_sf(aes(fill = MnPrcDv), data = iams_aggr, alpha = 1, colour = "black", linewidth = 0.15) +
  geom_sf(data = ie_canvas_utm, fill = "gray40", colour = "black", linewidth = 0.08) +
  scale_fill_gradient2("\nAverage \nrelative \nsampling \neffort (%)",
                       low = "#440154",
                       mid = "white",
                       high = "#FDE725",
                       midpoint = 0) +
  guides(fill = guide_colorbar(barwidth = 0.3,
                               frame.colour = "grey65", 
                               frame.linewidth = 0.2)) +
  coord_sf(xlim = c(112939.4, 698140), ylim = c(137461.7, 1252213)) +
  #coord_sf(xlim = c(81939.52, 660253.6), ylim = c(5315891, 6428933)) +
  scale_x_continuous(breaks = c(-14, -12, -10, -8), 
                     labels = c("14°W", "12°W", "10°W", "8°W")) +
  xlab("Longitude") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey65"),
        axis.text.x = element_text(size=5, angle = 40),
        axis.text.y = element_text(size=5),
        axis.title = element_blank(),
        legend.box.spacing = unit(4, "pt"), 
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_text(size=7,  face = "bold"), 
        legend.text = element_text(size=7),
        plot.margin = margin(0, 0, 0, 0))
print(iams2)

# IGFS

igfs1 <- ggplot() + 
  geom_sf(aes(fill = PercDev), data = igfs_data, alpha = 0.7, colour = "black", linewidth = 0.15) +
  geom_sf(data = ie_canvas_utm, fill = "gray40", colour = "black", linewidth = 0.08) +
  scale_fill_gradientn("\nRelative \nsampling \neffort (%)", 
                       values=scales::rescale(c(-100, -50, 0, 50, 100, 150)), 
                       colors = c("#440154", "#481567", "white", "#95D840", "#B6DE2B","#FDE725")) +
  guides(fill = guide_colorbar(barwidth = 0.3,
                               #height = 0.5, 
                               #barheight = unit(1.5, "cm"),
                               frame.colour = "grey65", 
                               frame.linewidth = 0.2)) +
  #scale_fill_gradientn("\nRelative \nsampling \neffort (%)", 
  #                    values=scales::rescale(c(-100, -50, 0, 50, 100, 150)), 
  #                   colors = c("dodgerblue4", "dodgerblue2", "white", "firebrick1", "firebrick2", "firebrick3")) +
  facet_wrap(~ Year) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey65"),
        axis.text.x = element_text(size=5, angle = 40),
        axis.text.y = element_text(size=5),
        axis.title = element_text(size=6),
        strip.text = element_text(size = 6, face = "bold", margin = margin(1, 1, 1, 1)),
        legend.box.spacing = unit(4, "pt"), 
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_text(size=7,  face = "bold"), 
        legend.text = element_text(size=7, margin = margin(l = 1, r = 1, unit = "mm")),
        legend.background = element_blank(),
        plot.margin = margin(0, 0, 0, 0)) +
  xlab("Longitude") +
  ylab("Latitude")
print(igfs1)


igfs2 <- ggplot() + 
  geom_sf(aes(fill = MnPrcDv), data = igfs_aggr, alpha = 0.7, colour = "black", linewidth = 0.15) +
  geom_sf(data = ie_canvas_utm, fill = "gray40", colour = "black", linewidth = 0.08) +
  scale_fill_gradientn("\nAverage \nrelative \nsampling \neffort (%)", 
                       values=scales::rescale(c(-100, -50, 0, 18)), 
                       colors = c("#440154", "#481567", "white", "#FDE725")) +
  guides(fill = guide_colorbar(barwidth = 0.3,
                               #height = 0.5,
                               #barheight = unit(1.5, "cm"),
                               frame.colour = "grey65", 
                               frame.linewidth = 0.2)) +
  # scale_fill_gradientn("\nAverage \nrelative \nsampling \neffort (%)", 
  #                     values=scales::rescale(c(-100, -50, 0, 18)), 
  #                    colors = c("dodgerblue4", "dodgerblue2", "white", "firebrick1")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey65"),
        axis.text.x = element_text(size=5, angle = 40),
        axis.text.y = element_text(size=5),
        axis.title = element_text(size=6),
        legend.box.spacing = unit(4, "pt"), 
        legend.margin = margin(0, 0, 0, 0),
        legend.title = element_text(size=7,  face = "bold"), 
        legend.text = element_text(size=7, margin = margin(l = 1, r = 1, unit = "mm")),
        legend.background = element_blank(),
        plot.margin = margin(0, 0, 0, 0)) +
  xlab("Longitude")
print(igfs2)


# Patchwork together

library(patchwork)
library(grid)
library(gtable)

gtable_plot1 <- ggplotGrob(iams1) # make facet plot a gtable object
gtable_plot1$widths <- gtable_plot1$widths * 0.95 # remove white space (padding) around facet
gtable_plot1$heights <- gtable_plot1$heights * 0.95

gtable_plot2 <- ggplotGrob(iams2) 
gtable_plot2$widths <- gtable_plot2$widths * 0.95 
gtable_plot2$heights <- gtable_plot2$heights * 0.95

gtable_plot3 <- ggplotGrob(igfs1) 
gtable_plot3$widths <- gtable_plot3$widths * 0.95 
gtable_plot3$heights <- gtable_plot3$heights * 0.95

gtable_plot4 <- ggplotGrob(igfs2)
gtable_plot4$widths <- gtable_plot4$widths * 0.95 
gtable_plot4$heights <- gtable_plot4$heights * 0.95





irl_plot <- 
  (wrap_elements(gtable_plot1) |  wrap_elements(gtable_plot2)) /
  (wrap_elements(gtable_plot3) | wrap_elements(gtable_plot4)) +
  plot_layout(design = design) +
  #plot_layout(widths = c(1, 0.6)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 7), 
        plot.tag.position = c(0.05, 1))
print(irl_plot)

ggsave("irl_relsampeff.png", irl_plot, dpi = 400, 
       width = 170, height = 150, units = "mm")

design <- "
AAB
CCD
"

irl_plot_2 <- # take this graph for publication
  wrap_elements(gtable_plot1) +  wrap_elements(gtable_plot2) +
  wrap_elements(gtable_plot3) + wrap_elements(gtable_plot4) +
  plot_layout(design = design) +
  #plot_layout(widths = c(1, 0.6)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 7), 
        plot.tag.position = c(0.05, 1))
print(irl_plot_2)

ggsave("irl_relsampeff2.png", irl_plot_2, dpi = 800, 
       width = 170, height = 175, units = "mm")




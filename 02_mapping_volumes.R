#Create a map which shows regional variation in call volumes
#Author: Stuti Bagri.................................................

#Read in geographical data
shapefile <- st_read("~/NHSER_JAN_2024_EN_BFC.shp") %>% 
  rename(region = "NHSER24NM")

#Combine geographical coordinates data with analysis data
map_vol <- shapefile %>% 
  inner_join(regional_variation, by = "region")

# Create categorisations to be used to determine shading in the map
# We will use the jenks method, so that 1) groupings are based on the data distribution (which is skewed) and 
                                      # 2) reflective of natural clusters and 
                                      # 3) variation within groups is minimised
# Reference: https://gisgeography.com/choropleth-maps-data-classification/

#Calculate natural breaks using jenks
jenks_breaks_vol <- classIntervals(regional_variation$vol_rate, n=6, style = "jenks")

#Add categorisation for the number of calls received
map_vol$vol_bucket <- cut(map_vol$vol_rate,
                          breaks = unique(jenks_breaks_vol$brks),
                          include.lowest = TRUE,
                          labels = FALSE)
map_vol$vol_bucket <- as.character(map_vol$vol_bucket)

#Categories are: 664-1713, 1714-2194, 2195-2708, 2709-3026, 3027-4391

#Create the map
call_vol_map0 <- ggplot(map_vol) +
  geom_sf_interactive(aes(fill = vol_bucket, 
                          tooltip = paste0(region,": ",format(vol_rate, big.mark=",")),
                          data_id=region),
                          lty = "solid", 
                          size = 0.2, 
                          colour = "black") +
  scale_fill_manual(aesthetics = "fill",
                    name = "Number of calls received per 100,000 population", 
                    values = c("#d7bfe6", "#D3C4FC","#B39DFF","#7140EA","#49148C"),
                    labels = c("664-1,713", "1,714-2,194", "2,195-2,708", "2,709-3,026", "3,027-4,391")) + 
  theme_minimal() +
  labs(title = "What is the rate of 111 calls received for mental health\nper 100,000 population across England? (2024/25)") +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.direction = "vertical",
    legend.title = element_text(colour="black",size=8.5),
    legend.text = element_text(colour="black", size=8.5),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title = element_text(size=12, colour="black")
  )
print(call_vol_map0)

#Making it interactive
call_volrate_map <- girafe(ggobj = call_vol_map0, 
                       options = list(opts_tooltip(css="background-color:#271544;color:white;font-size:12pt;font-family:Arial;padding:5px;border-radius:5px"),
                                      opts_hover(css = "fill:#fdea9d"),
                                      opts_sizing(rescale = TRUE, width = 0.8)))

#Save the html and png file
htmlwidgets::saveWidget(call_volrate_map, file = "~/Regional variation in 111 for MH call rates.html", selfcontained = TRUE, libdir = NULL)

setwd("~")
ggsave("111volumemapsvg.svg", plot=call_vol_map0, device="svglite")

png_1 <- call_vol_map0 + theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

print(png_1)
ggsave("111volumemappng.png", plot=png_1)


#Create a map which shows regional variation in call responsiveness
#Author: Stuti Bagri..........................................

#Read in geographical data
shapefile <- st_read("~/NHSER_JAN_2024_EN_BFC.shp") %>% 
  rename(region = "NHSER24NM")

#Combine geographical coordinates data with analysis data
map_data <- shapefile %>% 
   inner_join(regional_variation, by = "region") %>% 
   mutate(prop_callsanswered = round(prop_callsanswered,0))

# Create categorisations to be used to determine shading in the map
# We will use the jenks method, so that 1) groupings are based on the data distribution (which is skewed) and 
# 2) reflective of natural clusters and 
# 3) variation within groups is minimised
# Reference: https://gisgeography.com/choropleth-maps-data-classification/

#Calculate natural breaks using jenks
jenks_breaks_resp <- classIntervals(regional_variation$prop_callsanswered, n=5, style = "jenks")
jenks_breaks_resp$brks <- round(jenks_breaks_resp$brks, 0)

#Add categorisation for the number of calls received
map_data$prop_bucket <- cut(map_data$prop_callsanswered,
                            breaks = unique(jenks_breaks_resp$brks),
                            include.lowest = TRUE,
                            labels = FALSE)

map_data$prop_bucket <- as.character(map_data$prop_bucket)

#Categories are: 58-63%, 64-70%, 71-77%, 78-84%

#Create the map
call_resp_map0 <- ggplot(map_data) +
  geom_sf_interactive(aes(fill = prop_bucket, 
                          tooltip = paste0(region,": ",prop_callsanswered,"%"),
                          data_id=region),
                          lty = "solid", 
                          size = 0.2, 
                          colour = "black")+
  scale_fill_manual(aesthetics = "fill",
                    name = "Proportion of calls answered (%)", 
                    values = c("#49148C","#7140EA","#B39DFF","#D3C4FC"),
                    labels = c("58-63%", "64-70%", "71-77%", "78-84%")) + 
  theme_minimal() +
  labs(title = "Are 111 calls for mental health responded to uniformly across England? (2024/25") +
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
print(call_resp_map0)

#Making it interactive
call_resp_map <- girafe(ggobj = call_resp_map0, 
                       options = list(opts_tooltip(css="background-color:#271544;color:white;font-size:12pt;font-family:Arial;padding:5px;border-radius:5px"),
                                      opts_hover(css = "fill:#fdea9d"),
                                      opts_sizing(rescale = TRUE, width = 0.8)))

#Save the html and png file
htmlwidgets::saveWidget(call_resp_map, file = "~/Regional variation in 111 for MH call response.html", selfcontained = TRUE, libdir = NULL)

setwd("~")
ggsave("111responsemapsvg.svg", plot=call_resp_map0, device="svglite")

png_1 <- call_resp_map0 + theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

#print(png_1)
ggsave("111responsemappng.png", plot=png_1)


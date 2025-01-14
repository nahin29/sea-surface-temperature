library(terra)
library(tidyterra)
library(showtext)

# Load a Google Font (e.g., "Roboto")
font_add_google("Roboto", family = "Roboto")

# Register the font
showtext_auto()


sst_4jan_2025 = rast("C:/Users/nahin/OneDrive/Desktop/7_1_2025_sst.nc")

sst_4jan_2025_df = as_tibble(sst_4jan_2025, xy = T)

# Convert Kelvin to Celsius
sst_4jan_2025_df <- sst_4jan_2025_df %>%
  mutate(sst_celsius = analysed_sst - 273.15) %>%
  select(x, y, sst_celsius)

sst_4jan_2025_df = as_spatraster(sst_4jan_2025_df, crs = "epsg:4326")

library(ggplot2)
p <- project(sst_4jan_2025_df, method="near", "+proj=ortho +lat_1=24 +lat_2=0 +lon_0=-160")



# Creating the map
ggplot() +
  geom_spatraster(data = p) +
  scale_fill_gradientn(
    colors = c("#000080", "#4B0082", "#8A2BE2", "#FF69B4", "#FFC0CB", "#FFFF00"),
    na.value = "transparent",
    name = "Sea Surface Temperature (°C)"
  ) +
  geom_sf(data = world, fill = "grey20") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),  # Adjust the size of the legend keys
        legend.key.width = unit(1.5, "cm"),  # Adjust width of the legend keys
        legend.key.height = unit(0.5, "cm"),
        legend.title.position = "top",
        legend.title.align = 0.5,
        plot.title = element_text(size = 14, face = "bold")) +
  ggtitle("Pacific Ocean")



# function to extract the ocean data from the main dataset

# Define the function to extract ocean data
extract_ocean_data <- function(raster_data, xmin, xmax, ymin, ymax) {
  # Define the extent for the specified ocean region (Longitude, Latitude)
  ocean_extent <- ext(xmin, xmax, ymin, ymax)
  
  # Crop the raster to the specified ocean region
  cropped_raster <- crop(raster_data, ocean_extent)
  
  # Convert the cropped raster to a data.frame
  ocean_df <- as.data.frame(cropped_raster, xy = TRUE)
  
  return(ocean_df)
}

# extract the major ocean

#Indian Ocean region (20-150 longitude, -30 to 30 latitude)
indian_ocean_df <- extract_ocean_data(sst_4jan_2025_df, 20, 150, -30, 30)

# Atlantic Ocean (Longitude: -80 to 20, Latitude: -60 to 80)
atlantic_ocean_df <- extract_ocean_data(sst_4jan_2025_df, -80, 20, -60, 80)

# Pacific Ocean (Longitude: 150 to -80, Latitude: -60 to 60)
pacific_ocean_df <- extract_ocean_data(sst_4jan_2025_df, -80, 150, -60, 60)

# Arctic Ocean (Longitude: -180 to 180, Latitude: 60 to 90)
arctic_ocean_df <- extract_ocean_data(sst_4jan_2025_df, -180, 180, 60, 90)

# Southern Ocean (Longitude: -180 to 180, Latitude: -90 to -60)
southern_ocean_df <- extract_ocean_data(sst_4jan_2025_df, -180, 180, -90, -60)



library(ggplot2)
library(sf)

#change your ocean name
indian = ggplot() +
  geom_raster(data = indian_ocean_df, aes(x = x, y = y, fill = sst_celsius), interpolate = TRUE) +
  geom_sf(data = world, fill = "gray20", color = "black", size = 0.1) +  # Add the world map
  scale_fill_gradientn(
    colors = c("#000080", "#4B0082", "#8A2BE2", "#FF69B4", "#FFC0CB", "#FFFF00"),
    na.value = "transparent",
    name = "Sea Surface Temperature (°C)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.key.size = unit(1.5, "cm"),  # Adjust the size of the legend keys
    legend.key.width = unit(1.5, "cm"),  # Adjust width of the legend keys
    legend.key.height = unit(0.5, "cm"),
    legend.title.position = "top",
    legend.title.align = 0.5,
    axis.text = element_blank(),  # Remove axis text
    title = element_text(face = "bold"), 
    axis.title = element_blank()  # Remove the legend title
  ) +
  coord_sf(
    xlim = c(20, 150),   # Longitude limits to zoom into the Indian Ocean
    ylim = c(-30, 30),   # Latitude limits to zoom into the Indian Ocean
    expand = FALSE       # Remove extra padding around the map
  ) +
  
  labs(
    title = "Indian Ocean",
    x = "Longitude",
    y = "Latitude",
    fill = "SST (°C)"
  )




###DATA ANALYSIS PROJECT###
#PART6 : 3D distribution and density map
################################################################################
# required libraries
#install.packages("tidyverse")
library(sf)
library(elevatr)
library(raster)
library(sp)
library(png)
library(rayshader)
library(tidyverse)
library(RColorBrewer)
library(eks)

################################################################################
#map of Switzerland and Italy with relief
elmat <- raster_to_matrix(elevation_swi_ita)
attr(elmat, "extent") <- extent(elevation_swi_ita)
elmat %>% 
  sphere_shade(texture = "bw") %>%
  plot_map()

# 3d version of the map of the two countries (function plot_3d) 
elmat %>% 
  sphere_shade(texture = "bw") %>%
plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))

################################################################################
#3D MAP with the coordinates of each occurencies
# add landscape texture (only have Switzerland one) 
elevation.texture.map <- readPNG("/Users/mafalda/Desktop/DataAnalysis/project/data/Switzerland2.png")
#elevation.texture.map.crop <- crop.image(elevation.texture.map,xleft=146,ybottom=7,xright=203,ytop=256)

# add coordinate points 
latitude <- matrix_full$latitude
longitude <- matrix_full$longitude

# Create a data frame for GBIF data
gbif_coord <- data.frame(longitude,latitude)

# Extract elevation values
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(gbif_coord, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- raster::extract(elevation_swi_ita, points, method='bilinear')
elevation_df <- data.frame(elevation = elevation_points)

# 3d version 
elmat %>% 
  sphere_shade(texture = "bw") %>%
  #add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.7)  %>%
   add_shadow(cloud_shade(elmat, zscale = 100, start_altitude = 500, end_altitude = 2000,), 0) %>%
   add_water(detect_water(elmat), color = "lightblue") %>%
plot_3d(elmat, zscale = 100, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))

# Render points on the 3D elevation map
render_points(
  extent = extent(Swi_ita), size = 10,
  lat = gbif_coord$latitude, long = gbif_coord$longitude,
  altitude = elevation_points + 100, zscale = 1, color = "darkred"
)
#With this map we can not differenciate between the two species

################################################################################
#3D DENSITY MAP of the TWO SPECIES
#separation of the data of both species
matrix_full_sp1 <- matrix_full[matrix_full$species =="Triturus cristatus",]
matrix_full_sp2 <- matrix_full[matrix_full$species =="Triturus carnifex",]
sf_use_s2(FALSE)

#Density data for Triturus cristatus
 sf_points_cn <- data.frame(
    lat = matrix_full_sp1$latitude,
    lon = matrix_full_sp1$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
#plot(sf_points_cn)

skde1 <- st_kde(sf_points_cn, gridsize = c(100, 100))
#plot(skde1)
data_cn = st_get_contour(skde1, cont = c(seq(1, 99, 5)), disjoint = FALSE)

# Create a function to generate the color palette
color_palette_cn <- colorRampPalette(c("darkolivegreen4","darkolivegreen3","darkseagreen1","yellow","orange","red","darkred"))

# Define the number of colors in the palette
num_colors_cn <- length(data_cn$contlabel)

# Generate the color palette
palette_cn <- color_palette_cn(num_colors_cn)

#Density data for Triturus carnifex
 sf_points_mn <- data.frame(
    lat = matrix_full_sp2$latitude,
    lon = matrix_full_sp2$longitude
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
#plot(sf_points_mn)

skde2 <- st_kde(sf_points_cn, gridsize = c(100, 100))
#plot(skde2)
data_mn = st_get_contour(skde2, cont = c(seq(1, 99, 5)), disjoint = FALSE)

# Create a function to generate the color palette
color_palette_mn <- colorRampPalette(c("darkblue","blue","lightblue","yellow","orange", "pink", "magenta"))

# Define the number of colors in the palette
num_colors_mn <- length(data_mn$contlabel)

# Generate the color palette
palette_mn <- color_palette_mn(num_colors_mn)

################################################################################
#3D Density Model plot
elmat <- raster_to_matrix(elevation_swi_ita)

elmat %>%
 sphere_shade(texture = "bw") %>%
 #add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.9)  %>% #only texture for switzerland
 add_overlay(generate_polygon_overlay(data_cn, 
                        palette = palette_cn, linewidth=0,
                        extent = extent(elevation_swi_ita), heightmap = elmat),
                        alphalayer=0.7)  %>%
 add_overlay(generate_polygon_overlay(data_mn, 
                        palette = palette_mn, linewidth=0,
                        extent = extent(elevation_swi_ita), heightmap = elmat),
                        alphalayer=0.7)  %>%
#plot_map()
plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        phi = 45, windowsize = c(1500, 800))

# Render points on the 3D elevation map
render_points(
  extent = extent(Swi_ita), size = 5,
  lat = matrix_full$latitude, long = matrix_full$longitude,
  altitude = elevation_points + 100, zscale = 150, color = "black"
)


################################################################################
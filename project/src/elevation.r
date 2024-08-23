###DATA ANALYSIS PROJECT###
#PART2 : add elevation data
################################################################################
# required libraries
library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(png)

###############################################################################
sf_use_s2(FALSE)

#map of Switzerland and italy with colorimetric elevation
Swi_ita <- ne_countries(scale = "medium", returnclass = "sf",country =c("Switzerland", "Italy"))
elevation_swi_ita <- get_elev_raster(Swi_ita, z = 8)
plot(elevation_swi_ita)

# crop and mask to isolate only the desired countries
r2 <- crop(elevation_swi_ita, extent(Swi_ita))
elevation_swi_ita <- mask(r2, Swi_ita)
plot(elevation_swi_ita)

#add the occurences points of the species in the elevation map
latitude <- matrix_full$latitude
longitude <- matrix_full$longitude

#Create a data frame for GBIF data
gbif_coord <- data.frame(longitude,latitude)

#Extract elevation values
ll_prj <- "EPSG:4326" 
points <- sp::SpatialPoints(gbif_coord, 
                            proj4string = sp::CRS(SRS_string = ll_prj))
elevation_points <- extract(elevation_swi_ita, points, method='bilinear')
elevation_df <- data.frame(elevation = elevation_points)

#Add the elevation values to the matrix_full
matrix_full<-cbind(matrix_full,elevation_df)
View(matrix_full)

#Density plot to copare the density of each species at each elevation
density_plot <- ggplot(data= matrix_full, aes(x = elevation, group=species, fill=species)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(title = "Density Plot of elevation data", x = "Elevation[m]", y = "Density") +
    scale_fill_brewer(palette = "Set2") 

print(density_plot)

#We can see with this plot that the density of CN is the highest between 200-800m.
#MN density is higher at 250m. 
###############################################################################

###############################################################################
###############################################################################
#CODE POUR FAIRE DES 3D MAPS 
#elmat <- raster_to_matrix(elevation_swi_ita)
#attr(elmat, "extent") <- extent(elevation_swi_ita)
#elmat %>% 
  #sphere_shade(texture = "bw") %>%
  #plot_map()

###### 3d version 
#elmat %>% 
  #sphere_shade(texture = "bw") %>%
#plot_3d(elmat, zscale = 150, fov = 0, theta = 135, zoom = 0.75, 
        #phi = 45, windowsize = c(1500, 800))


#Image PNG of Switzerland to add texture to the 3D map  
#elevation.texture.map <- readPNG("/Users/mafalda/Desktop/DataAnalysis/project/data/Switzerland2.png")
#elmat %>%
 #sphere_shade(texture = "desert") %>%
  #add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.7)  %>%
  #plot_map()

# Render points on the 3D elevation map
#render_points(
  #extent = extent(Switzerland), size = 10,
  #lat = matrix_full$latitude, long = matrix_full$longitude,
  #altitude = elevation_points + 100, zscale = 150, color = "darkred"
#)

#3d version 
#elmat %>% 
  #sphere_shade(texture = "bw") %>%
  #add_overlay(elevation.texture.map, alphacolor = NULL, alphalayer = 0.7)  %>%
   #add_shadow(cloud_shade(elmat, zscale = 100, start_altitude = 500, end_altitude = 2000,), 0) %>%
   #add_water(detect_water(elmat), color = "lightblue") %>%
#plot_3d(elmat, zscale = 100, fov = 0, theta = 135, zoom = 0.75, 
        #phi = 45, windowsize = c(1500, 800))
############################################################################### 
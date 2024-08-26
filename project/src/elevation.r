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
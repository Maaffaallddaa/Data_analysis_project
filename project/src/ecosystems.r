###DATA ANALYSIS PROJECT###
#PART4 : add climate data : temperature + precipitation
################################################################################
# required libraries
library(raster)
library(ggplot2)

################################################################################
##### https://esri.maps.arcgis.com/home/item.html?id=3bfa1aa4cd9844d5a0922540210da25b
# Set the file path to your GeoTIFF
file_path <- "/Users/mafalda/Desktop/DataAnalysis/project/data/WorldEcosystem.tif"

# Read the raster GeoTIFF
ecosystem_raster <- raster(file_path)

#select my countries
Swi_ita<- ne_countries(scale = "medium", returnclass = "sf",country =c("Switzerland","Italy"))
#crop and mask
r2 <- crop(ecosystem_raster, extent(Swi_ita))
ecosystem <- mask(r2, Swi_ita)
plot(ecosystem)

#select longitude and latitude values from matrix_full
spatial_points <- SpatialPoints(coords = matrix_full[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(spatial_points,add=T,pch=16,cex=2)

# Extract values of ecoysytem corresonding to the coordinates 
eco_values <- raster::extract(ecosystem, spatial_points)
#print(eco_values)

#add the ecosystem values to the matrix_full
matrix_full <- data.frame(matrix_full,eco_values)

################################################################################
#METADATA
metadat_eco <- read.delim("/Users/mafalda/Desktop/DataAnalysis/project/data/WorldEcosystem.metadata.tsv")
matrix_full<- merge(matrix_full,metadat_eco,by.x="eco_values",by.y="Value")
#View(matrix_full)

#Create ggplot of the 
p2<-ggplot(matrix_full,aes(x=Climate_Re,fill=species))+
    geom_bar(position="dodge")+
    labs(title="Count of Observations of Each Species by Climate",
        x="Climate",y="Count of observations")+
    theme_minimal()

print(p2)
#We can see that Triturus carnifex is more found in warm temperate moist, 
#while Triturus cristatus in cool temperate moist

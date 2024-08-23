###DATA ANALYSIS PROJECT###
#PART3 : add climate data : temperature + precipitation
################################################################################
# required libraries
library(geodata)
library(terra)
library(sp)
library(ggplot2)
#install.packages("SpatialPoints")
#library(SpatialPoints)

################################################################################
# Attribute coordinates to each occurency
spatial_points <- SpatialPoints(coords = matrix_full[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))

################################################################################
###TEMPERATURE
# Retrieve temperature data for each country
sw_clim <- worldclim_country("switzerland", var = "tavg", path = tempdir())
it_clim <- worldclim_country("italy", var = "tavg", path = tempdir())

#merge the temperature data of the two coountries and transform
sw_it_clim <- merge(sw_clim, it_clim)
sw_it_br <- brick(sw_it_clim)

#BOUCLE: temperature for all months
matrix_clim_temp = NULL        # création d'un dataframe vide que l'on va remplir
vec_colnames1 = NULL           # vecteur vide qui sera le nom des colonnes
for(i in 1:12) {
  raster_temp <- as(sw_it_br[[i]], "Raster")
  vec_colnames1 <- c(vec_colnames1, names(raster_temp))                       
  raster_month <- raster::extract(raster_temp, spatial_points, method = 'bilinear')
  matrix_clim_temp <- cbind(matrix_clim_temp, raster_month)
}

# pour rajouter la moyenne des températures des mois dans une colonne 
vec_mean_temp <- as.vector(rowMeans(matrix_clim_temp))                  # moyenne sur toutes les lignes, pour tous les mois
#create matrix with temperature per month and average
matrix_temp <- data.frame(matrix_clim_temp, vec_mean_temp)
# Add the names of the column in the matrix
colnames(matrix_temp) <- c("January_temp","February_temp", "March_temp", "April_temp", "May_temp", "June_temp", "July_temp", "August_temp", "September_temp", "October_temp", "November_temp", "December_temp", "Average_temp")
View(matrix_temp)

# Plot density of temperature data 
ggplot(matrix_temp, aes(x = vec_mean_temp)) +
  geom_density(color = "darkblue", fill = "lightblue", adjust = 3) +
  theme_bw()
#density is higher between 7-10 mean temp

################################################################################
###PRECIPITATION
# Retrieve temperature data for each country
sw_prec <- worldclim_country("switzerland", var = "prec", path = tempdir())
it_prec <- worldclim_country("italy", var = "prec", path = tempdir())

#merge the temperature data of the two coountries and transform
sw_it_prec <- merge(sw_prec, it_prec)
sw_it_prec_br <- brick(sw_it_prec)

#BOUCLE: temperature for all months
matrix_clim_prec = NULL        # création d'un dataframe vide que l'on va remplir
vec_colnames2 = NULL           # vecteur vide qui sera le nom des colonnes
for(i in 1:12) {
  raster_prec <- as(sw_it_prec_br[[i]], "Raster")
  vec_colnames2 <- c(vec_colnames2, names(raster_prec))                       
  raster_month <- raster::extract(raster_prec, spatial_points, method = 'bilinear')
  matrix_clim_prec <- cbind(matrix_clim_prec, raster_month)
}

# pour rajouter la moyenne des températures des mois dans une colonne 
vec_mean_prec <- as.vector(rowMeans(matrix_clim_prec))                  # moyenne sur toutes les lignes, pour tous les mois
#matrix de precipitation moyenne et par mois
matrix_prec <- data.frame(matrix_clim_prec, vec_mean_prec)
#add the names to the columns
colnames(matrix_prec) <- c("January_prec","February_prec", "March_prec", "April_prec", "May_prec", "June_prec", "July_prec", "August_prec", "September_prec", "October_prec", "November_prec", "December_prec", "Average_prec")
View(matrix_prec)

# Plot density of precipitation 
ggplot(matrix_prec, aes(x = vec_mean_prec)) +
  geom_density(color = "darkblue", fill = "lightblue", adjust = 3) +
  theme_bw()
#density is higher between 70-100 mean precipitation

################################################################################
#add the climate values to the matrix_full
matrix_full <- data.frame(matrix_full,matrix_temp, matrix_prec)
View(matrix_full)

################################################################################
## Scatter plot pour voir la distribution de chaque occurence en fonction de la température et précipitation moyenne
ggplot(matrix_full, aes(x = vec_mean_prec, y= vec_mean_temp, color = species)) +
  geom_point() +
  labs(x = "Precipitation", y = "Temperature", color = "Species") +
  scale_color_manual(values=c("blue3", "orange2")) +
  theme_minimal()     

#We can see that the triturus cristatus prefers lower temperatures than triturus carnifex
#triturus carnifex has a larger climatic niche 

################################################################################
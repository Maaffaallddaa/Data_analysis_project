###DATA ANALYSIS PROJECT###
#PART1 : import data for both species

# Load required libraries
#remotes::install_github("wmgeolab/rgeoboundaries")
#install.packages("sf")
#install.packages("remotes")
library(remotes)
#ßremotes::install_github("ropensci/MODIStsp")
#install.packages("rgeoboundaries")
library(sf)
library(rgeoboundaries)
#install.packages("here")
#install.packages("viridisLite")
#install.packages("rgdal")
library(raster)
library(here)
library(ggplot2)
library(viridis)
library(terra)
#install_github("ropensci/MODIStsp")
library(MODIStsp)

###############################################################################
# TIFF for Switzerland
mapCH_boundary <- geoboundaries("switzerland")
dir.create("./data/modis", recursive = TRUE)

# Defining filepath to save downloaded spatial file
spatial_filepath <- "./data/modis/switzerland.shp"

# Saving downloaded spatial file on to our computer
st_write(mapCH_boundary, paste0(spatial_filepath))


#### check available data
MODIStsp_get_prodlayers("M*D13Q1")

MODIStsp(
  gui = FALSE,
  out_folder = "./data/modis",
  out_folder_mod = "./data/modis",
  selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  bandsel = "NDVI",
  user = "mstp_test",
  password = "MSTP_test_01",
  start_date = "2020.06.01",
  end_date = "2020.06.01",
  verbose = FALSE,
  spatmeth = "file",
  spafile = spatial_filepath,
  out_format = "GTiff"
)

# Downloading the boundary of switzerland
mapCH_boundary <- geoboundaries("switzerland")

###############################################################################
# TIFF for Italy
mapIT_boundary <- geoboundaries("Italy")

# Defining filepath to save downloaded spatial file
spatial_filepath <- "./data/modis/italy.shp"

# Saving downloaded spatial file on to our computer
st_write(mapIT_boundary, paste0(spatial_filepath))

#### check available data
MODIStsp_get_prodlayers("M*D13Q1")

MODIStsp(
  gui = FALSE,
  out_folder = "./data/modis",
  out_folder_mod = "./data/modis",
  selprod = "Vegetation Indexes_16Days_250m (M*D13Q1)",
  bandsel = "NDVI",
  user = "mstp_test",
  password = "MSTP_test_01",
  start_date = "2020.06.01",
  end_date = "2020.06.01",
  verbose = FALSE,
  spatmeth = "file",
  spafile = spatial_filepath,
  out_format = "GTiff"
)

# Downloading the boundary of Italy
mapIT_boundary <- geoboundaries("italy")

###############################################################################
# Reading in the downloaded NDVI raster data of Switzerland and Italy
NDVICH_raster <- raster("./data/MYD13Q1_NDVI_2020_153.tif")
NDVIIT_raster <- raster("./data/MYD13Q1_NDVI_2020_153.tif") #METTRE TIFF ITALY

#merging the two rasters
NDVI_raster <- merge(NDVICH_raster, NDVIIT_raster)
# Transforming the data
NDVI_raster <- projectRaster(NDVI_raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(NDVI_raster)

# Cropping the data
map_boundary <- geoboundaries(c("switzerland", "italy"))
NDVI_raster <- raster::mask(NDVI_raster, as_Spatial(map_boundary))
plot(NDVI_raster)

# Dividing values by 10000 to have NDVI values between -1 and 1
gain(NDVI_raster) <- 0.0001

# Assuming matrix_full is your data frame with latitude and longitude columns
spatial_points <- SpatialPoints(coords = matrix_full[, c("longitude","latitude")], proj4string = CRS("+proj=longlat +datum=WGS84"))
plot(spatial_points,add=T)

# Extract NDVI values
NDVI <- raster::extract(NDVI_raster, spatial_points)

#add the NDVI vallues to the matrix_full
matrix_full <- data.frame(matrix_full,NDVI)
###DATA ANALYSIS PROJECT###
#PART1 : import data for both species

# Load required libraries
library(rgbif)         # For accessing GBIF occurrence data
library(rnaturalearth) # For obtaining spatial data
library(ggplot2)       # For data visualization
library(rinat)         # For accessing iNaturalist occurrence data
library(raster)        # For spatial operations

###############################################################################

#The two species : Great crested Newt (Triturus cristatus) and Marbled Newt (Triturus carnifex)
myspecies1 <- c("Triturus cristatus")
myspecies2 <- c("Triturus carnifex")

#The two countries : Switzerland and Italy
Swiss_Italy <- ne_countries(scale = "medium", returnclass = "sf", country = c("Switzerland", "Italy"))

###############################################################################

### OCCURENCES OF SPECIES 1: THE GREAT CRESTED NEWT (CN) ###
#Download of the occurences in gbif for Switzerland and Italy
gbif_data1_CH <- occ_data(scientificName = myspecies1,country="CH", hasCoordinate = TRUE, limit = 5000)
gbif_data1_IT <- occ_data(scientificName = myspecies1,country="IT", hasCoordinate = TRUE, limit = 5000)

# Extract GBIF occurrences
occur1_CH <- gbif_data1_CH$data
occur1_IT <- gbif_data1_IT$data

# Plot of CN occurences in Switzreland + Italy
ggplot(data = Swiss_Italy) +
  geom_sf() +
  geom_point(data = occur1_CH, aes(x = decimalLongitude, y = decimalLatitude, fill=myspecies1), size = 4, 
             shape = 23, fill = "lightblue") + theme_classic() +
  geom_point(data = occur1_IT, aes(x = decimalLongitude, y = decimalLatitude, fill=myspecies1), size = 4, 
             shape = 23, fill = "blue") +
  coord_sf(xlim = c(5, 20), ylim = c(35, 50)) 
##################################################

# Extract relevant data from GBIF occurrences SWITZERLAND
species <- occur1_CH$species
latitude <- occur1_CH$decimalLatitude
longitude <- occur1_CH$decimalLongitude
source <- rep("gbif", length(species)) #create a column for the data source

# Create a data frame for CNN in Switzerland
dataCN_CH <- data.frame(species, latitude, longitude, source)

# Extract relevant data from GBIF occurrences ITALY
species <- occur1_IT$species
latitude <- occur1_IT$decimalLatitude
longitude <- occur1_IT$decimalLongitude
source <- rep("gbif", length(species))

# Create a data frame for CN in Italy
dataCN_IT <- data.frame(species, latitude, longitude, source)

#MATRIX FULL CN in Switzeralnd + Italy
matrix_full_CN <- rbind(dataCN_CH, dataCN_IT)
View(matrix_full_CN)

###############################################################################

### OCCURENCES OF SPECIES 2: THE MARBLED NEWT (MN) ###
#Download of the occurences in gbif for Switzerland and Italy
gbif_data2_CH <- occ_data(scientificName = myspecies2,country="CH", hasCoordinate = TRUE, limit = 5000)
gbif_data2_IT <- occ_data(scientificName = myspecies2,country="IT", hasCoordinate = TRUE, limit = 5000)

# Extract GBIF occurrences
occur2_CH <- gbif_data2_CH$data
occur2_IT <- gbif_data2_IT$data

# Plot of MN occurences in Switzreland + Italy
ggplot(data = Swiss_Italy) +
  geom_sf() +
  geom_point(data = occur2_CH, aes(x = decimalLongitude, y = decimalLatitude, fill=myspecies2), size = 4, 
             shape = 23, fill = "lightpink") + theme_classic() +
  geom_point(data = occur2_IT, aes(x = decimalLongitude, y = decimalLatitude, fill=myspecies2), size = 4, 
             shape = 23, fill = "magenta") +
  coord_sf(xlim = c(5, 20), ylim = c(35, 50)) 
##################################################

# Extract relevant data from GBIF occurrences SWITZERLAND
species <- occur2_CH$species
latitude <- occur2_CH$decimalLatitude
longitude <- occur2_CH$decimalLongitude
source <- rep("gbif", length(species)) #create a column for the data source

# Create a data frame for MN in Switzerland
dataMN_CH <- data.frame(species, latitude, longitude, source)

# Extract relevant data from GBIF occurrences ITALY
species <- occur2_IT$species
latitude <- occur2_IT$decimalLatitude
longitude <- occur2_IT$decimalLongitude
source <- rep("gbif", length(species))

# Create a data frame for MN in Italy
dataMN_IT <- data.frame(species, latitude, longitude, source)

#MATRIX FULL CN in Switzeralnd + Italy
matrix_full_MN <- rbind(dataMN_CH, dataMN_IT)

###############################################################################

#Combination of the two species data
matrix_full <- rbind(matrix_full_CN, matrix_full_MN)

# Plot combined data on a map 
p <- ggplot(data = Swiss_Italy) +
  geom_sf() +
  geom_point(data = matrix_full, aes(x = longitude, y = latitude, fill = species), size = 1.5, 
             shape = 23) +
  theme_classic() +
  coord_sf(xlim = c(5, 12), ylim = c(44, 50)) 

print(p)

#Those maps enable to have a first insight on the two species distribution
#We can see that CN is mainly found in Switzerland and MN in Italy, 
#however some MN individuals are also found in the southern parts of Switzerland
#MN is considered an invasive species in Switzerland
###############################################################################
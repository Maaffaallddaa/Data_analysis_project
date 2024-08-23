####################################################################################
###################### DATA ANALYSIS PERSONAL PROJECT ##############################
####################################################################################

#project: triton crêté (triturus cristatus) et triton méridional (triturus carnifex)

####################################################################################

#PART1: obtain data of the species distribution in Switzerland and in Italy
source("./src/import_data.r")

#PART2: add elevation values for each species' occurencies to the matrix
source("src/elevation.r")

#PART3: add climate data to the matrix
source("src/clim.r")

#PART4: add ecosystems values to the matrix
source("src/ecosystems.r")

#PART5: add satellite data to the matrix
source("./src/NDVI.r")
##METTRE TIFF ITALY CORRECT

#PART6: distribution and density maps in 3D
source("src/3D_maps.r")
#PALETTE DENSITY POUR LES DEUX ESPECES

#PART8:
source("src/plots.r")

#PART9:
source("src/stat.R")

#LIEN GITHUB
####################################################################################
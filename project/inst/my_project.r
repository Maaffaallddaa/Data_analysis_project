####################################################################################
###################### DATA ANALYSIS PERSONAL PROJECT ##############################
####################################################################################

#project: Great crested Newt (Triturus cristatus) and Marbled Newt (Triturus carnifex)

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

#PART6: distribution and density maps in 3D
source("src/3D_maps.r")

#PART7: PCA and heatmap analysis
source("src/multivariate_plot.r")

#PART8: statistic analysis :: correlation and ANOVA test
source("src/stat.R")

#PART9: random forest
source("src/machine_learning.r")

####################################################################################
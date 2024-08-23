
######################################################################
######################################################################
############## hierachical clustering
install.packages("permute")
install.packages("lattice")
install.packages("ggtree") #PAS DISPONIBLE DANS CETTE VERSION R 
library(vegan)
library(ape)
library(ggtree)
library(randomcoloR)

df <- matrix_full[,colnames(matrix_full) %in% c("Average_temp","elevation","Average_prec")]

df <- apply(df,2,as.numeric)

dist_df <- vegdist(df, method = "euclidian") # method="man" # is a bit better
hclust_df<- hclust(dist_df, method = "complete")

dendro_df <-  as.phylo(hclust_df)

plot(dendro_df)

############ circular plot

matrix_full$ID <- c(1:nrow(matrix_full))

ID <- as.factor(matrix_full$ID)
target_factor  <- as.factor(matrix_full$species)
g2 <- split(ID, target_factor)

#################################################################################
################################################################################


tree_plot <- ggtree::groupOTU(dendro_df, g2,group_name = "grouper")

#################################################################################
################################################################################


cols <- distinctColorPalette(length(unique(target_factor))) ## several color
cols <- cols[1:length(unique(target_factor))]

#################################################################################
################################################################################

circ <- ggtree(tree_plot, aes(color = grouper), size = 1,layout = "circular") + 
  geom_tiplab(size = 3) +
  scale_color_manual(values = cols) 

circ 


############################################################################
############################################################################
############### aggregate version 

df2 <- matrix_full_eco_elev_clim_sat[,colnames(matrix_full_eco_elev_clim_sat) %in% c("Climate_Re","temp","elevation","NDVI","precip")]

aggregated_df2 <- aggregate(. ~ Climate_Re, data = df2 , FUN = mean)

df2_hclust <- aggregated_df2[,colnames(aggregated_df2) %in% c("temp","elevation","NDVI","precip")]


dist_df2 <- vegdist(df2_hclust, method = "euclidian") # method="man" # is a bit better
hclust_df2<- hclust(dist_df2, method = "complete")

dendro_df2 <-  as.phylo(hclust_df2)

#plot(dendro_df2)

############ circular plot

aggregated_df2$ID <- c(1:nrow(aggregated_df2))

ID <- as.factor(aggregated_df2$ID)
target_factor  <- as.factor(aggregated_df2$Climate_Re)
g2 <- split(ID, target_factor)

#################################################################################
################################################################################

tree_plot2 <- ggtree::groupOTU(dendro_df2, g2,group_name = "grouper")

#################################################################################
################################################################################

cols <- distinctColorPalette(length(unique(target_factor))) ## several color
cols <- cols[1:(length(unique(target_factor))+1)]

#################################################################################
################################################################################

circ <- ggtree(tree_plot2, aes(color = grouper), size = 1,layout = "circular") + 
  geom_tiplab(size = 3) +
  scale_color_manual(values = cols) 

circ 
###DATA ANALYSIS PROJECT###
#PART7 : interactive plots

################################################################################
# required libraries
#install.packages("ggfortfy")
#install.packages("permute")
#install.packages("lattice")
#install.packages("plotly")
library(ggfortify)
library(vegan)
library(plotly)
library(gridExtra)
library(ggpubr)
library(ggplot2)
library(pheatmap)
library(randomcoloR)

################################################################################
#select variables dicrete and continuous in two different matrix
df <- matrix_full
df <- na.omit(df)
df_continous <- df[,colnames(df) %in% c("Average_temp","elevation","Average_prec")]
df_discrete <- df[,!(colnames(df) %in% c("Average_temp","elevation","Average_prec"))] #all variable except continous
df_continous <- apply(df_continous,2,as.numeric)
#View(df_continous)

################################################################################
#PCA 2D
pca_res <- prcomp(df_continous, scale. = TRUE)

#PCA WITH GGPLOT
# extract PCA results
pca_data <- as.data.frame(pca_res$x)  # Scores (principal components)
View(pca_data)

#PCA with ggplot
ggplot(pca_data, aes(x = PC1, y = PC2, color = species)) +
  geom_point() +
  geom_segment(data = as.data.frame(pca_res$rotation), 
               aes(xend = PC1, yend = PC2, x = 0, y = 0), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "black") +
  geom_text(data = as.data.frame(pca_res$rotation), 
            aes(x = PC1, y = PC2, label = rownames(pca_res$rotation)), 
            size = 3, hjust = 1, vjust = 1, color = "black") +
  theme_classic()
#We can see that the two groups don't overlap, 
#this indicates that the two species have different niches for the continous values

#pca with autoplot
pca2 <- autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') +
         theme(legend.title = element_text(size = 15, face = "bold"), legend.text = element_text(size = 15)) 
print(pca2)

################################################################################
#PCA EUCLIDIAN
row.names(df_continous) <- c(1:nrow(df_continous))
dist_matt <- vegdist(df_continous, method = "euclidian")

#shorten the matrix otherwise it doesnt work
df_reduced <- dist_matt[1:200,]
dist_matt_reduced <- vegdist(df_reduced, method = "euclidean")
  
#BUG AT THIS STEP!! (takes a long time but finally works)
D3_data_dist <- cmdscale(dist_matt, k = 3)
D3_data_dist <- data.frame(D3_data_dist)

cols <- df_discrete$species

PCOA <- ggplot(D3_data_dist, aes(x = X1, y = X2, color = cols)) +
  geom_point() + ggtitle("my project") +
  theme_classic()
PCOA

#PCA INTERACTIVE
intercative_pcao <- ggplotly(PCOA)
intercative_pcao

################################################################################
#PCA INTERACTIVE
pca <- princomp(df_continous, scores=T, cor=T)

# Scores
scores <- pca$scores
x <- scores[,1]
y <- scores[,2]
z <- scores[,3]

# Loadings
loads <- pca$loadings

# Loadings names
load_names <- rownames(loads)

# Scale factor for loadings
scale.loads <- 5

# 3D plot
p <- plot_ly() %>%
  add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="markers", color = df_discrete$species)

for (k in 1:nrow(loads)) {
   x <- c(0, loads[k,1])*scale.loads
   y <- c(0, loads[k,2])*scale.loads
   z <- c(0, loads[k,3])*scale.loads
   p <- p %>% add_trace(x=x, y=y, z=z,
            type="scatter3d", mode="lines",
            line = list(width=8),
            opacity = 1,
            name = load_names[k])  # Adding names to the loadings
}
print(p)
#the two species seem to be spatially separated in two different groups
#a lot of data so difficult to have a good representation with 3D plot
#better to use with a smallest dataset 

################################################################################
#HEATMAP 
data <- df_continous
row.names(data) <- c(1:nrow(data))

heatmap(scale(data))
#we can see thatthe variables precipitation and elevation are regrouped

#ADVANCED HEATMAP
# Sélection des facteurs pour les annotations
factor <- df_discrete[c("Climate_Re")]  
row.names(factor) <- c(1:nrow(factor))

#annotations
pheatmap(scale(data),
         annotation_row = factor)

# color palette of the heatmap
heat_pal <- grDevices::colorRampPalette(c("yellow", "lightblue", "lightpink", "lightgreen"))

# display la heatmap. 
ht <- pheatmap(scale(data),
         annotation_row = factor,
         cutree_rows = 2,
         cutree_cols = 2,
         cellwidth = 100,
         cellheight = 0.2,
         color = heat_pal(10))
ht

#the variables elevation, average temp and average prec are represented by columns
#the different color gradients show how the three variables differ for each Climate_Re
#each line corresponding to a specific Climate_Re have similar color patterns in each column
#the three variables vary mainly from 4 to -6

################################################################################
#NICHE COMPARISON IN SWITZERLAND
# only observations in switzerland for the two species
data_swi <- subset(matrix_full, country == "Switzerland")

#only the places where both species are present
data_swi_shared <- subset(data_swi, species %in% c("Triturus cristatus", "Triturus carnifex"))
#create a column for the location of each occurence with the latitude, longitude and elevation
data_swi_shared$location <- paste(data_swi_shared$latitude, data_swi_shared$longitude, data_swi_shared$elevation, sep = "_")
#View(data_swi_shared)

#niche<- aggregate(cbind(Average_temp, Average_prec, Landcover, W_Ecosystm) ~ location,
                                    #data = data_swi_shared, 
                                    #FUN = mean)
#print(niche)
#i wanted to use this code to see what are the niche characteristics of the locations shared by the two species in Switzerland,
#but it did not work
#so i only looked at the ecosystems 
ecosystem_counts <- aggregate(species ~ W_Ecosystm, data = data_swi_shared, FUN = length)
names(ecosystem_counts)[2] <- ("count")

#barplot to represent the ecosystems where the two species are present
ggplot(ecosystem_counts, aes(x = reorder(W_Ecosystm, -count), y = count, fill = W_Ecosystm)) +
  geom_bar(stat = "identity") +
  labs(title = "Most common ecosystems in locations shared by both species",
       x = "Ecosystem type", y = "Occurencies counts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #texte penché

#We can see that the three most common ecosystems in Switzerland are Cool Temperate moist cropland on mountains,
#Cool temperate moist grasslands on mountains and cool temperate moist forest on mountains
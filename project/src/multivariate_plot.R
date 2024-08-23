###DATA ANALYSIS PROJECT###
#PART7 : interactive plots
################################################################################
# required libraries
install.packages("ggfortfy")
install.packages("permute")
install.packages("lattice")
install.packages("plotly")
library(ggfortify)
library(vegan)
library(plotly)
library(gridExtra)
library(ggpubr)
library(ggplot2)

################################################################################
#select variables dicrete and continuous in two different matrix
df <- matrix_full
df <- na.omit(df)
View(df)
df_continous <- df[,colnames(df) %in% c("Average_temp","elevation","NDVI","Averange_prec")]
df_discrete <- df[,!(colnames(df) %in% c("Average_temp","elevation","NDVI","Average_prec"))] #all variable except continous
df_continous <- apply(df_continous,2,as.numeric)

################################################################################
#PCA 2D
pca_res <- prcomp(df_continous, scale. = TRUE)

#ERROR: PRCOMP NOT SUPPORTED BY AUTOPLOT!!
autoplot(pca_res)

autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

pca_plot <- autoplot(pca_res, data = df_discrete, colour = 'species',
                     loadings = TRUE, loadings.colour = 'black',
                     loadings.label = TRUE, loadings.label.size = 3)

pca_plot + theme_classic()

#same code???
autoplot(pca_res, data = df_discrete, colour = 'species',
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, frame = TRUE) + theme_classic()

##PCA2 WITH GGPLOT
# extract PCA results
pca_data <- as.data.frame(pca_res$x)  # Scores (principal components)
pca_data$species <- df_discrete$species  # Add species information

#PCA step by step
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
#We can that the two groups don't overlap, 
#this indicates that the two species have different niches for the continous values

# Autre plot de la PCA, modèle éliptique
#pca2 <- autoplot(pca_res, data = df_discrete, colour = 'species',
         #loadings = TRUE, loadings.colour = 'black',
         #loadings.label = TRUE, loadings.label.size = 3, frame = TRUE, frame.type = 'norm') +
         #theme(legend.title = element_text(size = 15, face = "bold"), legend.text = element_text(size = 15)) 

# Mettre les graph sur la même page
#grid.arrange(pca1, pca2 + rremove("x.text"), 
             #ncol = 1, nrow = 2)
################################################################################
#PCA EUCLIDIAN
row.names(df_continous) <- c(1:nrow(df_continous))

dist_matt <- vegdist(df_continous, method = "euclidian")  
#BUG AT THIS STEP!!
D3_data_dist <- cmdscale(dist_matt, k = 3)
D3_data_dist <- data.frame(D3_data_dist)

cols <- df_discrete$species

PCOA <- ggplot(D3_data_dist, aes(x = X1, y = X2, color = cols)) +
  geom_point() + ggtitle("my project") +
  theme_classic()
PCOA

################################################################################
#PCA INTERACTIVE
intercative_pcao <- ggplotly(PCOA)
intercative_pcao

# Convert dataframe columns to numeric
df_continous <- apply(df_continous, 2, as.numeric)

# PCA
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
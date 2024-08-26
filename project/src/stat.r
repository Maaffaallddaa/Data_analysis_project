###DATA ANALYSIS PROJECT###
#PART8 : statistical analysis
################################################################################
# required libraries
#install.packages("ggcorrplot")
#install.packages("ggfortify")
#install.packages("emmeans")
library(ggplot2)
library(ggcorrplot)
library(ggfortify)
library(corrplot)
library(pheatmap)
library(emmeans)
library(plotly)

################################################################################
# Data Exploration 
df <- matrix_full
View(matrix_full)
df <- na.omit(df)  # Remove rows with missing values

# Separate continuous and discrete variables
df_continous <- df[, colnames(df) %in% c("Average_temp", "elevation", "Average_prec")]
df_discrete <- df[, !(colnames(df) %in% c("Average_temp", "elevation", "Average_prec"))]

################################################################################
#CORRELATION ANALYSIS
#correlation of the numeric variables
cor <- cor(df_continous)
#is.numeric(df_continous)

# Plot the correlation matrix with hierarchical clustering
my_corplot <- corrplot(cor, order = 'hclust', addrect = 3)
#strong negative correlation btw elevation and average temperature (+higher, cooler) 
#weak correlation btw elevation and precipitation

# Prepare data for statistical analysis
data_stat <- data.frame(df_continous)
#View(data_stat)
temp<-data_stat$Average_temp

# Create a scatter plot with a linear regression line
P <- ggplot(data = data_stat, mapping = aes(x = Average_temp, y = Average_prec))

# Perform Pearson correlation test
cor.test(data_stat$Average_temp, data_stat$Average_prec)
#cor value = -0.26 (p-value 2.2e-16 => significative correlation)
#weak negative relation between temperature and precipitation (high temperature, less precipitation)

# Fit a linear model
linear_model <- lm(Average_temp ~ Average_prec, data = data_stat)
summary(linear_model)
anova(linear_model)
#estimate= -0.022 => same conclusion as correlation test

################################################################################
# create two different matrix for the two species
data_stat_sp1 <- matrix_full[matrix_full$species=="Triturus cristatus",]
data_stat_sp2 <- matrix_full[matrix_full$species=="Triturus carnifex",]
View(data_stat_sp1)

#FACTOR ANALYSIS FOR SPECIES 1
P_fact <- ggplot(data = data_stat_sp1, mapping = aes(x = Landcover, y = Average_temp, fill = Landcover))
P_fact <- P_fact + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 1, colour = "darkgreen") +  # Add mean 
  theme_classic()

print(P_fact)
ggplotly(P_fact)
#This plot permits to see the variation of temperature in the different landcovers fwher especies 1 is present

# linear model with landcover and ecosytems as a factor
model_sp1 <- lm(Average_temp ~ Landcover + W_Ecosystm, data = data_stat_sp1)
print(model_sp1)

#Post-hoc test
anova(model_sp1)
#The temperature variates significantly depending the landcover and the ecosystem type
em_sp1 <- emmeans(linear_model, list(pairwise ~ Landcover), adjust = "tukey")
print(em_sp1)

#FACTOR ANALYSIS FOR SPECIES 2
P_fact2 <- ggplot(data = data_stat_sp2, mapping = aes(x = Landcover, y = Average_temp, fill = Landcover))

P_fact2 <- P_fact2 + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 3, colour = "darkgreen") +  # Add mean 
  theme_classic()

print(P_fact2)
ggplotly(P_fact2)

# Fit a linear model with landcover as a factor
model_sp2 <- lm(Average_temp ~ Landcover+W_Ecosystm, data = data_stat_sp2)

#Post-hoc test
anova(model_sp2)
em_sp2 <- emmeans(linear_model, list(pairwise ~ Landcover), adjust = "tukey")
print(em_sp2)

####################################################################################
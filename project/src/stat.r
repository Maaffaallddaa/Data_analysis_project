###DATA ANALYSIS PROJECT###
#PART8 : statistical analysis
################################################################################
# required libraries
#install.packages("ggcorrplot")
#install.packages("corrplot")
library(ggplot2)
library(ggcorrplot)
library(ggfortify)
library(corrplot)
library(pheatmap)
library(emmeans)

################################################################################
# Data Exploration 
df <- matrix_full
df <- na.omit(df)  # Remove rows with missing values

# Separate continuous and discrete variables
df_continous <- df[, colnames(df) %in% c("temp", "elevation", "NDVI", "prec")]
df_discrete <- df[, !(colnames(df) %in% c("temp", "elevation", "NDVI", "prec"))]

################################################################################
# Correlation Matrix and Corplot
cor <- cor(df_continous)
as.numeric(df_continous)
# Plot the correlation matrix with hierarchical clustering
my_corplot <- corrplot(mydata.cor, order = 'hclust', addrect = 3)

################################################################################
# Correlation between continuous variables 

# Prepare data for statistical analysis
data_stat <- df_continous

# Create a scatter plot with a linear regression line
P <- ggplot(data = data_stat, mapping = aes(x = temp, y = precip))
P + geom_point(shape = 18) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()

P

# Perform Pearson correlation test
cor.test(data_stat$temp, data_stat$precip)

# Fit a linear model
linear_model <- lm(temp ~ precip, data = data_stat)
summary(linear_model)
anova(linear_model)

################################################################################
# Factor Analysis

# Use the original dataset for factor analysis
data_stat <- matrix_full_eco_elev_clim_sat

# Create a boxplot for temperature by landcover type
P_fact <- ggplot(data = data_stat, mapping = aes(x = Landcover, y = temp, fill = Landcover))

P_fact <- P_fact + geom_boxplot(varwidth = TRUE, outlier.shape = NA) +  # Change boxplot width 
  geom_jitter(alpha = 0.2, size = 2, width = 0.1) +  # Add points and spread them
  stat_summary(fun = mean, shape = 13, size = 1, colour = "darkgreen") +  # Add mean 
  theme_classic()

print(P_fact)

library(plotly)
ggplotly(P_fact)
# Fit a linear model with landcover as a factor
linear_model <- lm(temp ~ Landcover, data = data_stat)

####################################################################################
### Post-hoc test

# Perform ANOVA on the linear model
anova(linear_model)

# Conduct post-hoc tests with Tukey adjustment
em <- emmeans(linear_model, list(pairwise ~ Landcover), adjust = "tukey")
print(em)

####################################################################################
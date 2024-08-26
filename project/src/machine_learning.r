###DATA ANALYSIS PROJECT###
#PART9 : prediction with random forest

################################################################################
# Load necessary libraries
install.packages("caret")
install.packages("randomForest")
install.packages("windows")
library(caret)
library(randomForest)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(windows)

################################################################################
# Load the dataset
data_stat <- matrix_full

# Split data into training and testing sets
a <- createDataPartition(data_stat$Landcover, list=FALSE, p = 0.7)
matrix_full_train <- data_stat[a, ]
matrix_full_test <- data_stat[-a, ]

################################################################################
#CONTINOUS DATA
# Create the random forest model using continuous data
output.forest <- randomForest(elevation ~ Average_temp + Average_prec, 
                              data = matrix_full_train, ntree=100)

# Display the importance of each variable in the model
importance(output.forest)  
x11() # Open a new window for plotting
varImpPlot(output.forest)  # Plot variable importance
#the variable average temp is more impactul to predict the elevation

################################################################################
#PREDICTION TEST ON CONTINUOUS DATA
# Predict on the test data
pred_test <- predict(output.forest, newdata = matrix_full_test, type = "class")
#pred_test

# Actual values for comparison
valid_test <- matrix_full_test$Average_temp

# Create a data frame to compare predictions and actual values
matt_pred <- data.frame(valid_test, pred_test)

# Plot predictions vs actual values
P <- ggplot(data = matt_pred, mapping = aes(x = valid_test, y = pred_test)) 
P + geom_point(shape = 18) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()

# Calculate correlation between predictions and actual values
cor.test(valid_test, pred_test)
#cor = -0.7 which indicates that the prediction is negatively correlated with the real values
#this indicates that the model ppredicts the opposite of the real outcomes (?)

# Map the prediction vs real data
matt_pred$latitude <- matrix_full_test$latitude
matt_pred$longitude <- matrix_full_test$longitude

# Retrieve elevation data for Switzerland and Italy
Swi_ita <- ne_countries(scale = "medium", returnclass = "sf", country = c("Switzerland", "Italy"))

# Plot actual elevation values
ggplot(data = Swi_ita) +
  geom_sf() +
  geom_point(data = matrix_full_test, aes(x = longitude, y = latitude, alpha = elevation), 
             size = 4, shape = 16, color = "darkgreen") + 
  theme_classic()

# Plot predicted elevation values 
ggplot(data = Swi_ita) +
  geom_sf() +
  geom_point(data = matt_pred, aes(x = longitude, y = latitude, alpha = pred_test), 
             size = 4, shape = 16, color = "magenta") + 
  theme_classic()

#The elevation values seem similar for between the real values graph and the prediction one

# Calculate prediction accuracy
matt_pred$accuracy <- abs(matt_pred$valid_test - matt_pred$pred_test)

# Plot accuracy on a map of Switzerland
ggplot(data = Swi_ita) +
  geom_sf() +
  geom_point(data = matt_pred, aes(x = longitude, y = latitude, alpha = accuracy), 
             size = 2, shape = 16, color = "darkred") + 
  theme_classic()

################################################################################
#FACTOR DATA

data_stat <- matrix_full

# Split data into training and testing sets
a <- createDataPartition(data_stat$Landcover, list=FALSE, p = 0.7)
matrix_full_train <- data_stat[a, ]
matrix_full_test <- data_stat[-a, ]

# Create the random forest model using factor data
output.forest <- randomForest(as.factor(Landcover) ~ Average_temp + Average_prec, 
                              data = matrix_full_train, ntree=100)

# Display the importance of each variable in the model
importance(output.forest)  
varImpPlot(output.forest) 

# Predict on the test data
pred_test <- predict(output.forest, newdata = matrix_full_test, type = "class")
#pred_test

# Actual values for comparison
valid_test <- matrix_full_test$Landcover

# Create a data frame to compare predictions and actual values
matt_pred <- data.frame(valid_test, pred_test)

# Compute the confusion matrix and see the accuracy score
conf_mat <- confusionMatrix(table(matt_pred$valid_test, matt_pred$pred_test))
#conf_mat 

# Convert confusion matrix to data frame for plotting
conf_mat_data <- as.data.frame(conf_mat$table)

# Plot the confusion matrix
ggplot(data = conf_mat_data, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "yellow", high = "darkgreen") +
  labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
  theme_minimal()
#The values off the diagonal indicate the number of miscalifications
#overall the model permits to correctly prefict the elevation values (a lot of 0 off diagonal)

################################################################################
#PREDICTIONS ON THE FULL DATASET

# Create the random forest model using all variables
output.forest <- randomForest(as.factor(Landcover) ~ ., 
                              data = matrix_full_train, ntree=100)

# Plot variable importance
varImpPlot(output.forest) 
#The most impactful variables to predict the others are W-Ecosystm and Color
#The leat influential is April_temp
#this can be a useful analysis do  reduce the size of the dataset removing the least impactful variables
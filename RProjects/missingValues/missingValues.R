library(dplyr)
library(mltools)
library(data.table)
library(caret)
library(kernlab)
library(caTools)
library(e1071)
library(Metrics)
library(gbm)
library(naivebayes)
library(MASS)
library(ggplot2)

set.seed(42)

# Read in the data, combine the grasslands and forest dataframes
data_grass <- read.csv("/Users/samdulin/Downloads/NCRN_Bird_Monitoring_Data_FOREST_2018.csv", header = TRUE)
data_forest <- read.csv("/Users/samdulin/Downloads/NCRN_Bird_Monitoring_Data_GRASSLAND_2018.csv", header = TRUE)
data_grass <- data_grass[,-c(3)]
data_forest <- data_forest[,-c(28)]
colnames(data_grass)[19] <- c('TaxonCode')
df <- rbind(data_grass, data_forest)

df <- df[, c('Sex', "Location_Type", 'Visit', 'ID_Method', 'Interval_Length', 'TaxonCode', 'Temperature', "Sky", "Wind", "Humidity")]

df$Humidity <- as.numeric(df$Humidity)
df$Temperature <- as.numeric(df$Temperature)
df$Visit <- as.numeric(df$Visit)
df$TaxonCode <- as.numeric(df$TaxonCode)
df$Visit <- as.factor(df$Visit)


# Split the data frame based on if their is a missing value in the sex column
df_ind <- which((df$Sex == 'Male') | (df$Sex == 'Female'))
df_missing <- df[-df_ind,]
df <- df[df_ind,]

# Turn categorical time variable into numeric
ind0 <- which(substr(df$Interval_Length, start = 1, stop = 1) == '0')
ind2 <- which(substr(df$Interval_Length, start = 1, stop = 1) == '2')
ind5 <- which(substr(df$Interval_Length, start = 1, stop = 1) == '5')
ind7 <- which(substr(df$Interval_Length, start = 1, stop = 1) == '7')

df$Interval_Length[ind0] <- 1.25
df$Interval_Length[ind2] <- 3.75
df$Interval_Length[ind5] <- 6.25
df$Interval_Length[ind7] <- 8.75
df$Interval_Length <- as.numeric(df$Interval_Length)

# Define one-hot encoding function
dummy <- dummyVars(" ~ .", data=df)

# Perform one-hot encoding on data frame
data <- data.frame(predict(dummy, newdata=df))

# Keep only one column that indiciates bird gender
data <- data[,-2]
colnames(data)[1] <- 'Sex'

# Split data into train and test
split = sample.split(data$Sex, SplitRatio = 0.7)

training_set = subset(data, split == TRUE)
training_set$Sex <- as.factor(training_set$Sex)
test_set = subset(data, split == FALSE)

run_models <- function(training_set, test_set){
  # Create a dataframe to store performance metrics
  df_performance <- data.frame(matrix(nrow = 3, ncol = 3))
  colnames(df_performance) <- c('Naive Bayes', 'Decision Tree', 'LDA')
  row.names(df_performance) <- c('Precision', 'Recall', 'Accuracy')
  
  # Train the Bayes classifier
  nb = naiveBayes(formula = Sex ~ .,
                  data = training_set)
  
  summary(nb)
  
  # Make predictions
  y_pred = as.numeric(predict(nb, newdata = test_set[,-1]))
  y_pred[y_pred == 1] <- 0
  y_pred[y_pred == 2] <- 1
  y_pred
  
  # Precision and Accuracy
  df_performance[,1] <- c(precision(test_set$Sex, y_pred),
  recall(test_set$Sex, y_pred),
  accuracy(test_set$Sex, y_pred))
  
  # A Gradient Boosted Decision Tree
  fitControl <- trainControl(method = "cv", classProbs = FALSE)
  
  gbm.bird <- train(Sex ~ ., data = training_set, 
                    method = "gbm", 
                    trControl = fitControl,
                    verbose = FALSE)
  gbm.bird
  
  # Make predictions
  y_pred = as.numeric(predict(gbm.bird, newdata = test_set[-1], type = 'raw'))
  y_pred[y_pred == 1] <- 0
  y_pred[y_pred == 2] <- 1
  
  # Precision and Accuracy
  df_performance[,2] <- c(precision(test_set$Sex, y_pred),
  recall(test_set$Sex, y_pred),
  accuracy(test_set$Sex, y_pred))
  
  # LDA classification (Not-Naive Bayes)
  model <- lda(training_set[,-1], grouping=training_set[,1])
  
  # View model output
  model
  
  # Fit LDA model
  y_pred <- predict(model, test_set[,-1])
  y_pred <- as.numeric(y_pred$class)
  y_pred[y_pred == 1] <- 0
  y_pred[y_pred == 2] <- 1
  
  # Precision and Accuracy
  df_performance[,3] <- c(precision(test_set$Sex, y_pred),
  recall(test_set$Sex, y_pred),
  accuracy(test_set$Sex, y_pred))

  return(df_performance)
}

# Run models before using LDA
results_before <- run_models(training_set, test_set)

# LDA feature extraction
model <- lda(data[,-c(1,11)], grouping=data[,11])

# Fit LDA model and transform dataset
y_pred <- predict(model, data[,-c(1,11)])
new_data <- as.data.frame(y_pred$x)
new_data <- cbind(data$Sex, new_data)
colnames(new_data)[1] <- 'Sex'

# Split data into train and test
split = sample.split(new_data$Sex, SplitRatio = 0.75)

training_set = subset(new_data, split == TRUE)
training_set$Sex <- as.factor(training_set$Sex)
test_set = subset(new_data, split == FALSE)

# Run models before using LDA
results_after <- run_models(training_set, test_set)
print(results_after)

##### Plotting the test set results with LDA
nb = naiveBayes(formula = Sex ~ .,
                data = training_set)

set = test_set
X1 = test_set[,2]
X2 = test_set[,3]

grid_set = cbind(X1, X2)
colnames(grid_set) = c('LD1', 'LD2')

plot(set[, 2:3],
     main = 'LDA + Naive Bayes Classifier',
     xlab = 'LD1', ylab = 'LD2',
     xlim = c(min(X1),max(X1)), ylim = c(min(X2),max(X2)))

y_pred = predict(nb, newdata = test_set[,-1])

points(grid_set, pch = 21, bg = ifelse(y_pred == 1, 'orange', 'blue'))


# Now we use PCA instead of LDA to compare
y <- data$Sex
X <- data[, -1]


# Convert to numeric
i=0
while(i < ncol(X)){
  i=i+1  
  X[,i] = as.numeric(X[,i])
}
X <- t(X)

# Normalize the matrix
X_normalized <- scale(X)
X_normalized[is.na(X_normalized)] <- 0

# Compute the covariance matrix
cov_matrix <- cov(X_normalized)

# Find the principal components
res.pca <- prcomp(cov_matrix)

# Get the transformed dataset, and use for loop to find the near-optimal number of principal components. Sum of precision across classifiers is what we are trying to optimize.
PCA_precision_sum <- c()

for (i in 2:20){
  X_t <- as.data.frame(res.pca$rotation)[,1:i]
  new_data <- cbind(y, X_t)
  colnames(new_data)[1] <- 'Sex'
  
  # Split data into train and test
  split = sample.split(new_data$Sex, SplitRatio = 0.75)
  
  training_set = subset(new_data, split == TRUE)
  training_set$Sex <- as.factor(training_set$Sex)
  test_set = subset(new_data, split == FALSE)
  
  results_after <- run_models(training_set, test_set)
  
  PCA_precision_sum <- c(PCA_precision_sum, sum(results_after[1,]))
}

# Find the near-optimal number of components
optim_comps <- which.max(PCA_precision_sum) + 1
X_t <- as.data.frame(res.pca$rotation)[,1:optim_comps]
new_data <- cbind(y, X_t)
colnames(new_data)[1] <- 'Sex'


# Run models before using PCA
results_after <- run_models(training_set, test_set)

##### Plotting the test set results with PCA
nb = naiveBayes(formula = Sex ~ .,
                data = training_set)

set = test_set
X1 = test_set[,2]
X2 = test_set[,3]

grid_set = cbind(X1, X2)
colnames(grid_set) = c('PC1', 'pC2')

plot(set[, 2:3],
     main = 'PCA + Naive Bayes Classifier',
     xlab = 'PC1', ylab = 'PC2',
     xlim = c(min(X1),max(X1)), ylim = c(min(X2),max(X2)))

y_pred = predict(nb, newdata = test_set[,-1])

points(grid_set, pch = 21, bg = ifelse(y_pred == 1, 'orange', 'blue'))




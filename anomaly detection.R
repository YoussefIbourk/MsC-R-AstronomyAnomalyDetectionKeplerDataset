library(randomForest)
library(caret)


data <- read.csv("cumulative_2020.12.30_14.14.11.csv")

# data pre-processing

count_disposition <- table(data$koi_disposition)

count_p_disposition <- table(data$koi_pdisposition)


# exoplanet to precise if an observation is an exoplanet
data$exoplanet <- ifelse(data$koi_disposition == "CONFIRMED" | data$koi_disposition == "CANDIDATE", 1, 0)


set.seed(123)

# partitioning the data into training and testing sets
split_index <- createDataPartition(data$exoplanet, p = 0.8, list = FALSE)

train_data <- data[split_index,]
test_data <- data[-split_index,]

# training the random forest model
randomf_model <- randomForest(exoplanet ~ koi_period + koi_impact + koi_duration + koi_depth + koi_teq + koi_insol + koi_slogg + koi_srad + koi_steff, data = train_data, importance = TRUE, ntree = 1000)

# extract the feature importance
feature_importance <- importance(randomf_model)

# create a data frame with the feature importance only
feature_importance_df <- data.frame(feature = row.names(feature_importance), importance = feature_importance[,1])

# plot the feature importance
ggplot(feature_importance_df, aes(x = reorder(feature, -importance), y = importance)) +
  geom_bar(stat = "identity") +
  xlab("Feature") +
  ylab("Importance") +
  ggtitle("Feature importance")


# predict on the test data
prediction <- predict(randomf_model, test_data)

# the accuracy of the model
accuracy <- mean(prediction == test_data$exoplanet)

# exoplanets extraction
exo_p <- test_data[prediction == 1,]

# the test data anomaly scores 
anomaly_scores <- predict(randomf_model, test_data, type = "response")

# the observations that are anomalies
anomalies <- test_data[anomaly_scores < 0.1,]

# converting the data frame to a numeric vector and then to a numeric matrix
#   anomalies_numeric <- as.numeric(unlist(anomalies))

# remove NA values from the variable before converting it to numeric.
#   anomalies_num <- as.numeric(anomalies_numeric, na.rm = TRUE)


###### plotting anomalous observations ######

ggplot(anomalies, aes(x = koi_period, y = koi_impact, color = exoplanet)) +
  geom_point() +
  ggtitle("Anomalous observations (koi_period / koi_impact)")

ggplot(anomalies, aes(koi_duration)) +
  geom_density(aes(color = "red")) +
  ggtitle("Anomalous observations (koi_duration)")

ggplot(anomalies, aes(x = koi_period, y = koi_impact, group = koi_disposition)) +
  geom_line(aes(color = "red")) +
  ggtitle("Anomalous observations (koi_period / koi_impact / koi_disposition)")

# creating scatter plot matrix using the pairs()
pairs(anomalies[,c("koi_period", "koi_impact", "koi_duration", "koi_depth")], col = anomalies$exoplanet)

# create a violin plot using the geom_violin()
ggplot(anomalies, aes(x = exoplanet, y = koi_period)) +
  geom_violin(fill = "red") +
  ggtitle("Anomalous observations (exoplanet / koi_period)")


# count the number of objects with an anomaly score below the threshold < 0.1
low_score_count <- sum(anomaly_scores < 0.1)

# count the number of these objects
total_count <- nrow(test_data)

# calculate the percentage of objects with a low anomaly scores
low_score_percentage <- low_score_count / total_count

# creating a data frame
pie_data <- data.frame(Label = c("Low anomaly score", "Other"), Value = c(low_score_count, total_count - low_score_count), Percentage = c(low_score_percentage, 1 - low_score_percentage))

# the pie chart
ggplot(pie_data, aes(x = "", y = Value, fill = Label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(Percentage * 100, "%")), position = position_stack(vjust = 0.5)) +
  ggtitle("Anomaly score distribution")


### Basic Housing Prediction Model

###Dataset
#https://www.kaggle.com/datasets/rukenmissonnier/final-house?resource=download

#install.packages(tidyverse)
library(tidyverse)
#install.packages("corrplot")
library(corrplot)
#install.packages("caret")
library(caret)

file_path <- "/Users/samirelkhatib/Desktop/Personal Data Projects/Housing price prediction/house.csv"

housing_data <- read.csv(file_path)

head(housing_data)

summary(housing_data)

### Visuals

housing_data %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal()

cor_matrix <- cor(housing_data)
corrplot(cor_matrix, method = "circle")

housing_data %>%
  ggplot(aes(x = net_sqm, y = price)) +
  geom_point() +
  labs(title = "Scatter Plot of net_sqm vs. price")

### Prediction Model

set.seed(123)

index <- createDataPartition(housing_data$price, p = 0.7, list = FALSE)

train_data <- housing_data[index, ]
test_data <- housing_data[-index, ]

### Predictive Model

model <- lm(price ~ bedroom_count + net_sqm + center_distance + metro_distance + floor + age, data = train_data)
summary(model)

predictions <- predict(model, newdata = test_data)

# Evaluate the model

MAE <- mean(abs(predictions - test_data$price))
MSE <- mean((predictions - test_data$price)^2)
R2 <- 1 - (sum((predictions - test_data$price)^2) / sum((mean(test_data$price) - test_data$price)^2))

# Print evaluation metrics
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("R-squared (R2):", R2, "\n")

### New data (Feel free to add your own data to test the model)

new_data <- data.frame(
  bedroom_count = c(2),
  net_sqm = c(100),
  center_distance = c(1200),
  metro_distance = c(80),
  floor = c(10),
  age = c(50)
)

new_predictions <- predict(model, newdata = new_data)

# Print the predicted price
cat("Predicted Price:", new_predictions, "\n")


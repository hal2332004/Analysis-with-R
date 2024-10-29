# 0. Load library needed
library(ggplot2)
library(tidyr)
library(plotly)
library(lubridate)
library(corrplot)
library(forecast)
library(tidyverse)
library(caret) # L1/L2
library(glmnet) #cross-validation/grid search
library(xgboost)


# 1. Read data and preprocess data
df <- read.csv("D:/Ky 4/R_code/GoldUP.csv/GoldUP.csv")
colSums(is.na(df))
df$Date <- as.Date(df$Date, format = "%d-%m-%Y")
df$Month <- as.integer(format(df$Date, "%m"))
df$Year <- as.integer(format(df$Date, "%Y"))

# 2. Descriptive statistics
summary(df)
###
min_price <- min(df$Gold_Price, na.rm = TRUE)
max_price <- max(df$Gold_Price, na.rm = TRUE)
p <- ggplot(df, aes(x=Date)) +
  geom_point(aes(y=Gold_Price, color="Gold Price")) +
  labs(title="Giá vàng (2006-2024)",
       x="Ngày",
       y="Giá",
       color="Chỉ số") +
  theme_minimal() +
  ylim(min_price, max_price)

interactive_plot <- ggplotly(p)
interactive_plot
### Line Chart
columns_to_normalize <- c("Gold_Price", "Crude_Oil", "Interest_Rate", "USD_INR", "Sensex", "CPI", "USD_Index")
df_normalized <- df
df_normalized[columns_to_normalize] <- lapply(df[columns_to_normalize], function(x) {
  (x - mean(x)) / sd(x)
})

p <- ggplot(df_normalized, aes(x = Date)) +
  geom_line(aes(y = Gold_Price, color = "Gold Price (Z-Score)")) +
  geom_line(aes(y = USD_Index, color = "USD Index (Z-Score)")) +
  geom_line(aes(y = Crude_Oil, color = "Crude Oil (Z-Score)")) +
  geom_line(aes(y = Interest_Rate, color = "Interest Rate (Z-Score)")) +
  geom_line(aes(y = USD_INR, color = "USD/INR (Z-Score)")) +
  geom_line(aes(y = Sensex, color = "Sensex (Z-Score)")) +
  geom_line(aes(y = CPI, color = "CPI (Z-Score)")) +
  labs(title = "Z-Score Normalized Economic Indicators (2006-2024)",
       x = "Date",
       y = "Z-Score Values",
       color = "Index") +
  theme_minimal()

interactive_plot <- ggplotly(p)
interactive_plot

### Heat map
columns_to_use <- c("Gold_Price", "Crude_Oil", "Interest_Rate", "USD_INR", "Sensex", "CPI", "USD_Index")
cor_matrix <- cor(df[columns_to_use], use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))
cor_df <- cor_df[as.numeric(cor_df$Var1) < as.numeric(cor_df$Var2), ]
p <- ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  geom_text(aes(label = round(Freq, 2)), color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables")


interactive_plot <- ggplotly(p)
interactive_plot

## Pearson
cor_cpi <- cor.test(df$CPI, df$Gold_Price, method = "pearson")
print(cor_cpi)
cor_usd_index <- cor.test(df$USD_Index, df$Gold_Price, method = "pearson")
print(cor_usd_index)



## Anova
linear_model <- lm(Gold_Price ~ Crude_Oil + Interest_Rate + USD_INR + Sensex + CPI + USD_Index, data = df)
anova_results <- anova(linear_model)
print(anova_results)




### Time series Plot
gold_ts <- ts(df$Gold_Price, start = c(2000, 1), frequency = 12) # monthly data
decomp <- decompose(gold_ts)
plot(decomp)

####
gold_ts <- ts(df$Gold_Price, start = c(2000, 1), frequency = 12)
decomp <- decompose(gold_ts, type = "additive")
seasonal_component <- decomp$seasonal

seasonal_data <- data.frame(
  Month = rep(1:12, length.out = length(seasonal_component)),
  Seasonal = seasonal_component
)

ggplot(seasonal_data, aes(x = Month, y = Seasonal)) +
  geom_bar(stat = "identity", fill = "gold") +
  ggtitle("Seasonal Component of Gold Price by Month") +
  xlab("Month") + ylab("Seasonal Effect") +
  scale_x_continuous(breaks = 1:12) + 
  theme_minimal()


#### Model ###
X <- df[, c("Crude_Oil", "Interest_Rate", "USD_INR", "Sensex", "CPI", "USD_Index")] 
y <- df$Gold_Price

set.seed(123)  
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]
set.seed(123)
lm_model <- train(
  Gold_Price ~ ., 
  data = df[trainIndex,], 
  method = "lm", 
  trControl = trainControl(method = "cv", number = 5)  # 5-fold Cross-Validation
)


X_train_matrix <- as.matrix(X_train)
X_test_matrix <- as.matrix(X_test)
grid <- expand.grid(alpha = c(0, 1, length = 100),  # 0: Ridge, 1: Lasso
                    lambda = seq(0.001, 1, length = 100))
set.seed(123)
ridge_lasso_model <- train(
  x = X_train_matrix, 
  y = y_train, 
  method = "glmnet", 
  tuneGrid = grid, 
  trControl = trainControl(method = "cv", number = 5)
)
print(ridge_lasso_model)


predictions <- predict(ridge_lasso_model, newdata = X_test_matrix)
mse <- mean((y_test - predictions)^2)
r2 <- 1 - sum((y_test - predictions)^2) / sum((y_test - mean(y_test))^2)
rmse <- sqrt(mse)

cat("MSE:", mse, "\n")
cat("R^2:", r2, "\n")
cat("RMSE:", rmse, "\n")

best_params <- ridge_lasso_model$bestTune
print(best_params)

df_results <- data.frame(
  Date = df$Date[-trainIndex],  
  Gold_Price = y_test,           
  Predicted = predictions     
)

ggplot(df_results, aes(x = Date)) +
  geom_line(aes(y = Gold_Price, color = "Actual")) +          
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed") +  
  ggtitle("Actual vs Predicted Gold Price using CPI, USD_INR, and Sensex") +
  xlab("Date") + 
  ylab("Gold Price") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +  
  theme_minimal()

#### XGB
gold_ts <- ts(df$Gold_Price, start = c(2000, 10), frequency = 12)
decomp <- decompose(gold_ts, type = "additive")
seasonal_component <- decomp$seasonal

seasonal_data <- data.frame(
  Month = rep(1:12, length.out = length(seasonal_component)),
  Seasonal = seasonal_component
)

ggplot(seasonal_data, aes(x = Month, y = Seasonal)) +
  geom_bar(stat = "identity", fill = "gold") +
  ggtitle("Seasonal Component of Gold Price by Month") +
  xlab("Month") + ylab("Seasonal Effect") +
  scale_x_continuous(breaks = 1:12) + 
  theme_minimal()

#


X <- df[, c("Crude_Oil", "Interest_Rate", "USD_INR", "Sensex", "CPI", "USD_Index")]
y <- df$Gold_Price

set.seed(123)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

X_train_matrix <- as.matrix(X_train)
X_test_matrix <- as.matrix(X_test)

xgb_grid <- expand.grid(
  nrounds = 100,           # Number of boosting rounds
  max_depth = c(3, 6, 9, 12),  # Depth of trees
  eta = c(0.01, 0.05, 0.1, 0.2, 0.3), # Learning rate
  gamma = 0,               # Minimum loss reduction
  colsample_bytree = 1,    # Subsampling ratio of columns
  min_child_weight = 1,    # Minimum sum of instance weight (hessian) needed in a child
  subsample = 1            # Subsample ratio of the training instance
)

set.seed(123)
xgb_model <- train(
  x = X_train_matrix,
  y = y_train,
  method = "xgbTree",
  tuneGrid = xgb_grid,
  trControl = trainControl(method = "cv", number = 5)
)

print(xgb_model)
predictions <- predict(xgb_model, newdata = X_test_matrix)

mse <- mean((y_test - predictions)^2)
r2 <- 1 - sum((y_test - predictions)^2) / sum((y_test - mean(y_test))^2)
rmse <- sqrt(mse)

cat("MSE:", mse, "\n")
cat("R^2:", r2, "\n")
cat("RMSE:", rmse, "\n")

best_params <- xgb_model$bestTune
print(best_params)

df_results <- data.frame(
  Date = df$Date[-trainIndex],
  Gold_Price = y_test,
  Predicted = predictions
)

ggplot(df_results, aes(x = Date)) +
  geom_line(aes(y = Gold_Price, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted"), linetype = "dashed") +
  ggtitle("Actual vs Predicted Gold Price using CPI, USD_INR, and Sensex") +
  xlab("Date") +
  ylab("Gold Price") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal()








# Read the dataset
data <- read.csv("~/STAT 512 GROUP PROJECT/spotify-2023.csv")

data$streams <- as.numeric(data$streams)

data$key <- as.factor(data$key)

data <- na.omit(data)

model <- lm(streams ~ energy_. + liveness_. + key + energy_.*liveness_., data = data)

summary(model)
anova(model)
nrow(data)


#TEST

# Histogram of residuals
hist(residuals(model), main="Histogram of Residuals", xlab="Residuals", breaks=30, col="lightblue")

# Q-Q plot of residuals
qqnorm(residuals(model))
qqline(residuals(model), col="red")

# Assuming 'energy_' and 'liveness_' are your key variables, you can also check these for normality

# Histogram of 'energy_'
hist(data$energy_, main="Histogram of Energy", xlab="Energy", breaks=30, col="lightgreen")

# Q-Q plot of 'energy_'
qqnorm(data$energy_)
qqline(data$energy_, col="red")

# Histogram of 'liveness_'
hist(data$liveness_, main="Histogram of Liveness", xlab="Liveness", breaks=40, col="lightcoral")

# Q-Q plot of 'liveness_'
qqnorm(data$liveness_)
qqline(data$liveness_, col="red")


#SCATER PLOT

# Scatter plot for streams vs. energy_
plot(data$energy_, data$streams, main = "Streams vs. Energy", xlab = "Energy", ylab = "Streams", pch = 19, col = "blue")

# Scatter plot for streams vs. liveness_
plot(data$liveness_, data$streams, main = "Streams vs. Liveness", xlab = "Liveness", ylab = "Streams", pch = 19, col = "red")

boxplot(streams ~ key, data = data, main = "Streams by Key", xlab = "Key", ylab = "Streams", las = 2, col = rainbow(length(unique(data$key))))

# Calculate residuals
residuals <- resid(model)

# Calculate fitted values
fitted.values <- fitted(model)

# Plot residuals
plot(fitted.values, residuals, main = "Residuals vs. Fitted", xlab = "Fitted Values", ylab = "Residuals", pch = 19)
abline(h = 0, col = "red")

head(data,50)


# Normality Test for Residuals with Shapiro-Wilk Test
shapiro_test <- shapiro.test(residuals(model))
print(shapiro_test)

# Variance Inflation Factor (VIF) to check for multicollinearity
library(car)
vif_result <- vif(model)
print(vif_result)

# Leverage Plot to identify influential points
influencePlot(model)

# Durbin-Watson Test for Autocorrelation
library(lmtest)
dw_test <- dwtest(model)
print(dw_test)

library(lmtest)
bptest(model)







# Cook distance
cooksd <- cooks.distance(model)
plot(cooksd, type="h", main="Cook's Distance", ylab="Cook's distance")
abline(h=4/(nrow(data)-length(coef(model))), col="red")  # Threshold line




#box cox transformaiton 

library(MASS)  # Load MASS for the boxcox function
boxcox_result <- boxcox(model, lambda = seq(-2, 2, by=0.1))
lambda_opt <- boxcox_result$x[which.max(boxcox_result$y)]
data$streams_transformed <- data$streams^lambda_opt
transformed_model <- lm(streams_transformed ~ energy_. + liveness_. + key + energy_.*liveness_., data = data)
summary(transformed_model)





#Kfold 

library(caret)
set.seed(123)  # for reproducibility
folds <- createFolds(data$streams, k = 10)
cv_results <- lapply(folds, function(x) {
  training_set <- data[-x, ]
  test_set <- data[x, ]
  model <- lm(streams ~ energy_. + liveness_. + key, data = training_set)
  return(mean((predict(model, test_set) - test_set$streams)^2))  # returning MSE for each fold
})
mean(unlist(cv_results))  # average MSE across all folds



#DFBETA and DFFITS

dfbetas_values <- dfbetas(model)
plot(dfbetas_values, main="DFBETAs", ylab="DFBETAs values")

dffits_values <- dffits(model)
plot(dffits_values, type="h", main="DFFITS", ylab="DFFITS values")
abline(h=c(-2, 2) * sqrt(2/length(data$streams)), col="red")  # Threshold lines



# Variance Inflation Factor
library(car)
vif_values <- vif(model)
print(vif_values)  # Print VIF values to check for multicollinearity

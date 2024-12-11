data <- read.csv("DataSet.csv")

##Data Exploration 
str(data) 
summary(data)
library(knitr)

summary_data <- as.data.frame(summary(data))
kable(summary_data, caption = "Summary of Data", format = "markdown")

##Data Preprocessing/Cleaning/Dealing with missing values
#Step1: if conversion of products is 0, then sales and revenue=0, otherwise leave as NA

data$MonthlySales <- ifelse(data$Top.3.ASINs.Total.Conv..Share == 0 & data$Top.3.ASINs.Total.Conv..Share.Trend == 0 
                       & data$Top.1.ASIN.Conv..Share == 0
                       & data$Top.1.ASIN.Conv..Share.Trend == 0
                       & data$Top.2.ASIN.Conv..Share == 0
                       & data$Top.2.ASIN.Conv..Share.Trend == 0
                       & data$Top.3.ASIN.Conv..Share == 0
                       & data$Top.3.ASIN.Conv..Share.Trend == 0, 0, data$Top.3.ASINs.Total.Monthly.Sales)

data$MonthlyRevenue <- ifelse(data$Top.3.ASINs.Total.Conv..Share == 0 & data$Top.3.ASINs.Total.Conv..Share.Trend == 0 
                       & data$Top.1.ASIN.Conv..Share == 0
                       & data$Top.1.ASIN.Conv..Share.Trend == 0
                       & data$Top.2.ASIN.Conv..Share == 0
                       & data$Top.2.ASIN.Conv..Share.Trend == 0
                       & data$Top.3.ASIN.Conv..Share == 0
                       & data$Top.3.ASIN.Conv..Share.Trend == 0, 0, data$Top.3.ASINs.Total.Monthly.Revenue)

#Checking difference in missing values
summary(data$MonthlyRevenue)
summary(data$MonthlySales)

#Our main variables are listing age, revenue, sales, review count and rating
#if all these are NA even after Step1, we omit the rows instead of imputing

ImpData <- ImpData <- c("MonthlyRevenue", "MonthlySales", "Top.3.Clicked.ASINs.Monthly.Average.Age", 
                        "Top.3.ASINs.Total.Review.Count", "Top.3.ASIN.Total.Average.Rating")

data_cleaned <- data[!apply(data[, ImpData], 1, function(x) all(is.na(x))), ]

#Our main variables are listing age, revenue, sales, review count and rating
#if review count, rating and age are still NA, we omit the rows instead of imputing

ImpData2 <- c("Top.3.Clicked.ASINs.Monthly.Average.Age", 
                        "Top.3.ASINs.Total.Review.Count", "Top.3.ASIN.Total.Average.Rating")

data_cleaned2 <- data_cleaned[!apply(data_cleaned[, ImpData2], 1, function(x) all(is.na(x))), ]


install.packages("VIM")
library(VIM)
library(dplyr)

data_cleaned3 <- data_cleaned2[, !colnames(data_cleaned2) %in% 
                                 c("Top.3.ASINs.Total.Monthly.Sales", 
                                   "Top.3.ASINs.Total.Monthly.Revenue")]

#if sales, review and rating is NA - remove
ImpData3 <- c("MonthlySales", 
              "Top.3.ASINs.Total.Review.Count", "Top.3.ASIN.Total.Average.Rating")

data_cleaned4 <- data_cleaned3[!apply(data_cleaned3[, ImpData3], 1, function(x) all(is.na(x))), ]

#if sales and revenue is NA - remove
ImpData4 <- c("MonthlySales","MonthlyRevenue")

data_cleaned5 <- data_cleaned4[!apply(data_cleaned4[, ImpData4], 1, function(x) all(is.na(x))), ]

summary(data_cleaned5$Top.3.Clicked.ASINs.Monthly.Average.Age)
summary(data_cleaned5$Top.3.ASINs.Total.Review.Count)
summary(data_cleaned5$Top.3.ASIN.Total.Average.Rating)
summary(data_cleaned5$MonthlySales)
summary(data_cleaned5$MonthlyRevenue)

#####
# Example using linear regression to predict missing values
model <- lm(Top.3.Clicked.ASINs.Monthly.Average.Age ~ MonthlySales + Competing.Products +
              Search.Volume + Top.3.ASINs.Total.Review.Count, data = data_cleaned5)
data_cleaned5$Top.3.Clicked.ASINs.Monthly.Average.Age[is.na(data_cleaned5$Top.3.Clicked.ASINs.Monthly.Average.Age)] <- 
  predict(model, newdata = data_cleaned5[is.na(data_cleaned5$Top.3.Clicked.ASINs.Monthly.Average.Age), ])

summary(model)

data_cleaned5 <- data_cleaned5[data_cleaned5$Top.3.Clicked.ASINs.Monthly.Average.Age >= 0, ]


#For all remaining NA values, we use clustering - better than imputing because of the nature of dat
imputed_data <- kNN(data_cleaned5, k = 3)
View(imputed_data)


#Remove unwanted columns
#Final-Cleaned Data
finaldf <- imputed_data[, !grepl("imp", names(imputed_data))]

summary(finaldf)

#Export data set
write.csv(finaldf,"E:/UTD MSBA/Fall 2024/Business Analytics with R/Project/finaldf.csv", row.names = FALSE)


##Finding relationship between variables through corr matrix
numeric_data <- finaldf[sapply(finaldf, is.numeric)]
correlation <- cor(numeric_data); correlation

install.packages("plotly")
library(plotly)
plot_ly(z = correlation, type = "heatmap", colors = colorRamp(c("blue", "white", "red")))

plot_ly(
  z = correlation,              # Data for the heatmap
  x = colnames(correlation),    # Column names for x-axis
  y = rownames(correlation),    # Row names for y-axis
  type = "heatmap",            # Type of plot
  colors = colorRamp(c("blue", "white", "red"))  # Color scale
) %>%
  layout(
    title = "Correlation Matrix",  # Title of the plot
    xaxis = list(title = "Variables"),  # x-axis label
    yaxis = list(title = "Variables")   # y-axis label
  )


#renaming columns for ease
print(colnames(finaldf))

colnames(finaldf) <- c("kw","svol", "svoltrend", "sfreqr", 
                     "sfreqt", "t3tot_click","t3tot_ctrend","t3tot_conv","t3tot_convtrend",
                     "t1id", "t1click","t1ctrend","t1conv","t1convtrend",
                     "t2id","t2click","t2ctrend","t2conv","t2convtrend",
                     "t3id","t3click","t3ctrend","t3conv","t3convtrend",
                     "comp_prod","t3avg_mage","t3tot_rev","t3tot_rat","msales","mrev")

# Change the order of columns
finaldf <- finaldf[, c(1, 10, 15, 20,2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 16, 17, 18, 
                   19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)]

install.packages("tidyr")
library(tidyr)

#side by side histograms
# Select columns 5 to 30
subset_data <- finaldf[, 5:30]

# Reshape data to a long format
long_data <- subset_data %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

library(ggplot2)
library(tidyr)
library(scales)
# Plot side-by-side histograms
ggplot(long_data, aes(x = Value)) +
  geom_histogram(bins = 30, color = "black", fill = "grey", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6.5)) +
  labs(title = "Side-by-Side Histograms", x = "Value", y = "Frequency")

#### scatter plots with sales
# Select the columns from 5 to 29, (Sales : 29th column)
df_subset <- finaldf[, c(5:29)]  

# Gather the data into a long format for ggplot
df_long <- df_subset %>%
  gather(key = "Variable", value = "Value", -msales)

####
# Create the scatter plots with small, filled circles and trend lines
ggplot(df_long, aes(x = Value, y = msales)) +
  geom_point(alpha = 0.6, color = "grey", size = 1, shape = 16) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  facet_wrap(~ Variable, scales = "free", ncol = 4) +  
  labs(x = "Variable Value", y = "msales") +
  theme_bw() +
  theme(strip.text = element_text(size = 8),   
        axis.text.x = element_text(size = 8),    
        axis.text.y = element_text(size = 8)) 


##log-transformations after observing variables that have large range AND are skewed

# Specify columns to log-transform
columns_to_transform <- c("comp_prod","msales","sfreqr","svol","svoltrend","t3tot_rev",
                          "t3tot_click","t3tot_conv","t1click","t1conv","t2click","t2conv",
                          "t3click","t3conv","t3avg_mage")

# Apply log transformation, handling zeros, negatives, and ensuring positivity
for (col in columns_to_transform) {
  min_value <- min(finaldf[[col]], na.rm = TRUE) 
  
  if (min_value <= 0) {
    # Shift values to make them all positive
    shift_value <- abs(min_value) + 1
    transformed_values <- finaldf[[col]] + shift_value
  } else {
    # No need to shift for strictly positive columns
    transformed_values <- finaldf[[col]]
  }
  
  # Replace any non-positive or NA values (if present) with a small positive value
  transformed_values[transformed_values <= 0 | is.na(transformed_values)] <- 1e-6
  
  # Add log-transformed values as a new column
  finaldf[[paste0("log_", col)]] <- log(transformed_values)
}

## histograms of log-transformed variables

# Load necessary libraries
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Select log-transformed columns
log_columns <- grep("^log_", colnames(finaldf), value = TRUE)

# Reshape finaldf for ggplot2
finaldf_long <- finaldf[, log_columns]  
finaldf_long <- reshape2::melt(finaldf_long)  

# Create side-by-side histograms
ggplot(finaldf_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  facet_wrap(~variable, scales = "free", ncol = 4) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10)  
  ) +
  labs(
    title = "Histograms of Log-Transformed Variables",
    x = "Log-Transformed Values",
    y = "Frequency"
  )

# Calculate skewness for log-transformed variables
log_skewness <- sapply(finaldf[log_columns], skewness, na.rm = TRUE)
print(log_skewness)

# Calculate skewness for log-transformed variables
log_skewness <- sapply(finaldf[5:30], skewness, na.rm = TRUE)
print(log_skewness)

##Scaling for linear regression

finaldf2 <- finaldf[,-(1:4)]
finaldf2 <- sapply(finaldf2,scale)
finaldf2 <- data.frame(finaldf2)

### multiple linear regression

# partition
set.seed(123)  
train.index <- sample(c(1:dim(finaldf2)[1]), dim(finaldf2)[1]*0.6)  
train.df <- finaldf2[train.index, ]
valid.df <- finaldf2[-train.index, ]



# using lm() to run a linear regression of msales on selected predictors in the
# training set. 

library(forecast)
##domain knowledge
dmodel <- lm(log_msales ~ log_svol + log_svoltrend + sfreqr +
                   log_t1click + t1ctrend +  t1convtrend +
                   log_t2click + t2ctrend  +  t2convtrend +
                   log_t3click + t3ctrend +  t3convtrend +
                   log_comp_prod + t3avg_mage + log_t3tot_rev + t3tot_rat, data = train.df)
summary(dmodel)
prediction <- predict(dmodel, valid.df)
accuracy(prediction,valid.df$log_msales)
AIC(dmodel)

#full model with all variables:
full_model <- lm(log_msales ~ log_svol + log_svoltrend + sfreqr +
                   log_t1click + t1ctrend + log_t1conv +  t1convtrend +
                   log_t2click + t2ctrend + log_t2conv +  t2convtrend +
                   log_t3click + t3ctrend + log_t3conv +  t3convtrend +
                   log_comp_prod + t3avg_mage + log_t3tot_rev + t3tot_rat, data = train.df)
summary(full_model)
prediction <- predict(full_model, valid.df)
accuracy(prediction,valid.df$log_msales)
AIC(full_model)

#create model with no predictors
df.lm.null <- lm(log_msales~1, data = train.df)

# use step() to run forward regression.
df.lm.stepf <- step(df.lm.null, scope=list(lower=df.lm.null, upper=full_model), direction = "forward")
summary(df.lm.stepf)  

# predicting forward with validation set
df.lm.step.predf <- predict(df.lm.stepf, valid.df)
accuracy(df.lm.step.predf, valid.df$log_msales)


# use step() to run backward regression.
df.lm.stepb <- step(full_model, direction = "backward")
summary(df.lm.stepb)  

# predicting backward with validation set
df.lm.step.predb <- predict(df.lm.stepb, valid.df)
accuracy(df.lm.step.predb, valid.df$log_msales)


# use step() to run 'both' regression.
df.lm.stepfb <- step(full_model, direction = "both")
summary(df.lm.stepfb)  

# predicting both with validation set
df.lm.step.predfb <- predict(df.lm.stepfb, valid.df)
accuracy(df.lm.step.predfb, valid.df$log_msales)

###CHECK ###
original_sds <- sapply(data, sd)
coefficients <- coef(model)
original_coefficients <- coefficients[-1] * original_sds
names(original_coefficients) <- names(coefficients[-1])

## 
install.packages("gains") 
library(gains)

actual = valid.df$log_msales

#lift for forward
gain1 = gains(actual, 
              df.lm.step.predf,
              group = 10)

plot(c(0, gain1$cume.pct.of.total*sum(actual))~c(0, gain1$cume.obs), type = "l", 
     xlab = "#Cases", ylab = "Cumulative MSales", main = "Lift Chart for forwards")
segments(0, 0, nrow(valid.df), sum(actual), lty = "dashed", col = "red", lwd = 2)

## lift for backward/both (same)
gain1 = gains(actual, 
              df.lm.step.predfb,
              group = 10)

plot(c(0, gain1$cume.pct.of.total*sum(actual))~c(0, gain1$cume.obs), type = "l", 
     xlab = "#Cases", ylab = "Cumulative MSales", main = "Lift Chart for both")
segments(0, 0, nrow(valid.df), sum(actual), lty = "dashed", col = "red", lwd = 2)

##choosing backward model - checking residuals
final.model <- lm(formula = log_msales ~ log_svol + log_svoltrend + t1ctrend + 
     log_t1conv + log_t2click + log_t2conv + t2convtrend + log_t3conv + 
     t3convtrend + log_comp_prod + log_t3tot_rev + t3tot_rat, data = train.df)


#Residual Analysis
fitted_values <- predict(final.model,valid.df)
actual <- valid.df$log_msales
residuals <- actual - fitted_values

# Residuals vs. Fitted Values
plot(fitted_values, residuals,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# Histogram of Residuals
hist(residuals, breaks = 20,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "blue", border = "black")













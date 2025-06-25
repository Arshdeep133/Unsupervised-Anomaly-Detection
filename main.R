#set the working directory for your R session
getwd()
setwd("C:/Users/Tushar/Desktop/cmpt-318/cmpt318-project")

#The table() function reads a TXT file into a data frame in R.
#The header = TRUE argument means that the first row of the TXT file contains column names.
raw_data <- read.table("TermProjectData.txt", header = TRUE, sep = ",")

raw_data$Date <- as.Date(raw_data$Date, format = "%d/%m/%Y")

# Filter rows based on the date range to convert into weeks
df <- raw_data[raw_data$Date >= as.Date("2006-12-17") & raw_data$Date <= as.Date("2009-11-28"), ]

colSums(is.na(df))
library(zoo)

min_date <- min(df$Date)
df$Week <- as.integer(1 + floor(as.double(difftime(df$Date, min_date, units="weeks"))))

# Count the number of NAs for 'Global_reactive_power' for each date
na_counts_by_date <- tapply(is.na(df$Global_reactive_power), df$Date, sum)
na_counts_sorted <- sort(na_counts_by_date, decreasing = TRUE)
top_5_dates <- names(na_counts_sorted)[1:5]
top_5_week <- df$Week[df$Date %in% top_5_dates]
# Remove rows corresponding to these top 10 dates
df <- df[!df$Week %in% top_5_week, ]

#check for NA in all attributes
na_rows_date <- is.na(df$Date)
na_rows_time <- is.na(df$Time)
na_rows_active_power <- is.na(df$Global_active_power)
na_rows_reactive_power <- is.na(df$Global_reactive_power)
na_rows_voltage <- is.na(df$Voltage)
na_rows_intensity <- is.na(df$Global_intensity)
na_rows_SM1 <- is.na(df$Sub_metering_1)
na_rows_SM2 <- is.na(df$Sub_metering_2)
na_rows_SM3 <- is.na(df$Sub_metering_3)

# linear interpolation for NA values
num_null <- colSums(is.na(df)) > 0
df[, num_null] <- na.approx(df[,num_null])

# Feature Scaling using Standardization
#Z Score for each datapoint
df$Z_score <- scale(df[, c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
                           "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")], center = TRUE, scale = TRUE)

#create dataframe for z scores
new_df_temp <- as.data.frame(df$Z_score)
new_df_temp$Date <- df$Date
new_df_temp$Time <- df$Time

new_df_temp <- new_df_temp[c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
                             "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]

new_df <- new_df_temp

# remove outliers
outlier_mask <- apply(new_df_temp[, c("Global_active_power", "Global_reactive_power", "Voltage", 
                                      "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")], 
                      1, function(row) any(abs(row) > 3))

new_df_temp[outlier_mask, c("Global_active_power", "Global_reactive_power", "Voltage", 
                            "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")] <- NA
num_null <- colSums(is.na(new_df_temp)) > 0
new_df_temp[, num_null] <- na.approx(new_df_temp[,num_null])


new_df <- new_df_temp
library(ggplot2)
numerical_df <- new_df[, c("Global_active_power", "Global_reactive_power", 
                           "Voltage", "Global_intensity", 
                           "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]
pca <- prcomp(numerical_df, scale. = TRUE)


# Summary of PCA results
print(summary(pca))

#library(ggbiplot)

#plot <- ggbiplot(pca, obs.scale = 1,var.scale = 1, ellipse = TRUE, circle = TRUE,         
 #        title = "PCA Biplot of Electricity Consumption Responses") + theme_minimal()

#print(plot)

# Prints out the order of the top variables for PCA1 AND PCA2
loading_scores1 <- pca$rotation[,1]
loading_scores2 <- pca$rotation[,2]

energy_scores1 <- abs(loading_scores1)  
energy_scores2 <- abs(loading_scores2)  

energy_scores_ranked1 <- sort(energy_scores1, decreasing = TRUE)
energy_scores_ranked2 <- sort(energy_scores2, decreasing = TRUE)

top_energy1 <- names(energy_scores_ranked1[1:7])
top_energy2 <- names(energy_scores_ranked2[1:7])

print("Top variables for PCA1:")
print(top_energy1)

print("Top variables for PCA2:")
print(top_energy2)


nrow(new_df)
nrow(raw_data)
selected_data <- new_df[, c("Global_active_power", "Global_intensity", 
                            "Global_reactive_power", "Sub_metering_3")]

new_df$Date <- raw_data$Date[as.numeric(rownames(new_df))]#Making sure the date is in date format
new_df$Date <- as.Date(new_df$Date, format = "%d/%m/%Y")
train_data<- new_df[format(new_df$Date,"%Y")%in% c("2006","2007","2008"),] #training set
test_data<-new_df[format(new_df$Date,"%Y")=="2009",] #testing set


train_data<-train_data[weekdays(train_data$Date)=="Wednesday",]
test_data<-test_data[weekdays(test_data$Date)=="Wednesday",]

train_data <- train_data[as.POSIXct(train_data$Time, format = "%H:%M:%S") >= as.POSIXct("12:00:00", format = "%H:%M:%S") &
                           as.POSIXct(train_data$Time, format = "%H:%M:%S") <= as.POSIXct("14:00:00", format = "%H:%M:%S"), ]

test_data <- test_data[as.POSIXct(test_data$Time, format = "%H:%M:%S") >= as.POSIXct("12:00:00", format = "%H:%M:%S") &
                         as.POSIXct(test_data$Time, format = "%H:%M:%S") <= as.POSIXct("14:00:00", format = "%H:%M:%S"), ]
print(nrow(train_data))
print(nrow(test_data))


# Calculate ntimes for train_data
ntimes_train <- table(train_data$Date)  # Count rows for each Wednesday in train data
ntimes_train <- as.numeric(ntimes_train)  # Convert to numeric vector

# Calculate ntimes for test_data
ntimes_test <- table(test_data$Date)  # Count rows for each Wednesday in test data
ntimes_test <- as.numeric(ntimes_test)  # Convert to numeric vector

# Validate the results
print(ntimes_train)  # Sequence lengths for train data
print(ntimes_test)   # Sequence lengths for test data

sum(ntimes_train) == nrow(train_data)  # Should return TRUE
sum(ntimes_test) == nrow(test_data)  # Should return TRUE


library(depmixS4)



#Using PCA-selected variables for HMM
selected_data <- train_data[, c("Global_active_power", "Global_intensity", 
                                "Global_reactive_power", "Sub_metering_3")]

#selected_data <- na.omit(selected_data)
selected_data<-round(selected_data*2)/2 # Discretize train data
#selected_data <- discretize_data(selected_data, method = "equal_width", bins = 5)  # Discretize data
#selected_data <- as.data.frame(lapply(selected_data, as.factor))

selected_data <- data.frame(lapply(selected_data, as.numeric))

# making a function to train hmm models with varying states
train_hmm<- function(data,num_states){
  model<-depmix(
    response = list(
      Global_active_power ~ 1,
      Global_intensity ~ 1,
      Global_reactive_power ~ 1,
      Sub_metering_3 ~ 1
    ),
    data = data,
    nstates = num_states,
    ntimes = ntimes_train,
    family = list(multinomial(), multinomial(), multinomial(), multinomial()) 
  )
  
  fit<-fit(model)
  list(log_val=logLik(fit),BIC_val=BIC(fit),model=fit)
}
state_range <- 8
state_range <- seq(4,8,by=2)
hmm_results <- lapply(state_range, function(states) {
  tryCatch({
    train_hmm(selected_data, states)
  }, error = function(e) {
    cat("Error for states =", states, ":", e$message, "\n")
    NULL
  })
})

# Filter out unsuccessful models
hmm_results <- Filter(Negate(is.null), hmm_results)

valid_indices <- which(!sapply(hmm_results, is.null))
# Extract log-likelihood and BIC values for comparison
metrics <- data.frame(
  States = state_range[valid_indices],
  LogLikelihood = sapply(hmm_results[valid_indices], function(res) res$log_val),
  BIC = sapply(hmm_results[valid_indices], function(res) res$BIC_val)
)

# Print the metrics for comparison
print(metrics)
# Select the best model (lowest BIC)
best_valid_index <- which.min(metrics$BIC)
best_model_index <- valid_indices[best_valid_index]
best_model <- hmm_results[[best_model_index]]$model
print(best_model)

if (!best_model_index %in% valid_indices) {
  stop("Error: `best_model_index` does not correspond to a valid model.")
}

if (is.null(best_model)) {
  stop("Error: Failed to retrieve the best model.")
}


# Evaluate the best model on the test data
test_selected_data <- test_data[, c("Global_active_power", "Global_intensity", 
                                    "Global_reactive_power", "Sub_metering_3")]
test_selected_data <- na.omit(test_selected_data)
#test_selected_data <- discretize_data(test_selected_data, method = "equal_width", bins = 5)  # Discretize data
test_selected_data<-round(test_selected_data*2)/2
#test_selected_data <- as.data.frame(lapply(test_selected_data, as.factor))

test_model <- depmix(
  response = list(
    Global_active_power ~ 1,
    Global_intensity ~ 1,
    Global_reactive_power ~ 1,
    Sub_metering_3 ~ 1
  ),
  data = test_selected_data,          # Scaled test data
  nstates = best_model@nstates,       # Use the number of states from the trained model
  ntimes = ntimes_test,               # Sequence lengths for test data
  family = list(multinomial(), multinomial(), multinomial(), multinomial())
)
test_model <- setpars(test_model, getpars(best_model))

# Evaluate the best trained model on test data
test_loglik <- logLik(test_model)
# Normalize the log-likelihood for comparison
normalized_test_loglik <- test_loglik / nrow(test_selected_data)

# Evaluate the log-likelihood on the training data
train_loglik <- logLik(best_model)

# Normalize the log-likelihood for the training data
normalized_train_loglik <- train_loglik / nrow(selected_data)
# Print the results
cat("Normalized Log-Likelihood (Test Data):", normalized_test_loglik, "\n")
# Print the results for training data
cat("Normalized Log-Likelihood (Train Data):", normalized_train_loglik, "\n")


# Partiton test data into 10 weeks
  unique_indicies <- which(!duplicated(test_data[1]))
  test_week<-list()
  for (i in 1:min(10,length(unique_indicies)-1)){
    test_week[[i]]<-test_selected_data[unique_indicies[i]:(unique_indicies[i+1]-1),]
  }
  threshold_deviation=0
  for (i in 1:min(10,length(unique_indicies))){
    week_test<-data.frame(test_week[[i]])
    # Ignore columns with only 1 unique value, else the code errors
    factor_columns <- c("Global_active_power", "Global_intensity", "Global_reactive_power", "Sub_metering_3")
    valid_factors <- sapply(week_test[, factor_columns], function(x) length(unique(x)) > 1)
    response_vars <- factor_columns[valid_factors]
    if (length(response_vars) == 0) {
      message("Skipping test week ", i, " because all factors have only one level.")
      next  # Skip to the next iteration
    }  
    response_list <- lapply(response_vars, function(var) as.formula(paste(var, "~ 1")))
    
    test_week_model <- depmix(
      response = response_list,
      data = week_test,          # Scaled test data
      nstates = best_model@nstates,
      family = rep(list(multinomial()), length(response_vars))
    )
    test_week_loglik <- logLik(test_week_model)
    normalized_test_week_loglik <- test_week_loglik / nrow(week_test)
    print(normalized_test_week_loglik)
    if(threshold_deviation<abs(normalized_test_week_loglik-normalized_train_loglik)){
      threshold_deviation<-abs(normalized_test_week_loglik-normalized_train_loglik)
    }
  }
  lower_threshold<-normalized_train_loglik-threshold_deviation
  upper_threshold<-normalized_train_loglik+threshold_deviation
  print(threshold_deviation)
  print(lower_threshold)
  print(upper_threshold)
  print(normalized_train_loglik)
  
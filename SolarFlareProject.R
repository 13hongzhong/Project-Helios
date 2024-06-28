library(readxl)
library(class)


Solar_Flares_04_05 <- read_excel("C:\\Users\\levia\\Downloads\\Solar_flare_RHESSI_2004_05.xlsx")

#Function to estimate flare intensity based on total.count and given location (X,Y)
estimate_intensity_KNN <- function(data, X_pos, Y_pos, k = 10) {
  
  # Calculate distances based on X_pos and Y_pos
  distances <- sqrt((data$x.pos.asec - X_pos)^2 + (data$y.pos.asec - Y_pos)^2)

  # Find k nearest neighbors
  nearest_indices <- order(distances)[1:k]
  nearest_data <- data[nearest_indices, ]
  
  # Remove missing values in Total_Counts column
  non_missing_mean <- mean(na.omit(nearest_data$total.counts))
  nearest_data$total.counts[is.na(nearest_data$total.counts)] <- non_missing_mean
  
  # Calculate KDE on Total Counts of the nearest neighbors
  kde_result <- density(nearest_data$total.counts)
  # concentration of peaks indicates:
  # high intensity if number of peaks is high (numerous high peaks)
  # low intensity if number of peaks is low (numerous low peaks)
  concentration_of_peaks = mean(kde_result$y)
  
  print(concentration_of_peaks)
  
  # Calculate average Total Counts of nearest neighbors
  mean_number_of_peaks <- mean(nearest_data$total.counts)
  
  #If average number of peak flares of k flares is below average in the total dataset
  #If peak count is low/below average, make concentration and intensity inversely related
  if(mean_number_of_peaks < mean(Solar_Flares_04_05$total.counts)) {
    concentration_of_peaks = -(concentration_of_peaks)
  }
  
  #Estimate intensity based on average number of peaks and concentration of peaks
  intensity <- mean_number_of_peaks * concentration_of_peaks
  
  # Return the intensity estimate
  return(intensity)
}

# Create a subset for months 1 to 4 (January to April)
Solar_Flares_04_05$dt.start <- as.Date(Solar_Flares_04_05$dt.start, format = "%Y-%m-%d %H:%M:%S")
Subset_Jan_Apr_04 <- subset(Solar_Flares_04_05, format(dt.start, "%Y-%m") %in% c("2004-01", "2004-02", "2004-03", "2004-04"))
Subset_Sep_Dec_05 <- subset(Solar_Flares_04_05, format(dt.start, "%Y-%m") %in% c("2005-09", "2005-10", "2005-11", "2005-12"))

#Create random sample of 50 (to reduce complexity)
set.seed(42)  # You can use any integer value as the seed

# Get the row indices of 30 random samples from Subset (Play with random sample number)
random_indices <- sample(nrow(Subset_Jan_Apr_04), 1)
random_indices <- sample(nrow(Subset_Sep_Dec_05), 50)


# Create a new data frame with 30 random samples
random_samples_Jan_Apr_04 <- Subset_Jan_Apr_04[random_indices, ]
random_samples_Sep_Dec_05 <- Subset_Sep_Dec_05[random_indices, ]


# Get unique X_pos and Y_pos values from the dataset
X_pos_values_Jan_Apr_04 <- unique(random_samples_Jan_Apr_04$x.pos.asec)
Y_pos_values_Jan_Apr_04 <- unique(random_samples_Jan_Apr_04$y.pos.asec)

X_pos_values_Sep_Dec_05 <- unique(random_samples_Sep_Dec_05$x.pos.asec)
Y_pos_values_Sep_Dec_05 <- unique(random_samples_Sep_Dec_05$y.pos.asec)

#list of intensitys and locations (X,Y) for each subset
intensity_list_Jan_Apr_04 <- list()

# Iterate through unique X_pos and Y_pos values and compute intensity using the estimate_intensity function
#Jan - Apr
for (i in 1:length(X_pos_values_Jan_Apr_04)) {
  for (j in 1:length(Y_pos_values_Jan_Apr_04)) {
    X_pos <- X_pos_values_Jan_Apr_04[i]
    Y_pos <- Y_pos_values_Jan_Apr_04[j]
    
    # Estimate intensity using the function
    intensity <- estimate_intensity_KNN(Solar_Flares_04_05, X_pos, Y_pos)
    intensity_entry <- list(X_pos = X_pos, Y_pos = Y_pos, intensity = intensity)
    intensity_list_Jan_Apr_04 <- c(intensity_list_Jan_Apr_04, list(intensity_entry))
    # Store the KDE density in the intensity map
    #intensity_map[j, i] <- mean(intensity$KDE_density)
  }
}

# Same thing for second subset (Sept 2005- Dec 2005)
#list of intensitys and locations (X,Y) for each subset
intensity_list_Sep_Dec_05 <- list()

# Iterate through unique X_pos and Y_pos values and compute intensity using the estimate_intensity function
#Jan - Apr
for (i in 1:length(X_pos_values_Sep_Dec_05)) {
  for (j in 1:length(Y_pos_values_Sep_Dec_05)) {
    X_pos <- X_pos_values_Sep_Dec_05[i]
    Y_pos <- Y_pos_values_Sep_Dec_05[j]
    
    # Estimate intensity using the function
    intensity <- estimate_intensity_KNN(Solar_Flares_04_05, X_pos, Y_pos)
    intensity_entry <- list(X_pos = X_pos, Y_pos = Y_pos, intensity = intensity)
    intensity_list_Sep_Dec_05 <- c(intensity_list_Sep_Dec_05, list(intensity_entry))
    # Store the KDE density in the intensity map
    #intensity_map[j, i] <- mean(intensity$KDE_density)
  }
}


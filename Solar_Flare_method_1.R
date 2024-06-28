library(readxl)
library(class)
library(ggplot2)


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

intensity_estimation <- function(data, X_pos, Y_pos, k=5) {
  # Calculate distances based on X_pos and Y_pos
  distances <- sqrt((data$x.pos.asec - X_pos)^2 + (data$y.pos.asec - Y_pos)^2)
  
  # Find k nearest neighbors
  nearest_indices <- order(distances)[1:k]
  
  nearest_data <- data[nearest_indices, ]

  density <- density(nearest_data$total.counts)
  max_density_indices <- which.max(density$y)[1]

  
  total_count_intensity <- density$x[max_density_indices]
  return(total_count_intensity)
  
}


create_intensity_map <- function(dataset) {
  #dataset$intensity <- intensity_estimation(dataset)
  
  data <- data.frame(x = x_Jan_Apr, y = y_Jan_Apr, intensity = intensity_values_Jan_Apr)

  breaks <- quantile(dataset$intensity, probs = seq(0, 1, length.out = 501))
  colors <- rainbow(length(breaks) - 1)
  
  # Find colors for each intensity value
  #col_indices <- findInterval(dataset$intensity, breaks, all.inside = TRUE)
  #dataset$color <- colors[col_indices]
  
  ggplot(dataset, aes(x = x.pos.asec, y = y.pos.asec, color = intensity)) +
    geom_point(size = 3) +
    scale_color_continuous(guide = "legend", breaks = breaks, labels = NULL, 
                           low = colors[1], high = colors[length(colors)]) +
    theme_minimal() +
    labs(title = "Flare Intensity Map (Jan-Apr 2004)", x = "X-Axis Label", 
         y = "Y-Axis Label", color = "Intensity")
}

# Create a subset for months 1 to 4 (January to April)
Solar_Flares_04_05$dt.start <- as.Date(Solar_Flares_04_05$dt.start, format = "%Y-%m-%d %H:%M:%S")
Subset_Jan_Apr_04 <- subset(Solar_Flares_04_05, format(dt.start, "%Y-%m") %in% c("2004-01", "2004-02", "2004-03", "2004-04"))
Subset_Sept_Dec_05 <- subset(Solar_Flares_04_05, format(dt.start, "%Y-%m") %in% c("2005-09", "2005-10", "2005-11", "2005-12"))


result_df <- data.frame(x.pos.asec = numeric(0), y.pos.asec = numeric(0), intensity = numeric(0))

for(i in 1:nrow(Subset_Jan_Apr_04)) {
  x <- Subset_Jan_Apr_04$x.pos.asec[i]
  y <- Subset_Jan_Apr_04$y.pos.asec[i]
  intensity <- intensity_estimation(Subset_Jan_Apr_04, x, y)
  
  # Add a row to the result_df data frame
  result_df <- rbind(result_df, data.frame(x.pos.asec = x, y.pos.asec = y, intensity = intensity))
}


breaks <- quantile(result_df$intensity, probs = seq(0, 1, length.out = 50))
colors <- heat.colors(length(breaks) - 1)

# Find colors for each intensity value
col_indices <- findInterval(result_df$intensity, breaks, all.inside = TRUE)
point_colors <- colors[col_indices]

plot(result_df$x.pos.asec, result_df$y.pos.asec, col = "white", pch = 20, cex = 2, main = "Flare Intensity Estimation Jan-Apr 2004 (Method 1)")
points(result_df$x.pos.asec, result_df$y.pos.asec, col = point_colors, pch = 20, cex = 2)

legend("topright", legend = c("Low", "Medium", "High"), fill = custom_colors, title = "Intensity",horiz = TRUE, cex = 0.4)



result_df_2 <- data.frame(x.pos.asec = numeric(0), y.pos.asec = numeric(0), intensity = numeric(0))

for(i in 1:nrow(Subset_Sept_Dec_05)) {
  x <- Subset_Sept_Dec_05$x.pos.asec[i]
  y <- Subset_Sept_Dec_05$y.pos.asec[i]
  intensity <- intensity_estimation(Subset_Sept_Dec_05, x, y)
  
  # Add a row to the result_df data frame
  result_df_2 <- rbind(result_df, data.frame(x.pos.asec = x, y.pos.asec = y, intensity = intensity))
}

breaks <- quantile(result_df$intensity, probs = seq(0, 1, length.out = 50))
colors <- heat.colors(length(breaks) - 1)

# Find colors for each intensity value
col_indices <- findInterval(result_df_2$intensity, breaks, all.inside = TRUE)
point_colors <- colors[col_indices]

plot(result_df_2$x.pos.asec, result_df_2$y.pos.asec, col = "white", pch = 20, cex = 2, main = "Flare Intensity Estimation Sep-Dec 2005 (Method 1)")
points(result_df_2$x.pos.asec, result_df_2$y.pos.asec, col = point_colors, pch = 20, cex = 2)

legend("topright", legend = c("Low", "Medium", "High"), fill = custom_colors, title = "Intensity",horiz = TRUE, cex = 0.4)



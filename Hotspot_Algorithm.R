
#Hotspot Algorithm

#x and y coordinates
x_min <- min(Solar_Flares_04_05$x.pos.asec)
x_max <- max(Solar_Flares_04_05$x.pos.asec)
y_min <- min(Solar_Flares_04_05$y.pos.asec)
y_max <- max(Solar_Flares_04_05$y.pos.asec)

grid_size <- 60

# Grid of the cells
x_grid <- seq(x_min, x_max, by = grid_size)
y_grid <- seq(y_min, y_max, by = grid_size)


# An empty grid to store the counts
grid_counts <- matrix(0, nrow = length(y_grid) - 1, ncol = length(x_grid) - 1)


# Loops through each cell in the grid
for (i in 1:(length(x_grid) - 1)) {
  for (j in 1:(length(y_grid) - 1)) {
    x1 <- x_grid[i]
    x2 <- x_grid[i + 1]
    y1 <- y_grid[j]
    y2 <- y_grid[j + 1]
    
    # Count the number of points that fall within the current cell
    points_in_cell <- subset(Solar_Flares_04_05, x.pos.asec >= x1 & x.pos.asec < x2 & y.pos.asec >= y1 & y.pos.asec < y2)
    cell_count <- nrow(points_in_cell)
    
    # Store the count in the grid
    grid_counts[j, i] <- cell_count
  }
}

#print(grid_counts)


# Count the number of points in each cell
count_per_cell <- grid_counts



# Determine intensity threshold d1 and d2 for Method1, based on your results for Task1 


#d1


# Find the maximum count in the grid
max_count <- max(grid_counts)

# Set threshold at 50% of the maximum count
threshold <- max_count / 2

# Look for the cells that exceed the threshold
thresholded_grid <- grid_counts > threshold

# Extract cells that meet the threshold
high_density_cells <- grid_counts * thresholded_grid

#print(high_density_cells)

# Total count of cells meeting the threshold
total_high_density_count <- sum(high_density_cells)

#print(total_high_density_count)


#d2

# Find the maximum count in the grid
max_count <- max(grid_counts)

# Set threshold at 30% of the maximum count
threshold <- max_count * 0.3 

# Look for the cells that fall below the threshold
below_threshold_grid <- grid_counts < threshold

# Extract cells that meet the threshold
low_density_cells <- grid_counts * below_threshold_grid

#print(low_density_cells)

# Total count of cells meeting the threshold
total_low_density_count <- sum(low_density_cells)

#print(total_low_density_count)

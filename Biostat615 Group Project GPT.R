# Function to manually calculate distances
calculate_distance <- function(data1, data2, metric = "euclidean") {
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  dist_matrix <- matrix(0, nrow = n1, ncol = n2)
  
  # Calculate distance for each pair of points
  for (i in 1:n1) {
    for (j in 1:n2) {
      if (metric == "euclidean") {
        # Euclidean distance
        dist_matrix[i, j] <- sqrt(sum((data1[i, ] - data2[j, ])^2))
      } else if (metric == "manhattan") {
        # Manhattan distance
        dist_matrix[i, j] <- sum(abs(data1[i, ] - data2[j, ]))
      } else {
        stop("Unsupported metric!")  # Error if an unsupported metric is provided
      }
    }
  }
  return(dist_matrix)
}

# Define the DBSCAN model structure
dbscan_model <- function(data, eps = 0.5, minPts = 5, metric = "euclidean") {
  model <- list(
    data = data,  # Input data
    eps = eps,  # Neighborhood radius
    minPts = minPts,  # Minimum points to form a dense region
    metric = metric,  # Distance metric
    clusters = NULL,  # Placeholder for cluster labels
    core_points = NULL  # Placeholder for core points
  )
  class(model) <- "dbscan_model"  # Assign a custom class for the model
  return(model)
}

# Fit the DBSCAN model
fit.dbscan_model <- function(model) {
  n <- nrow(model$data)
  dist_matrix <- calculate_distance(model$data, model$data, metric = model$metric)
  
  # Initialize cluster labels and core point markers
  clusters <- rep(0, n)
  core_points <- rep(FALSE, n)
  cluster_id <- 0
  
  # Iterate through all points
  for (i in 1:n) {
    if (clusters[i] != 0) next  # Skip already processed points
    
    # Find neighbors of the current point
    neighbors <- which(dist_matrix[i, ] <= model$eps)
    if (length(neighbors) < model$minPts) {
      clusters[i] <- -1  # Mark as noise
    } else {
      cluster_id <- cluster_id + 1
      clusters[i] <- cluster_id
      core_points[i] <- TRUE
      # Expand the cluster to include density-reachable points
      clusters <- expand_cluster(i, neighbors, cluster_id, clusters, core_points, dist_matrix, model$eps, model$minPts)
    }
  }
  
  # Save results to the model
  model$clusters <- clusters
  model$core_points <- core_points
  return(model)
}

# Helper function to expand a cluster
expand_cluster <- function(current_point, neighbors, cluster_id, clusters, core_points, dist_matrix, eps, minPts) {
  queue <- neighbors
  
  # Process all points in the queue
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    
    if (clusters[current] == -1) {  # If the point is noise, assign it to the cluster
      clusters[current] <- cluster_id
    }
    
    if (clusters[current] == 0) {  # If the point is not yet processed
      clusters[current] <- cluster_id
      current_neighbors <- which(dist_matrix[current, ] <= eps)
      if (length(current_neighbors) >= minPts) {
        core_points[current] <- TRUE
        queue <- c(queue, current_neighbors)  # Add neighbors to the queue
      }
    }
  }
  return(clusters)
}

# Predict cluster labels for new data
predict.dbscan_model <- function(model, new_data) {
  dist_matrix <- calculate_distance(new_data, model$data, metric = model$metric)
  
  # Initialize predictions as noise
  predictions <- rep(-1, nrow(new_data))  
  
  # Check each new point against the core points in the model
  for (i in seq_len(nrow(new_data))) {
    close_core <- which(dist_matrix[i, model$core_points] <= model$eps)
    if (length(close_core) > 0) {
      predictions[i] <- model$clusters[close_core[1]]  # Assign to the cluster of the nearest core point
    }
  }
  return(predictions)
}



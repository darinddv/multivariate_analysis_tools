### Generic Distance Matrix

euclidean_distance <- function(x1, x2) {
  sqrt(sum((x1 - x2)^2))
}

compute_distance_matrix <- function(data_matrix, distance_func) {
  distance_matrix <- as.matrix(dist(data_matrix, method = distance_func))
  
  return(distance_matrix)
}

compute_matching_distance <- function(data_matrix) {
  num_rows <- nrow(data_matrix)
  distance_matrix <- matrix(0, nrow = num_rows, ncol = num_rows)
  
  for (i in 1:num_rows) {
    for (j in 1:num_rows) {
      matches <- sum(data_matrix[i, ] == data_matrix[j, ])
      distance_matrix[i, j] <- matches / ncol(data_matrix)
    }
  }
  distance_matrix <- round(distance_matrix, digits = 2)
  distance_matrix
}

### Gower Similarity/Distance

compute_max_difference <- function(data_matrix) {
  max_diff <- apply(data_matrix, 2, 
                    function(column) max(abs(outer(column, column, "-"))))
  max_diff
}

compute_gower_distance <- function(data_matrix, distance = FALSE) {
  num_rows <- nrow(data_matrix)
  num_columns <- ncol(data_matrix)
  
  differences <- vector("list", length = num_rows * (num_rows - 1) / 2)
  index <- 1
  R <- compute_max_difference(data_matrix)
  for (i in 1:(num_rows)) {
    for (j in 1:num_rows) {
      differences[[index]] <- sum(1-abs(data_matrix[i, ] - data_matrix[j, ])/R)/num_columns
      index <- index + 1
    }
  }
  
  differences <- unlist(differences)
  
  if(distance){
    sqrt(1-matrix(differences,ncol=num_rows))
  }
  else{
    matrix(differences,ncol=num_rows)
  }
}

### Gower Centering

subtract_means <- function(data_matrix) {
  row_means <- rowMeans(data_matrix)
  col_means <- colMeans(data_matrix)
  matrix_mean <- mean(data_matrix)
  
  # Subtract row means from each row
  subtracted_rows <- t(t(data_matrix) - row_means)
  
  # Subtract column means from each column
  subtracted_cols <- subtracted_rows - col_means
  
  # Subtract the mean of the entire matrix
  result <- round(subtracted_cols + matrix_mean,2)
  result
}

### Compute Eigendecomposition Using Trimmed Eigenvalues

compute_eigendecomposition <- function(data_matrix) {
  eigen_result <- eigen(data_matrix)
  eigenvalues <- zapsmall(unname(unlist(eigen_result[1])))
  eigen_result$vectors <- eigen_result$vectors %*% diag(sqrt(eigenvalues))
  return(eigen_result$vectors)
}

### Compute Full Ordination

compute_ordination <- function(data_matrix) {
  # Check if the input matrix is square
  if (!is.matrix(data_matrix) || nrow(data_matrix) != ncol(data_matrix)) {
    stop("Input matrix must be square.")
  }
  
  # Compute a_hi
  ahi_matrix <- -1/2 * data_matrix^2
  
  # Compute delta_1 (subtract means)
  subtract_means <- function(x) {
    row_means <- rowMeans(x)
    col_means <- colMeans(x)
    x - row_means - col_means + mean(x)
  }
  centered_matrix <- subtract_means(ahi_matrix)
  
  # Compute eigenvalues and eigenvectors
  #eigenvalues <- zapsmall(unname(unlist(eigen(delta_1)$values)))
  #eigenvectors <- as.data.frame(compute_eigenvectors(delta_1))
  decomposition <- compute_eigendecomposition(centered_matrix)
  return(decomposition)
}


# Binarize matrix
transform_matrix <- function(data_matrix) {
  result_matrix <- data_matrix
  last_col <- ncol(data_matrix)
  
  for (col in 1:(last_col - 1)) {
    result_matrix[, col] <- ifelse(data_matrix[, col] %in% c(0, 1), 0,
                                   ifelse(data_matrix[, col] %in% c(2, 3), 1, data_matrix[, col]))
  }
  
  return(result_matrix)
}


order_similarity_pairs <- function(similarity_matrix) {
  # Get the number of rows and columns in the similarity matrix
  num_rows <- nrow(similarity_matrix)
  num_cols <- ncol(similarity_matrix)
  
  # Compute the indices of the upper triangle elements of the similarity matrix
  upper_triangle_indices <- upper.tri(similarity_matrix)
  
  # Generate a sequence of row and column indices for the upper triangle elements
  row_indices <- row(similarity_matrix)[upper_triangle_indices]
  col_indices <- col(similarity_matrix)[upper_triangle_indices]
  
  # Combine the row and column indices to create unique pairs
  pairs <- paste(row_indices, col_indices)
  
  # Get the corresponding similarity values
  similarity_values <- similarity_matrix[upper_triangle_indices]
  
  # Create a data frame with the unique pairs and their similarity values
  ordered_pairs_df <- data.frame(Pair = pairs, Similarity = similarity_values)
  
  # Split the 'Pair' column into two columns
  split_pairs <- strsplit(ordered_pairs_df$Pair, " ")
  ordered_pairs_df <- data.frame(Object1 = sapply(split_pairs, `[`, 1),
                                 Object2 = sapply(split_pairs, `[`, 2),
                                 Similarity = ordered_pairs_df$Similarity)
  
  # Sort the data frame by similarity in descending order
  ordered_pairs_df <- ordered_pairs_df[order(-ordered_pairs_df$Similarity), ]
  
  return(ordered_pairs_df)
}
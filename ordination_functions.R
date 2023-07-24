### Generic Distance Matrix

euclidean_distance <- function(x1, x2) {
  sqrt(sum((x1 - x2)^2))
}

compute_distance_matrix <- function(data_matrix, distance_func) {
  distance_matrix <- as.matrix(dist(data_matrix, method = distance_func))
  
  return(distance_matrix)
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


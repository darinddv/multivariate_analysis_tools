### Scatterplot Helper

plot_pca <- function(data, x_column, y_column, color_column = NULL) {
  require(ggplot2)
  if (!is.null(color_column)) {
    ggplot(data, aes(x = .data[[x_column]], y = .data[[y_column]], color = .data[[color_column]])) +
      geom_point() +
      labs(x = x_column, y = y_column, color = color_column) +
      scale_color_manual(values = c("deeppink", "dodgerblue")) +
      ggtitle("Scatterplot of PCoA Components")
  } else {
    ggplot(data, aes(x = .data[[x_column]], y = .data[[y_column]])) +
      geom_point() +
      labs(x = x_column, y = y_column) +
      ggtitle("Scatterplot of PCoA Components")
  }
}

# Ordered heatmap

heatmap_ordered <- function(data, ...) {
  n <- nrow(data)
  m <- ncol(data)
  
  if (!is.matrix(data)) {
    stop("Input 'data' must be a matrix.")
  }
  
  # Create the heatmap using the heatmap function and reverse the order of rows
  heatmap(data[rev(order(1:n)), ], Rowv = NA, Colv = NA, ...)
}
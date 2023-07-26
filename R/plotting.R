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

# coldiss()
# Color plots of a dissimilarity matrix, without and with ordering
#
# License: GPL-2
# Author:  Francois Gillet
#          23 August 2012 - rev. 07 June 2016

"coldiss" <- function(D,
                      nc = 4,
                      byrank = TRUE,
                      diag = FALSE) {
  require(gclus)
  
  D <- as.dist(as.matrix(D))
  
  if (max(D) > 1)
    D <- D / max(D)
  
  if (byrank) {
    spe.color <- dmat.color(1 - D, cm.colors(nc))
  }
  else {
    spe.color <- dmat.color(1 - D, byrank = FALSE, cm.colors(nc))
  }
  
  spe.o <- order.single(1 - D)
  speo.color <- spe.color[spe.o, spe.o]
  
  op <- par(mfrow = c(1, 2), pty = "s")
  
  if (diag) {
    plotcolors(
      spe.color,
      rlabels = attributes(D)$Labels,
      main = "Dissimilarity Matrix",
      dlabels = attributes(D)$Labels
    )
    plotcolors(
      speo.color,
      rlabels = attributes(D)$Labels[spe.o],
      main = "Ordered Dissimilarity Matrix",
      dlabels = attributes(D)$Labels[spe.o]
    )
  }
  else {
    plotcolors(spe.color, rlabels = attributes(D)$Labels,
               main = "Dissimilarity Matrix")
    plotcolors(speo.color,
               rlabels = attributes(D)$Labels[spe.o],
               main = "Ordered Dissimilarity Matrix")
  }
  
  par(op)
}

# Usage:
# coldiss(D = dissimilarity.matrix, nc = 4, byrank = TRUE, diag = FALSE)
# If D is not a dissimilarity matrix (max(D) > 1), then D is divided by max(D)
# nc 							number of colours (classes)
# byrank = TRUE		equal-sized classes
# byrank = FALSE	equal-length intervals
# diag = TRUE			print object labels also on the diagonal

# Example:
# coldiss(spe.dj, nc = 9, byrank = FALSE, diag = TRUE)

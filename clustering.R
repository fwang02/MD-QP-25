library(ggplot2)
library(ggdendro)

dd <- read.csv("data_preprocessed.csv", stringsAsFactors = T)

# set a list of numerical variables
numeriques <- which(sapply(dd, is.numeric))
numeriques

dcon <- dd[, numeriques]

# KMEANS CLUSTERING
set.seed(123)
sse <- sapply(1:10, function(k) {
  kmeans(dcon, k)$tot.withinss
})

plot(1:10, sse, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "SSE")

# K-means clustering
k <- 7
k1 <- kmeans(dcon, k)

print(k1)

attributes(k1)

k1$size

k1$withinss

k1$centers

centers_df <- as.data.frame(k1$centers)
centers_df$cluster <- rownames(centers_df)

ggplot(centers_df, aes(x = centers_df[, 3], y = centers_df[, 2], color = cluster)) +
  geom_point(size = 5) +
  labs(
    title = "K-means Cluster Centers",
    x = "Feature 3",
    y = "Feature 2",
    color = "Cluster"
  ) +
  theme_minimal()

ggsave("outputs/clustering/kmeans_centers.png")

# Hierarchical clustering

dist_matrix <- dist(dcon[1:50, ])
h1 <- hclust(dist_matrix, method = "ward.D2")

ggdendrogram(h1, rotate = T)
ggsave("outputs/clustering/hierarchical_clustering.png")

dist_matrix <- dist(dcon)
h1 <- hclust(dist_matrix, method = "ward.D2")

ggdendrogram(h1, rotate = T)
ggsave("outputs/clustering/hierarchical_clustering_all.png")

# Cut the dendrogram
c1 <- cutree(h1, k = 7)

cdg <- aggregate(dcon, by = list(cluster = c1), FUN = mean)

plot(cdg[, 1], cdg[, 5])

# compare numerical variables
for (i in 1:length(numeriques)) {
  for (j in i + 1:length(numeriques)) {
    if (i != j) { # Avoid self-pairing
      p <- ggplot(dcon, aes(x = dcon[, 1], y = dcon[, 2], color = as.factor(c1))) +
        geom_point(size = 1) +
        labs(
          title = "Clustering of data in 7 classes",
          x = names(dcon)[i],
          y = names(dcon)[j],
          color = "Class"
        ) +
        scale_color_brewer(palette = "Set1") +
        theme_minimal(base_size = 8) +
        theme(legend.position = "bottom")

      ggsave(paste0("outputs/clustering/numvsnum/", names(dcon)[i], "_vs_", names(dcon)[j], ".png"), bg = "white", dpi = 300)
    }
  }
}

library(ggplot2)
library(ggdendro)
library(cluster)

dd <- read.csv("data_preprocessed.csv", stringsAsFactors = T)

# set a list of numerical variables
numeriques <- which(sapply(dd, is.numeric))
numeriques

##dcon <- dd[, numeriques]

# KMEANS CLUSTERING
set.seed(123)
sse <- sapply(1:10, function(k) {
  kmeans(dcon, k)$tot.withinss
})

plot(1:10, sse, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters K", ylab = "SSE")

# K-means clustering
k <- 4
k1 <- kmeans(dcon, k)

print(k1)

attributes(k1)

k1$size

k1$withinss

k1$centers

# LETS COMPUTE THE DECOMPOSITION OF INERTIA

Bss <- sum(rowSums(k1$centers^2)*k1$size)
Bss
Wss <- sum(k1$withinss)
Wss
Tss <- k1$totss
Tss

Bss+Wss

Ib1 <- 100*Bss/(Bss+Wss)
Ib1


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


library(cluster)
dist_matrix <- daisy(dd, metric = "gower", stand=TRUE)
distMatrix<-dist_matrix^2
h1 <- hclust(distMatrix, method = "ward.D2")

ggdendrogram(h1, rotate = T)
ggsave("outputs/clustering/hierarchical_clustering_all.png")

# Cut the dendogram
c1 <- cutree(h1, k = 4)

cdg <- aggregate(dcon, by = list(cluster = c1), FUN = mean)

plot(cdg[, 1], cdg[, 5])

# compare numerical variables
for (i in 1:length(numeriques)) {
  for (j in i:length(numeriques)) {
    if (i != j) { # Avoid self-pairing
      p <- ggplot(dcon, aes(x = dcon[, i], y = dcon[, j], color = as.factor(c1))) +
        geom_point(size = 2) +
        labs(
          title = "Clustering of data in 7 classes",
          x = names(dcon)[i],
          y = names(dcon)[j],
          color = "Class"
        ) +
        scale_color_brewer(palette = "Set1") +
        theme_minimal()

      ggsave(paste0("outputs/clustering/numvsnum/", names(dcon)[i], "_vs_", names(dcon)[j], ".png"), bg = "white", dpi = 300, width = 10, height = 6)
    }
  }
}


for (i in 1:length(numeriques)) {
  # Boxplot
  p <- ggplot(dcon, aes(x = as.factor(c1), y = dcon[, i], fill = as.factor(c1))) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Boxplot of ", names(dcon)[i], " by cluster"),
      x = "Class",
      y = names(dcon)[i],
      fill = "Class"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
  ggsave(paste0("outputs/clustering/boxplots/", names(dcon)[i], "_by_cluster.png"), plot = p, bg = "white", dpi = 300, width = 10, height = 6)
}

#tests
anova_results <- lapply(names(dcon), function(var) {
  formula <- as.formula(paste(var, "~ c1"))  
  anova_test <- aov(formula, data = dcon)  
  summary(anova_test) 
})

p_values <- sapply(names(dcon), function(var) {
  formula <- as.formula(paste(var, "~ c1"))
  anova_test <- aov(formula, data = dcon)
  summary(anova_test)[[1]][["Pr(>F)"]][1]  
})

p_values <- sort(p_values)
print(p_values)


for (var in names(dcon)) {
  kruskal_result <- kruskal.test(dcon[[var]] ~ as.factor(c1))
  print(paste("Variable:", var))
  print(kruskal_result)
}

dd <- read.csv("data_preprocessed.csv", stringsAsFactors = T)
library(ggplot2)
library(dplyr)
library(rgl)
library(ggbiplot)
library(RColorBrewer)


attach(dd)

# set a list of numerical variables
numeriques <- which(sapply(dd, is.numeric))
numeriques

dcon <- dd[, numeriques]
sapply(dcon, class)

# PRINCIPAL COMPONENT ANALYSIS OF dcon
pc1 <- prcomp(dcon, scale = TRUE)
class(pc1)
attributes(pc1)
print(pc1)
str(pc1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj <- pc1$sdev^2
inerProj
totalIner <- sum(inerProj)
totalIner
pinerEix <- 100 * inerProj / totalIner
pinerEix
barplot(pinerEix)

# Cummulated Inertia in subspaces
barplot(100 * cumsum(pc1$sdev[1:dim(dcon)[2]]^2) / dim(dcon)[2])
percInerAccum <- 100 * cumsum(pc1$sdev[1:dim(dcon)[2]]^2) / dim(dcon)[2]
percInerAccum

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd <- 3

print(pc1)
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE 3 DIMENSIONS
View(pc1$x)
dim(pc1$x)
dim(dcon)
dcon[2000, ]
pc1$x[2000, ]

Psi <- pc1$x[, 1:nd]
dim(Psi)
Psi[2000, ]
# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden <- row.names(dcon)
etiq <- names(dcon)
ze <- rep(0, length(etiq))

# PLOT OF INDIVIDUALS and Projection of variables
Phi <- cor(dcon, Psi)
View(Phi)
k <- 1

for (i in 1:2) {
  k <- k + 1
  for (j in k:3) {
    # eje1<-i
    # eje2<-j

    png(filename = paste0("Individuals", i, " vs ", j, ".png"), width = 1000, height = 600)
    p <- plot(Psi[, i], Psi[, j], type = "n")
    text(Psi[, i], Psi[, j], labels = iden, cex = 0.5)
    axis(side = 1, pos = 0, labels = F, col = "cyan")
    axis(side = 3, pos = 0, labels = F, col = "cyan")
    axis(side = 2, pos = 0, labels = F, col = "cyan")
    axis(side = 4, pos = 0, labels = F, col = "cyan")
    dev.off()
    print(p)

    X <- Phi[, i]
    Y <- Phi[, j]

    png(filename = paste0("Projection_of_variables", i, " vs ", j, ".png"), width = 1000, height = 600)
    p1 <- plot(Psi[, i], Psi[, j], type = "n", xlim = c(min(X, 0), max(X, 0)), ylim = c(-1, 1))
    axis(side = 1, pos = 0, labels = F)
    axis(side = 3, pos = 0, labels = F)
    axis(side = 2, pos = 0, labels = F)
    axis(side = 4, pos = 0, labels = F)
    arrows(ze, ze, X, Y, length = 0.07, col = "blue")
    text(X, Y, labels = etiq, col = "darkblue", cex = 0.7)
    dev.off()
    print(p1)
  }
}

# 3D PLOT
plot3d(Psi[, 1], Psi[, 2], Psi[, 3])

# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# Creation of a new dataframe with the qualitative variables
categoriques <- which(sapply(dd, is.factor))
categoriques
dcat <- dd[, categoriques]

# Create a new directory to save the plots
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Plot the PCA projection for each qualitative variable
for (k in seq_along(dcat)) {
  # Create a data frame with the PCA projection and the qualitative variable
  plot_data <- data.frame(
    x = Psi[, 1],
    y = Psi[, 2],
    varcat = factor(dcat[, k])
  )

  # Center of each group
  group_centers <- aggregate(cbind(x, y) ~ varcat, data = plot_data, FUN = mean)

  # Plot
  p <- ggplot(plot_data, aes(x = x, y = y, color = varcat)) +
    geom_point() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "darkgray") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
    geom_text(data = group_centers, aes(x = x, y = y, label = varcat), color = "black", size = 3) +
    labs(
      title = paste("PCA Projection -", colnames(dcat)[k]),
      x = "PC1", y = "PC2",
      color = colnames(dcat)[k]
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Save the plot
  ggsave(
    filename = paste0("PCA_projection_", colnames(dcat)[k], ".png"),
    plot = p, width = 10, height = 6, dpi = 600, bg = "white",
    path = "outputs"
  )
}

# PCA Biplot

X <- Phi[, 1]
Y <- Phi[, 2]

categorical_columns <- which(sapply(dd, function(x) is.factor(x) | is.character(x)))
colors <- setNames(rainbow(length(categorical_columns)), names(dd)[categorical_columns])

centroids <- do.call(rbind, lapply(names(dd)[categorical_columns], function(var) {
  aggregate(Psi[, 1:2], by = list(Category = dd[[var]]), FUN = mean) %>%
    mutate(Variable = var)
}))

colors <- brewer.pal(9, "Set1")[-9]
colors <- c(colors, brewer.pal(8, "Set2")[c(1, 2, 4, 7)])

ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_segment(aes(x = 0, y = 0, xend = X, yend = Y),
    arrow = arrow(length = unit(0.07, "inches")),
    color = "lightgray"
  ) +
  geom_text(aes(x = X, y = Y, label = etiq), color = "grey", size = 3) +
  geom_text(
    data = centroids,
    aes(x = PC1, y = PC2, label = Category, color = Variable),
    size = 3
  ) +
  scale_color_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "PCA Biplot with Categorical Variables",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Categorical Variable"
  )

ggsave("PCA_biplot_categorical.png", width = 12, height = 6, dpi = 300, bg = "white", path = "outputs")

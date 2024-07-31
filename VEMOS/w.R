data("iris")

#Quitar la columna de especie
iri_subset <- iris[,c("Sepal.Length", "Sepal.Width")]

set.seed(123) # Semilla para reproducibilidad
km_res <- kmeans(iri_subset, centers = 3)
km_res

library(ggplot2)
iris$Cluster <- factor(km_res$cluster)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "K-means con 3 clusters") +
  theme(legend.position = "bottom")




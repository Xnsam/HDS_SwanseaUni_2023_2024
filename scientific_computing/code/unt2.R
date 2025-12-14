library(tidyverse)
library(ggplot2)
library(factoextra)
library(mice)


data <- read_csv('./code/heart_cluster.csv')
head(data)


# remove the columns
remove_columns <- c('...1', 'num')
data <- data %>%
  select(-remove_columns)

# -------- 
# kmeans clustering

# select columns of the numeric data type
kmeans_data <- data %>%
  select(is.numeric)

# check for na values
if(sum(is.na(kmeans_data)) > 0){
  print("Missing values !")
} else {
  print(" No missing values")
}
# impute na values
kmeans_data <- mice(kmeans_data)
kmeans_data <- complete(kmeans_data)
if(sum(is.na(kmeans_data)) > 0){
  print("Missing values !")
} else {
  print(" No missing values")
}

# scale the data data
kmeans_data <- scale(kmeans_data)


# optimal value of k
# Decide how many clusters to look at
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(kmeans_data, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  ) +
  labs(x='Number of clusters', y="WSS", title="Optimal clusters")
plot(scree_plot)


optimal_k <- c(2, 3, 4, 5)

head(kmeans_data)
set.seed(123)
# selected only age and thalach to understand the relation of
# heart disease ~ thalach + age
for(k in optimal_k){
  # Build model with k clusters: km.out
  km.out <- kmeans(kmeans_data, centers = k, nstart = 20)
  
  cluster_data <- data.frame(kmeans_data)
  cluster_data$cluster_id <- as.factor(km.out$cluster)
  
  g <- ggplot(cluster_data, aes(age, thalach, color = cluster_id)) +
    geom_point(alpha = 0.25) +
    labs(x="Age", y="thalach", title=paste("Optimal K ", k))
  plot(g)
}


# Q1 and answer ================================================================
# ==============================================================================


# -------- 
# hierarchial clustering


#  cut the tree using optimal k = 2 for linkage method = average
optimal_k <- c(2, 3, 4, 5)
linkage_method <- c("average", "complete", "ward.D2")


for(k in optimal_k){
  for(link_method in linkage_method){
    res <- hcut(dist(kmeans_data), 
                k = k, 
                hc_method=link_method
                )
    
    # Visualize the dendrogram
    g1 <- fviz_dend(res, rect = TRUE) +
        labs(title=paste("Dendogram: Optimal k", k, "method", link_method))
    plot(g1)
    
    # Visualize the silhouette
    g2 <- fviz_silhouette(res) +
      labs(title=paste("Silhouette: Optimal k", k, "method", link_method))
    plot(g2)
    
  }
}



# Q2 and answer ================================================================
# ==============================================================================


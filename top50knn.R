library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)

top50.data = read.csv('dataset/top50.csv')

colnames(top50.data)
colnames(top50.data) <- tolower(make.names(colnames(top50.data)))
colnames(top50.data)

#top50.data <- top50.data[sample(nrow(top50.data),30),]

top50.data <- top50.data %>% 
    distinct(x, .keep_all = T) %>% 
    remove_rownames() %>%
    column_to_rownames(var = 'x')

str(top50.data)

top50.data <- scale(top50.data)

# View the firt 3 rows of the data
head(top50.data, n = 3)

## ------------------------------------------------------------------------
library(cluster)

distance <- get_dist(top50.data, method = 'euclidean')
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## ----k-means-optimal-clusters-wss, fig.height=3--------------------------
library(factoextra)

fviz_nbclust(top50.data, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)

fviz_nbclust(top50.data, kmeans, method = "wss")
fviz_nbclust(top50.data, kmeans, method = "silhouette")

#gap_stat <- clusGap(top50.data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
#fviz_gap_stat(gap_stat)

## The plot above represents the variance within the clusters. It decreases as k increases, but it can be seen a bend (or "elbow") at k = 4. This bend indicates that additional clusters beyond the fourth have little value.. In the next section, we'll classify the observations into 4 clusters.

## ------------------------------------------------------------------------
# Compute k-means with k = 4
#set.seed(123)
km.res <- kmeans(top50.data, 4, nstart = 25)

## As the final result of k-means clustering result is sensitive to the random starting assignments, we specify *nstart = 25*. This means that R will try 25 different random starting assignments and then select the best results corresponding to the one with the lowest within cluster variation. The default value of *nstart* in R is one. But, it's strongly recommended to compute *k-means clustering* with a large value of *nstart* such as 25 or 50, in order to have a more stable result.

## ------------------------------------------------------------------------
# Print the results
print(km.res)

## The printed output displays:

## ------------------------------------------------------------------------
aggregate(top50.data, by=list(cluster=km.res$cluster), mean)

## ------------------------------------------------------------------------
dd <- cbind(top50.data, cluster = km.res$cluster)
dd <- data.frame(dd)
plt <- dd %>% 
    group_by(cluster) %>% 
    summarise(counts = n()) %>%
    ggplot(aes(x=reorder(cluster,counts), y=counts)) +
    geom_bar(stat = 'identity', fill ='steelblue') +
    geom_text(aes(label=counts), vjust=1.6, color="white", size=4)

print(plt)

## ---- eval = FALSE-------------------------------------------------------
## # Cluster number for each of the observations
## km.res$cluster

## ------------------------------------------------------------------------
head(km.res$cluster, 4)

## ------------------------------------------------------------------------
# Cluster size
km.res$size
# Cluster means
km.res$centers

fviz_cluster(km.res, data = top50.data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

## Correlation between data
top50.data.corr <- cor(top50.data)
corrplot(top50.data.corr, method="number", type="lower")

top50.data %>%
    as_tibble() %>%
    mutate(cluster = km.res$cluster,
           state = row.names(top50.data)) %>%
    ggplot(aes(speechiness., beats.per.minute, color = factor(cluster), label = state)) +
    geom_text()
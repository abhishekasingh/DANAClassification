##################
# Import library #
##################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(cluster)
library(factoextra)
library(clustertend)
library(NbClust)
library(fpc)
library(clValid)
library(pvclust)
library(dendextend)
library(gplots)
library(pheatmap)
library(d3heatmap)

##################
# Import dataset #
##################

top50.data = read.csv('dataset/maindataset/top50.csv')

colnames(top50.data)
colnames(top50.data) <- tolower(make.names(colnames(top50.data)))
colnames(top50.data)

top50.data <- top50.data %>% 
    select(-genre) %>% 
    remove_rownames() %>%
    column_to_rownames(var = 'artist.name')

str(top50.data)

#################################
# Assessing Clustering Tendency #
#################################

#########
# Scale #
#########

top50.data <- scale(top50.data)

####################################
# View the firt 3 rows of the data #
####################################

head(top50.data, n = 3)

#################################
# Visual inspection of the data #
#################################

fviz_pca_ind(prcomp(top50.data), title = "PCA - Iris data",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

#############################
# Compute Hopkins statistic #
#############################
hopkins.res <- get_clust_tendency(top50.data, n = nrow(top50.data)-1, graph = FALSE)
hopkins.res$hopkins_stat

print(paste("The H value is above threshold value 0.5 and is", round(hopkins.res$hopkins_stat,3)))

##################
# Visual methods #
##################

fviz_dist(dist(top50.data), show_labels = FALSE)+
    labs(title = "Top 50 Spotify Songs - 2019")

###########################
# Hierarchical Clustering #
###########################

factoextra::fviz_dend(hclust(dist(top50.data), "ward.D2"), 
                      main = "Hierarchical Clustering", sub = "", cex = 0.6)

#########################################
# Agglomerative Hierarchical Clustering #
#########################################

res.dist <- dist(top50.data, method = "euclidean")

as.matrix(res.dist)[1:5, 1:5]

###########
# Linkage #
###########

####################
# 1. Single Method #
####################

single.res.hc <- hclust(d = res.dist, method = "single")

### Verify the cluster tree
# Compute cophentic distance
single.res.coph <- cophenetic(single.res.hc)

# Correlation between cophenetic distance and the original distance
cor(res.dist, single.res.coph)

######################
# 2. Complete Method #
######################

complete.res.hc <- hclust(d = res.dist, method = "complete")

### Verify the cluster tree
# Compute cophentic distance
complete.res.coph <- cophenetic(complete.res.hc)

# Correlation between cophenetic distance and the original distance
cor(res.dist, complete.res.coph)

#####################
# 3. Average Method #
#####################

average.res.hc <- hclust(d = res.dist, method = "average")

### Verify the cluster tree
# Compute cophentic distance
average.res.coph <- cophenetic(average.res.hc)

# Correlation between cophenetic distance and the original distance
cor(res.dist, average.res.coph)

#####################
# 3. Ward Method #
#####################

ward.res.hc <- hclust(d = res.dist, method = "ward.D2")

### Verify the cluster tree
# Compute cophentic distance
ward.res.coph <- cophenetic(ward.res.hc)

# Correlation between cophenetic distance and the original distance
cor(res.dist, ward.res.coph)

##############
# Dendrogram #
##############

fviz_dend(single.res.hc, 
          cex = 0.5,
          main = "Dendrogram - Single Linkage")

fviz_dend(complete.res.hc, 
          cex = 0.5,
          main = "Dendrogram - Complete Linkage")

fviz_dend(average.res.hc, 
          cex = 0.5,
          main = "Dendrogram - Average Linkage")

fviz_dend(ward.res.hc,
          cex = 0.5,
          main = "Dendrogram - ward.D2 Linkage")

###############################################
# Determining The Optimal Number Of Clusters: #
###############################################

################
# Elbow method #
################
fviz_nbclust(top50.data, kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method")

#####################
# Silhouette method #
#####################

fviz_nbclust(top50.data, kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method")

#################
# Gap statistic #
#################

fviz_nbclust(top50.data, kmeans, nstart = 25,  method = "gap_stat", nboot = 150)+
    labs(subtitle = "Gap statistic method")

#############
# NbClust() #
#############

nb.res <- NbClust(top50.data, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb.res)

###########################################
# Choosing the Best Clustering Algorithms #
###########################################

# Compute clValid
clmethods <- c("kmeans", "hierarchical", "pam")
intern.res <- clValid(top50.data, nClust = 2:6, 
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern.res)

clmethods <- c("kmeans", "hierarchical", "pam")
stab.res <- clValid(top50.data, nClust = 2:6, clMethods = clmethods, 
                validation = "stability")
# Display only optimal Scores
optimalScores(stab.res)

###################################
# Dendrogram with cluster value 2 #
###################################

#####################
# 1. Single Linkage #
#####################

fviz_dend(single.res.hc, k = 2,
          cex = 0.5,
          k_colors = c("#00AFBB", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Dendrogram - Single Linkage",
          )

fviz_cluster(list(data = top50.data, cluster = cutree(single.res.hc, k = 2)),
             palette = c("#00AFBB", "#FC4E07"), 
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE,
             ggtheme = theme_minimal(),
             main = "Cluster Plot - Single Linkage"
             )

######################
# 2. Complete Method #
######################

fviz_dend(complete.res.hc, k = 2,
          cex = 0.5,
          k_colors = c("#00AFBB", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Dendrogram - Complete Linkage"
          )

fviz_cluster(list(data = top50.data, cluster = cutree(complete.res.hc, k = 2)),
             palette = c("#00AFBB", "#FC4E07"), 
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE, ggtheme = theme_minimal(),
             main = "Cluster Plot - Single Linkage"
             )

####################
# 3. Average Method #
####################

fviz_dend(average.res.hc, k = 2,
          cex = 0.5,
          k_colors = c("#00AFBB", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Dendrogram - Average Linkage"
)

fviz_cluster(list(data = top50.data, cluster = cutree(average.res.hc, k = 2)),
             palette = c("#00AFBB", "#FC4E07"), 
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE, ggtheme = theme_minimal(),
             main = "Cluster Plot - Average Linkage"
)

##################
# 4. Ward Method #
##################

fviz_dend(ward.res.hc, k = 2,
          cex = 0.5,
          k_colors = c("#00AFBB", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE,
          main = "Dendrogram - ward.D2 Linkage"
)

fviz_cluster(list(data = top50.data, cluster = cutree(ward.res.hc, k = 2)),
             palette = c("#00AFBB", "#FC4E07"), 
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE, ggtheme = theme_minimal(),
             main = "Cluster Plot - ward.D2 Linkage"
)

####################################
# Divisive Hierarchical Clustering #
####################################

res.diana <- diana(top50.data)
fviz_dend(res.diana, cex = 0.5,
          k = 2,
          palette = "jco"
)

##########################
# Dendrograms comparison #
##########################

# Create dendrograms
single.dend <- as.dendrogram (single.res.hc)
complete.dend <- as.dendrogram (complete.res.hc)
average.dend <- as.dendrogram (average.res.hc)
ward.dend <- as.dendrogram (ward.res.hc)

# Create a list to hold dendrograms
dend_list <- dendlist("Complete" = complete.dend, "Single" = single.dend,
                      "Average" = average.dend, "Ward" = ward.dend)
cors <- cor.dendlist(dend_list)

# Print correlation matrix
round(cors, 2)

# Visualize the correlation matrix
corrplot(cors, "pie", "lower")

# From the cors ward and average has the maximum correlation and second is average and complete 

####################
# Ward and Average #
####################

dendlist(ward.dend, average.dend) %>%
    untangle(method = "step1side") %>%
    entanglement()

dendlist(ward.dend, average.dend) %>%
    untangle(method = "step1side") %>% 
    tanglegram(
        highlight_distinct_edges = FALSE,
        common_subtrees_color_lines = FALSE,
        common_subtrees_color_branches = TRUE 
        )

########################
# Average and Complete #
########################

dendlist(average.dend, complete.dend) %>%
    untangle(method = "step1side") %>%
    entanglement()

dendlist(average.dend, complete.dend) %>%
    untangle(method = "step1side") %>% 
    tanglegram(
        highlight_distinct_edges = FALSE,
        common_subtrees_color_lines = FALSE,
        common_subtrees_color_branches = TRUE 
        )

###########
# Heatmap #
###########

# Default plot
heatmap(top50.data, 
        scale = "none")

# Enhanced heat maps
heatmap.2(top50.data, 
          scale = "none", 
          col = bluered(100), 
          trace = "none", 
          density.info = "none")

# Pretty heat maps
pheatmap(top50.data,
         cutree_rows = 2)

# Interactive heat maps
d3heatmap(top50.data, colors = "RdYlBu",
          k_row = 4,
          k_col = 2
          )
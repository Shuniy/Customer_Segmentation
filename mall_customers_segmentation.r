library(plotrix)
library(ggplot2)
library(purrr)
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)

set.seed(123)

# lOADING DATA SET
column_names  = c('CustomerID', 'Gender', 'Age', 'AnnualIncome', 'SpendingScore')

customer_data = read.csv(file.path(getwd(), "Mall_Customers.csv"), stringsAsFactors = FALSE, col.names = column_names)
str(customer_data)
names(customer_data)

# Checking data
head(customer_data)
summary(customer_data$Age)

sd(customer_data$Age)
summary(customer_data$AnnualIncome)
sd(customer_data$AnnualIncome)
summary(customer_data$Age)

# Gender distribution
gender = table(customer_data$Gender)
barplot(gender, main = "Gender Comparison",
        ylab = "Count",
        xlab = "Gender",
        col = rainbow(2),
        legend = rownames(gender))

pct = round(gender / sum(gender) * 100)
lbs = paste(c("Female", "Male"), " ", pct, "%", sep = " ")

pie3D(gender, labels = lbs,
      main = "Ratio of Female and Male")

# Age 
hist(customer_data$Age,
     col = "steelblue",
     main = "Count of Age Class",
     xlab = "Age Class",
     ylab = "Frequency",
     labels = TRUE)

boxplot(customer_data$Age,
        col = "steelblue",
        main = "Descriptive Analysis of Age")

# Annual Income of the customers
summary(customer_data$AnnualIncome)
hist(customer_data$AnnualIncome,
     col = "steelblue",
     main = "Distribution for Annual Income",
     xlab = "Annual Income Class",
     ylab = "Frequency",
     labels = TRUE)

plot(density(customer_data$AnnualIncome),
     col = "blue",
     main = "Density Plot for Annual Income",
     xlab = "Annual Income Class",
     ylab = "Density")

polygon(density(customer_data$AnnualIncome),
        col = "orange")

summary(customer_data$SpendingScore)
boxplot(customer_data$SpendingScore,
        horizontal = TRUE,
        col = "steelblue",
        main = "BoxPlot for Descriptive Analysis of Spending Score")

hist(customer_data$SpendingScore,
     main = "Histogram for Spending Score",
     xlab = "Spending Score Class",
     ylab = "Frequency",
     col = "steelblue",
     labels = TRUE)

# Using K-Means for segmentation
# Determining K by using elbow method
# iss <- intra cluster sum of squares
iss <- function(k){
    kmeans(customer_data[, 3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd")$tot.withinss
}

k.values <- 1:10
iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Total intra-clusters sum of squares")

# Average Silhouette score method
# k = 2
k_2 <- kmeans(customer_data[, 3:5], 2, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_2 <- plot(silhouette(k_2$cluster, dist(customer_data[, 3:5], "euclidean")))

# k = 3
k_3 <- kmeans(customer_data[, 3:5], 3, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_3 <- plot(silhouette(k_3$cluster, dist(customer_data[, 3:5], "euclidean")))

# k = 4
k_4 <- kmeans(customer_data[, 3:5], 4, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_4 <- plot(silhouette(k_4$cluster, dist(customer_data[, 3:5], "euclidean")))

# k = 5
k_5 <- kmeans(customer_data[, 3:5], 5, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_5 <- plot(silhouette(k_5$cluster, dist(customer_data[, 3:5], "euclidean")))

# k = 6
k_6 <- kmeans(customer_data[, 3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_6 <- plot(silhouette(k_6$cluster, dist(customer_data[, 3:5], "euclidean")))

# k = 7
k_7 <- kmeans(customer_data[, 3:5], 7, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_7 <- plot(silhouette(k_7$cluster, dist(customer_data[, 3:5], "euclidean")))

# k = 8
k_8 <- kmeans(customer_data[, 3:5], 8, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_8 <- plot(silhouette(k_8$cluster, dist(customer_data[, 3:5], "euclidean")))

# k = 9
k_9 <- kmeans(customer_data[, 3:5], 9, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_9 <- plot(silhouette(k_9$cluster, dist(customer_data[, 3:5], "euclidean")))

# k = 10
k_10 <- kmeans(customer_data[, 3:5], 10, iter.max = 100, nstart = 50, algorithm = "Lloyd")
s_10 <- plot(silhouette(k_10$cluster, dist(customer_data[, 3:5], "euclidean")))

# Visualization of silhoutte scores
fviz_nbclust(customer_data[, 3:5], kmeans, method = "silhouette")

# Gap statistic method
stat_gap <- clusGap(customer_data[, 3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

# We will be using 6 clusters
k6 <- kmeans(customer_data[, 3:5], 6, iter.max = 100, nstart = 50, algorithm = "Lloyd")
k6

# Visualizing the cluster with 2 principle components
pcclust = prcomp(customer_data[, 3:5], scale = FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[, 1:2]

# Plotting clusters
ggplot(customer_data, aes(x = AnnualIncome, y = SpendingScore)) + 
    geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
    scale_color_discrete(name = " ",
                         breaks = c("1", "2", "3", "4", "5", "6"),
                         labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
    ggtitle("Segments of Mall Customers by Spending Score and Annual Income", subtitle = "Using K-means Clustering")

ggplot(customer_data, aes(x = SpendingScore, y = Age)) + 
    geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
    scale_color_discrete(name = " ",
                         breaks = c("1", "2", "3", "4", "5", "6"),
                         labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")) +
    ggtitle("Segments of Mall Customers by Age and Spending Score", subtitle = "Using K-means Clustering")

kCols = function(vec){cols = rainbow (length(unique (vec)))
                return (cols[as.numeric(as.factor(vec))])}

digCluster <- k6$cluster; 
dignm <- as.character(digCluster);# K-means clusters

plot(pcclust$x[, 1:2], col = kCols(digCluster), pch = 19, xlab = "K-means", ylab = "classes")
legend("bottomleft", unique(dignm), fill = unique(kCols(digCluster)))

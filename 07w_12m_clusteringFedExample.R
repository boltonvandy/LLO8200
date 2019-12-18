
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load libraries

library(ggplot2)
library(cluster)
library(factoextra)
library(dendextend)
library(dplyr)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load data
FederalistPapers <- read.csv("fedPapers85.csv", na.strings = c(""))

# Create backup of FederalistPapers in case it's needed
FederalistPapers_Orig <- FederalistPapers

# Take a look at the data
View(FederalistPapers)

# Check for missing values
sum(is.na(FederalistPapers))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# K means

# Remove author names from dataset
FedPapers_km <-FederalistPapers[,2:72]

# Make the file names the row names. Need a dataframe of numerical values for k-means
rownames(FedPapers_km) <- FedPapers_km[,1]
FedPapers_km[,1] <- NULL

View(FedPapers_km)

# Optimal number of clusters
fviz_nbclust(FederalistPapers, FUN=hcut,method = "wss")
fviz_nbclust(FederalistPapers, FUN =hcut, method = "silhouette")

# Set seed for fixed random seed
set.seed(20)

# run k-means
Clusters <- kmeans(FedPapers_km, 4)
FedPapers_km$Clusters <- as.factor(Clusters$cluster)

str(Clusters)
Clusters$centers

# Add clusters to dataframe original dataframe with author name
FedPapers_km2 <- FederalistPapers
FedPapers_km2$Clusters <- as.factor(Clusters$cluster)
                                      
# Plot results
clusplot(FedPapers_km, FedPapers_km$Clusters, color=TRUE, shade=TRUE, labels=0, lines=0)

ggplot(data=FedPapers_km2, aes(x=author, fill=Clusters))+
  geom_bar(stat="count") +
  labs(title = "K = 4") +
  theme(plot.title = element_text(hjust=0.5), text=element_text(size=15))

#---------------------------------------------------------------------------------------------------------------------------------------------------------------
# Hierachical Clustering Algorithms (HAC)

# Remove author names from dataset
FedPapers_HAC <- FederalistPapers[,c(2:72)]

# Make the file names the row names. Need a dataframe of numerical values for HAC
rownames(FedPapers_HAC) <- FedPapers_HAC[,1]
FedPapers_HAC[,1] <- NULL

View(FedPapers_HAC)

# Calculate distance in a variety of ways
distance  <- dist(FedPapers_HAC, method = "euclidean")
distance2 <- dist(FedPapers_HAC, method = "maximum")
distance3 <- dist(FedPapers_HAC, method = "manhattan")
distance4 <- dist(FedPapers_HAC, method = "canberra")
distance5 <- dist(FedPapers_HAC, method = "binary")
distance6 <- dist(FedPapers_HAC, method = "cosine")

HAC <- hclust(distance6, method="complete")
plot(HAC, cex=0.6, hang=-1)
rect.hclust(HAC, k =4, border=2:5)

HAC2 <- hclust(distance, method="single")
plot(HAC2, cex=0.6)
rect.hclust(HAC, k =4, border=2:5)

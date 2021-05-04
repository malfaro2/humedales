# https://www.statmethods.net/advstats/cluster.html

## Complete analyis for ConHum

# Prepare Data
load("data_clean/data_clean.Rdata")
source("code/packages.R")
summary(ConHum)
mydata <- na.omit(ConHum[,-2]) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 5) # 5 cluster solution

# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")


clustersConHum <- ConHum %>% 
  select(Formulario:Name) %>% 
  mutate(groups=groups) %>% 
  mutate(cluster2=mydata$fit.cluster)

save(clustersConHum, file='data_clean/cluster_ConHum.Rdata')
write.csv(clustersConHum, file='data_clean/cluster_ConHum.csv')

########################################################################
########################################################################s

## Complete analyis for FactInf

# Prepare Data
summary(FactInf)
mydata2 <- type_convert(FactInf)
mydata2 <- na.omit(mydata2[,-c(2,10,19)]) # listwise deletion of missing
mydata2 <- scale(mydata2) # standardize variables

# Determine number of clusters
wss <- (nrow(mydata2)-1)*sum(apply(mydata2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata2,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata2, 4) # 5 cluster solution

# get cluster means
aggregate(mydata2,by=list(fit$cluster),FUN=mean)

# append cluster assignment
mydata2 <- data.frame(mydata2, fit$cluster)

# Ward Hierarchical Clustering
d <- dist(mydata2, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
groups <- cutree(fit, k=4) # cut tree into 4 clusters

# draw dendogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border="red")

clustersFactInf <- FactInf %>% 
  select(Formulario:nom_hum) %>% 
  mutate(groups=groups) %>% 
  mutate(cluster2=mydata2$fit.cluster)

save(clustersFactInf, file='data_clean/cluster_FactInf.Rdata')
write.csv(clustersFactInf, file='data_clean/cluster_FactInf.csv')

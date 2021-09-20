# https://www.statmethods.net/advstats/cluster.html

## Complete analyis for ConHum

# Prepare Data
load("data_clean/data_clean.Rdata")
source("code/packages.R")
set.seed(1818)
summary(ConHum)
mydata <- na.omit(ConHum[,-c(1,2,3)]) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
#fit <- kmeans(mydata, 5) # 7 cluster solution

# get cluster means
# aggregate(mydata,by=list(fit$cluster),FUN=mean)

# Ward Hierarchical Clustering
d <- dist(mydata, method = "euclidean") # distance matrix
fit2 <- hclust(d, method="ward.D2")
plot(fit2) # display dendogram
groups <- cutree(fit2, k=5) # cut tree into 4 clusters

# draw dendogram with red borders around the 4 clusters
#rect.hclust(fit2, k=7, border="red")

clustersConHum2 <- ConHum %>% 
  select(Formulario:nom_hum) %>% 
  mutate(groups5=groups) 

clustersConHum <- ConHum %>% 
  mutate(groups5=groups) %>% 
  group_by(groups5) %>% 
  summarise(sumBE=sum(BuenEstado), sumDR=sum(Drenado),sumGP=sum(Gan_Presen), 
            sumPI=sum(Plantas_In), sumSE=sum(Seco), sumArt=sum(Artificial),
            sumSE=sum(Sediment), sumRE=sum(Restaur), sumPR=sum(Proc_Resta),
            sumCO=sum(Contamin), sumCL=sum(Colmat),
            sumCU=sum(Cultivado), sumDA=sum(damage), n=n())

kable(clustersConHum)

save(clustersConHum, file='data_clean/cluster_ConHum.Rdata')
write.csv(clustersConHum2, file='data_clean/cluster_ConHum.csv')

########################################################################
########################################################################s

## Complete analyis for FactInf

# Prepare Data
summary(FactInf)
set.seed(1919)
mydata2 <- type_convert(FactInf)
mydata2 <- na.omit(mydata2[,-c(1,2,10,19,20,21)]) # listwise deletion of missing
mydata2 <- scale(mydata2) # standardize variables

# Determine number of clusters
wss <- (nrow(mydata2)-1)*sum(apply(mydata2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata2,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
# fit <- kmeans(mydata2, 7) # 7 cluster solution

# get cluster means
# aggregate(mydata2,by=list(fit$cluster),FUN=mean)

# Ward Hierarchical Clustering
d <- dist(mydata2, method = "euclidean") # distance matrix
fit2 <- hclust(d, method="ward.D2")
plot(fit2) # display dendogram
groups <- cutree(fit2, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 4 clusters
#rect.hclust(fit2, k=5, border="red")

clustersFactInf2 <- FactInf %>% 
  select(Formulario:nom_hum) %>% 
  mutate(groups5=groups) 

clustersFactInf <- type_convert(FactInf) %>% 
mutate(groups5=groups) %>% 
  group_by(groups5) %>% 
  summarise(sumBosq=sum(Bosques), sumCh=sum(Charr_Taco),sumSab=sum(Sabanas), 
            sumRef=sum(Reforest),sumGI=sum(Gan_Intens), sumGE=sum(Gan_Extens),
            sumAgr=sum(Agricult),sumAq=sum(Acuacult),
            sumPE=sum(Pesca), sumMol=sum(Moluscos),
            sumER=sum(Extr_Recur), sumInf=sum(Infraestr), 
            sumTC=sum(Turism_Com), sumTA=sum(Transp_Acu), 
            sumInd=sum(Industria), sumiV=sum(IV), 
            n=n())

kable(clustersFactInf)

save(clustersFactInf, file='data_clean/cluster_FactInf.Rdata')
write.csv(clustersFactInf2, file='data_clean/cluster_FactInf.csv')

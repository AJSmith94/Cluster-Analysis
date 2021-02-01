#Clustering Analysis-A Smith

#We choose MTCars as our dataset, and reduce the number of 
#variables for simplicity
cars<-mtcars[,1:7]
carsreduced<-mtcars[1:15,1:7]
head(cars)
cars

#A scatterplot for the 7 variables
plot(cars)
plot(cars[,3],cars[,1],xlab="Displacement (Cubic inches)",ylab="MPG (Miles Per Gallon)",
     main="Plot of Engine Size vs Mileage")
plot(cars[,3],cars[,4],xlab="Displacement (Cubic inches)",ylab="Horsepower (hp)",
     main="Plot of Engine Size Vs Horsepower")

#Ensure all the neccessary packages are loaded in R

library(tidyverse) 
library(cluster)
library(factoextra)
library(dendextend)

#Scale the data to ensure varaibles with larger variability do
#not dominate the data
ScaledCars<-scale(cars)
ScaledCars.reduced<-scale(carsreduced)

#calculate distances between observations
EuclideanDistance<-dist(ScaledCars,method="euclidean")
EuclideanDistance.subset<-dist(ScaledCars.reduced,method="euclidean")

#Prodice a 3x3 matrix of distances to demonstrate
round(as.matrix(EuclideanDistance)[1:5,1:5],1)

dev.off()

#Create a heatmap to visualise distances between observations
fviz_dist(EuclideanDistance,gradient = list(low = "black", mid = "grey", high ="white"))
 fviz_dist(EuclideanDistance.subset,gradient = list(low = "black", mid = "grey", high ="white"))

#Optimal clusters graph for full dataset and reduced dataset
 
fviz_nbclust(cars,kmeans,method="wss")+geom_vline(xintercept = 2, linetype = 2)

options(scipen=999)
fviz_nbclust(carsreduced,kmeans,method="wss")+geom_vline(xintercept = 3, linetype = 2)

#Clustering using k-means
kmeanscars<-kmeans(cars,2)
kmeanscars
kmeanscars.reduced<-kmeans(carsreduced,3)
kmeanscars.reduced
?kmeans
aggregate(mtcars, by=list(cluster=kmeanscars$cluster), mean)
aggregate(cars, by=list(cluster=kmeanscars$cluster), mean)

dd <- cbind(cars, cluster = kmeanscars$cluster)
dd

fviz_cluster(kmeanscars, data = cars,
             ellipse.type = "t", 
             star.plot = TRUE, 
             repel = TRUE,
             ggtheme = theme_minimal(),main="Cluster plot (K-Means)")

fviz_cluster(kmeanscars.reduced, data = carsreduced,
             ellipse.type = "euclid",
             star.plot = TRUE,
             repel = TRUE, 
             ggtheme = theme_minimal(),main="Cluster plot (K-Means)")

#Clustering using Partitioning Around Medoids (PAM)
fviz_nbclust(cars, pam, method = "wss")+
  theme_classic()

fviz_nbclust(carsreduced, pam, method = "silhouette")+
  theme_classic()

#Optimal number of clusters is suggested as 2
pam.alg <- pam(cars, 2)
pam.alg

pam.alg.reduced <- pam(carsreduced, 5)
pam.alg.reduced

fviz_cluster(pam.alg,
             ellipse.type = "t",
             repel = TRUE,
             ggtheme = theme_classic(),main="Cluster plot (PAM)")

fviz_cluster(pam.alg.reduced,
             ellipse.type = "t",
             repel = TRUE,
             ggtheme = theme_classic(),main="Cluster plot (PAM)")

#Clustering Large Applications (CLARA) approach

fviz_nbclust(cars, clara, method = "silhouette")+
  theme_classic()

fviz_nbclust(carsreduced, clara, method = "silhouette")+
  theme_classic()

#Again, CLARA recommends two clusters for MTCars dataset

clara.alg <- clara(cars, 2, pamLike = TRUE,samples = 5)
clara.alg

clara.alg.reduced<-clara(carsreduced, 5, pamLike = TRUE,samples = 5)
clara.alg.reduced

fviz_cluster(clara.alg,
             ellipse.type = "t", # Concentration ellipse
              pointsize = 1,
             ggtheme = theme_classic(),repel = TRUE,main="Cluster plot (CLARA)"
)

fviz_cluster(clara.alg.reduced,
             ellipse.type = "t", # Concentration ellipse
             pointsize = 1,
             ggtheme = theme_classic(),repel = TRUE,main="Cluster plot (CLARA)"
)





#Hierarchical clustering
distance<-dist(ScaledCars,method="euclidean")
as.matrix(distance)[1:4,1:4]
distancereduced<-dist(Scaledcarsreduced,method="euclidean")

distancereduced<-dist(ScaledCars.reduced,method="euclidean")
as.matrix(distancereduced)[1:5,1:5]

distancereducedhccomplete<-hclust(d=distancereduced,method="complete")
fviz_dend(distancereducedhccomplete,cex=0.55,main = "Cluster Dendrogram (Complete Method)")

distancereducedhcsingle<-hclust(d=distancereduced,method="single")
fviz_dend(distancereducedhcsingle,cex=0.50,main = "Cluster Dendrogram (Single Method)")

distancereducedhcaverage<-hclust(d=distancereduced,method="average")
fviz_dend(distancereducedhcaverage,cex=0.5, main = "Cluster Dendrogram (Average Method)")

distancereducedhccentroid<-hclust(d=distancereduced,method="centroid")
fviz_dend(distancereducedhccentroid,cex=0.40, main = "Cluster Dendrogram (Centroid Method)")

distancereducedhcward<-hclust(d=distancereduced,method="ward.D2")
fviz_dend(distancereducedhcward,cex=0.5, main = "Cluster Dendrogram (Ward Method)")


#Create a dendrogram
distancehccomplete<-hclust(d=distance,method="complete")
fviz_dend(distancehccomplete,cex=0.5)

distancehcsingle<-hclust(d=distance,method="single")
fviz_dend(distancehcsingle,cex=0.5)

grp<-cutree(res.hc,k=4)
head(grp,n=4)
table(grp)
rownames(cars)[grp==1]

fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.6, color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_cluster(list(data = ScaledCars, cluster = grp),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#Creating a tanglegram
library(dendextend)
hc1<-hclust(res.dist.reduced,method="average")
hc2<-hclust(res.dist.reduced,method="ward.D2")
dend1<-as.dendrogram(hc1)
dend2<-as.dendrogram(hc2)
dend_list<-dendlist(dend1,dend2)

tanglegram(dend1,dend2,highlight_distinct_edges = FALSE)
dev.off()


copheneticdistcomplete<-cophenetic(distancereducedhccomplete)
cor(copheneticdistcomplete,distancereduced)

copheneticdistsingle<-cophenetic(distancereducedhcsingle)
cor(copheneticdistsingle,distancereduced)

copheneticdistaverage<-cophenetic(distancereducedhcaverage)
cor(copheneticdistaverage,distancereduced)

copheneticdistcentroid<-cophenetic(distancereducedhccentroid)
cor(copheneticdistcentroid,distancereduced)

copheneticdistward<-cophenetic(distancereducedhcward)
cor(copheneticdistward,distancereduced)

cutsingle<-cutree(distancereducedhcsingle,k=3)
cutsingle
rownames(ScaledCars.reduced)[cutsingle==2]

fviz_dend(distancereducedhcsingle, k = 3,cex = 0.6, rect = TRUE,main="Cut Cluster Dendrogram (Single Linkage)",color_labels_by_k =TRUE,k_colors = c("red","darkgreen","blue"))

fviz_cluster(list(data = carsreduced, cluster = cutsingle), repel =TRUE,show.clust.cent = FALSE, ellipse.type = "t", ggtheme = theme_minimal(),main="Visual Representation of Dendrogram Cut",palette=c("blue","darkgreen","red"))

cutsingle5<-cutree(distancereducedhcsingle,k=5)
cutsingle5
fviz_dend(distancereducedhcsingle, k = 5,cex = 0.6, rect = TRUE,main="Cut Cluster Dendrogram (Single Linkage)",color_labels_by_k =TRUE,k_colors = c("red","darkgreen","blue","gold3","purple"))

fviz_cluster(list(data = carsreduced, cluster = cutsingle5), repel =TRUE,show.clust.cent = FALSE, ellipse.type = "t", ggtheme = theme_minimal(),main="Visual Representation of Dendrogram Cut",palette=c("purple","gold3","blue","darkgreen","red"))

carsreducedpca<-prcomp(ScaledCars.reduced)
fviz_pca_ind(carsreducedpca, addlabels = FALSE)

distance<-dist(ScaledCars,method="euclidean")
distancehccomplete<-hclust(d=distance,method="complete")
distancehcsingle<-hclust(d=distance,method="single")
distancehcaverage<-hclust(d=distance,method="average")
distancehccentroid<-hclust(d=distance,method="centroid")
distancehcward<-hclust(d=distance,method="ward.D2")

copheneticdistfullcomplete<-cophenetic(distancehccomplete)
cor(copheneticdistfullcomplete,distance)

copheneticdistfullsingle<-cophenetic(distancehcsingle)
cor(copheneticdistfullsingle,distance)

copheneticdistfullaverage<-cophenetic(distancehcaverage)
cor(copheneticdistfullaverage,distance)

copheneticdistfullcentroid<-cophenetic(distancehccentroid)
cor(copheneticdistfullcentroid,distance)

copheneticdistfullward<-cophenetic(distancehcward)
cor(copheneticdistfullward,distance)

fviz_dend(distancehcaverage,cex=0.5,main = "Cluster Dendrogram (Average Method)")
fviz_dend(distancehcaverage,k=2,cex=0.5,main = "Cluster Dendrogram (Average Method)",rect=TRUE,palette = c("red","blue"))

cutreefullcars<-cutree(distancehcaverage,k=2)
cutreefullcars

fviz_cluster(list(data = cars, cluster = cutreefullcars), repel =TRUE,show.clust.cent = FALSE, ellipse.type = "t", ggtheme = theme_minimal(),main="Visual Representation of Dendrogram Cut",palette = c("blue","red"))

dendreducedsingle<-as.dendrogram(distancereducedhcsingle)
dendreducedcomplete<-as.dendrogram(distancereducedhccomplete)
tanglegram(dendreducedsingle,dendreducedcomplete,lab.cex =0.8,edge.lwd=3,main_left="Single Linkage Dendrogram",main_right="Complete Linkage Dendrogram",common_subtrees_color_branches = TRUE)
dendreducedaverage<-as.dendrogram(distancereducedhcaverage)
tanglegram(dendreducedsingle,dendreducedaverage,lab.cex =0.8,edge.lwd=3,main_left="Single Linkage Dendrogram",main_right="Average Linkage Dendrogram",common_subtrees_color_branches = TRUE)
dendreducedcentroid<-as.dendrogram(distancereducedhccentroid)

dendreducedaverage<-as.dendrogram(distancereducedhcaverage)
dendreducedward<-as.dendrogram(distancereducedhcward)
tanglegram(dendreducedaverage,dendreducedward,lab.cex =0.8,edge.lwd=3,main_left="Average Linkage Dendrogram",main_right="Ward Linkage Dendrogram",common_subtrees_color_branches = TRUE)

entanglement(dendreducedsingle,dendreducedsingle)
entanglement(dendreducedsingle,dendreducedcomplete)
entanglement(dendreducedaverage,dendreducedward)

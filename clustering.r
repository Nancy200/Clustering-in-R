#ASSIGNMENT 4

#QUES A:
#read data 
data=read.csv("EastWestAirlinesCluster.csv")
#add row name as ID
row.names(data) <- data[,1]
#remove the ID column
data <- data[,-1]
#normalize the input variables
data.norm=sapply(data,scale)
#add row names to data.norm
row.names(data.norm) <- row.names(data)
#compute distance from normalized data
distance.norm <- dist(data.norm, method = "euclidean")
#use hclust with above distance and ward's method
Hmodel <- hclust(distance.norm, method = "ward.D")
plot(Hmodel, hang = -1, ann = FALSE)
# from the dendrogram, we can conclude there are 5 clusters
#cut the dendrogram in 5 clusters
Hmodel_reduced=cutree(Hmodel, k=5)
Hmodel_reduced

#QUES C
clust.centroid = function(i, dat, groups) {
  ind = (groups == i)
  colMeans(dat[ind,])
}
sapply(unique(Hmodel_reduced), clust.centroid,data, Hmodel_reduced)

#set labels as cluster membership and ID number
row.names(data.norm) <- paste(Hmodel_reduced, ": ", row.names(data), sep = "")
# plot heatmap
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(data.norm), Colv = NA, hclustfun = hclust,
        col=rev(paste("gray",1:99,sep="")))

#QUES D
#repeating the whole model with 95% of sample data
#divide data into 95 % and 5%
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.95,0.05))
newdata  <- data[sample, ]
#run the same steps with newdata
#normalize the input variables
newdata.norm=sapply(newdata,scale)
#add row names to data.norm
row.names(newdata.norm) <- row.names(newdata)
#compute distance from normalized data
newdistance.norm <- dist(newdata.norm, method = "euclidean")
#use hclust with above distance and ward's method
newHmodel <- hclust(newdistance.norm, method = "ward.D")
plot(newHmodel, hang = -1, ann = FALSE)
# from the dendrogram, we can conclude there are  clusters
#cut the dendrogram in 4 clusters
newHmodel_reduced=cutree(newHmodel, k=4)
newHmodel_reduced
#set labels as cluster membership and ID number
row.names(newdata.norm) <- paste(newHmodel_reduced, ": ", row.names(newdata), sep = "")
# plot heatmap
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(newdata.norm), Colv = NA, hclustfun = hclust,
        col=rev(paste("gray",1:99,sep="")))


#QUES E
#apply K means clustering
kModel <- kmeans(data.norm, 5)
kModel$cluster
kModel$centers
kModel$withinss
set.seed(123)
kModel$size

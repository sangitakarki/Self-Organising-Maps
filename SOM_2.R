
x <- read.table(file="file_name.txt", header=T, row.names=1, sep=",")
library(kohonen)
library(pheatmap)


som.model <- som(as.matrix(x), grid=somgrid(6,6, "hexagonal", toroidal = F))
mydata <- som.model$codes[[1]] #extract the matrix containing codebook vectors

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 

for (i in 2:24) { #i must be less than 5*5 the grid size defined at the begining
  wss[i] <- sum(kmeans(mydata, centers = i)$withinss)
}

plot(wss, type = "l")
abline(v=5)

som_cluster <- cutree(hclust(dist(mydata)), k = 6)
som_cluster <- cutree(hclust(as.dist(1-cor(t(mydata)))), k = 6)
pdf(file="SOM.pdf", height=6, width=6)
plot(som.model, bgcol = som_cluster, main = "Clusters") 
add.cluster.boundaries(som.model, som_cluster)
dev.off()

som.model$unit.classif
head(som.model)

final_cluster <- som_cluster[som.model$unit.classif]
final <- cbind(final_cluster, x)
write.table(final, file="clusters.txt", quote = F, sep="\t")

clust <- data.frame(as.character(final_cluster)); row.names(clust) <- row.names(x)
pdf(file="clusters.pdf", height=10, width=10)
pheatmap(x, annotation_row=clust, clustering_distance_rows="correlation")
dev.off()

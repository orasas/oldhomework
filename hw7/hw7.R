points <- c(1,4,9,16,25,36,49,64,81)

d=dist(points)
clust = hclust(d, method = 'centroid')
par(mfrow = c(1,1))
plot(clust, hang = -1)
rect.hclust(clust, k = 3, border = 2)

clust$height
clust$merge

par(mfrow = c(1, 1))
res =  numeric(8)
for (K in 1:8)
{
  res[K] = kmeans(d, centers = K, nstart = 1)$betweenss #W(K)
}
plot(res, type = 'b', pch = 20, ylab = 'B(K)', xlab = 'K')

require(cluster)
coef.hclust(clust) #69%

################  2
head(mtcars[,1:7])
D = dist(mtcars[,1:7])
car.hclust = hclust(D, method = 'average')
par(mfrow = c(1,1))
plot(car.hclust, hang = -1)


model.kmeans = kmeans(mtcars[,1:7], centers = 3, nstart = 100) #basic algorith
round(model.kmeans$betweenss / model.kmeans$totss * 100, 2) #percent of between clusters variavility in total vaiability
model.kmeans$cluster

nrow(mtcars)
par(mfrow = c(1, 1))
res =  numeric(20)
for (K in 1:20)
{
  res[K] = kmeans(mtcars[,1:7], centers = K, nstart = 1)$betweenss #W(K)
}
plot(res, type = 'b', pch = 20, ylab = 'B(K)', xlab = 'K')

res =  numeric(20)
N = nrow(mtcars)
for (K in 2:20)
{
  model.kmeans = kmeans(mtcars[,1:7], centers = K, nstart = 20) #W(K)
  res[K] = (model.kmeans$betweenss / (K - 1)) / (model.kmeans$tot.withinss / (N - K))
}
plot(res, type = 'b', pch = 20, ylab = 'CH(K)', xlab = 'K')
which.max(res) #best K
library(vegan) #for cascadeKM command
model.cascade = cascadeKM(mtcars[,1:7], 2, 10)
model.cascade$results #CH indexes
plot(model.cascade)

library(cluster) #for silhouette command
sil.index = silhouette(model.kmeans$cluster, dist = dist(mtcars[,1:7], method = 'euclidean'))
summary(sil.index) #mean s(i) for each cluster
plot(sil.index) #no narrow silhouettes - good clustering
sil.index[,3] #s(i)
mean(sil.index[,3]) #mean s(i)
res =  numeric(20)
dist.ED = dist(mtcars[,1:7], method = 'euclidean')
for (K in 2:20)
{
  model.kmeans = kmeans(mtcars[,1:7], centers = K, nstart = 20)
  res[K] = mean(silhouette(model.kmeans$cluster, dist = dist.ED)[,3])
}
plot(res, type = 'b', pch = 20, ylab = expression(bar(s)), xlab = 'K')
which.max(res) #best K (2)

######### 3

votes.repub
vote.c <- votes.repub[complete.cases(votes.repub),]
head(vote.c)

vote.dist <- dist(vote.c, method = "manhattan")
vote.clust <- hclust(vote.dist, method = "complete")

plot(vote.clust, hang = -1)


vote.pca <- prcomp(vote.c, scale = T)
cluster.vote.pca = hclust(dist(vote.pca$x), method = 'complete') #clustering on PCA's
par(mfrow = c(1, 2))
plot(vote.pca$x, pch = 20, col = cutree(vote.clust, k = 4), cex = 1.5, type = 'n') #groups from clustering on orginal data in PCA space
text(vote.pca$x, labels = rownames(vote.c), col = cutree(vote.clust, k = 4))
plot(vote.pca$x, pch = 20, col = cutree(cluster.vote.pca, k = 4), cex = 1.5, type = 'n') #groups from clustering on PC data in PCA space
text(vote.pca$x, labels = rownames(vote.c), col = cutree(cluster.vote.pca, k = 4))

?cascadeKM
vote.cascade = cascadeKM(vote.c, 2, 10)
vote.cascade$results
plot(vote.cascade)

res =  numeric(20)
N = nrow(vote.c)
for (K in 2:20)
{
  model.kmeans = kmeans(vote.c, centers = K, nstart = 20) #W(K)
  res[K] = (model.kmeans$betweenss / (K - 1)) / (model.kmeans$tot.withinss / (N - K))
}
plot(res, type = 'b', pch = 20, ylab = 'CH(K)', xlab = 'K')
which.max(res) # 2



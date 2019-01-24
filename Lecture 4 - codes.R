#function to calculate the number of possible clusterings for n observations and K clusters
n.clusteringss = function(n = 100, K = 4)
{
  result = 0
  for (k in 1:K)
  {
    result = result + (-1)^(K-k) * choose(K, k) * k^n
  }
  result / factorial(K)
}
n.clusteringss() #6.695575e+58
n.clusteringss(n = 100, K = 5) #6.573841e+67

#function to calculate the number of possible dendrograms for n leafs (observations)
n.dendorgrams = function(n = 100)
{
  if (n > 1) return(factorial(2 * n - 3) / (2^(n - 2) * factorial(n - 2)))
  else (return(cat('n should be greater than 1')))
}
n.dendorgrams(1)
n.dendorgrams(3) #1
n.dendorgrams(10) #34,459,425

#Compare computational complexity of hierarchical methods
curve(x^3, xlim = c(0, 12), col = 'red', lwd = 2, xlab = 'n', ylab = 'O(n)', main = 'Computational complexity')
curve(x^2 * log(x), xlim = c(0, 12), add = T, col = 'green', lwd = 2)
curve(2^x, xlim = c(0, 12), add = T, col = 'blue', lwd = 2)
uniroot(function(x) x^3 - 2^x, lower = 8, upper = 12) #x^3 - 2^x = 0; x = 9.94
points(9.94, 9.94^3, pch = 20, cex = 3)
uniroot(function(x) x^2 * log(x) - 2^x, lower = 5, upper = 7) #x^2 * log(x) - 2^x = 0; x = 6.03
points(6.03, 2^6.03, pch = 20, cex = 3)
legend('topleft', legend = c('Agglomerative (naive)', 'Agglomerative (best)', 'Divisive'), col = c('red', 'green', 'blue'), lwd = 2, cex = 1.2)

###############################
### Hierarchcial clustering ###
###############################

#not good for big data sets
#Example (single linkage)
c1 = c(0, 1.4, 9.7, 15.9, 15.1, 13.7)
c2 = c(1.4, 0, 9.3, 15.2, 14.4, 12.7)
c3 = c(9.7, 9.3, 0, 10.9, 10.0, 13.8)
c4 = c(15.9, 15.2, 10.9, 0, 2.2, 8.2)
c5 = c(15.1, 14.4, 10.0, 2.2, 0, 8.3)
c6 = c(13.7, 12.7, 13.8, 8.2, 8.3, 0)
D = cbind(c1, c2, c3, c4, c5, c6)
colnames(D) = 1:6
D = as.dist(D, diag = T)
ex.hclust = hclust(D, method = 'single')
par(mfrow = c(1,1))
plot(ex.hclust, hang = -1)

ex.hclust$merge #fusion steps (negative means sigletons)
#1 step = first and second singleton, 2 step is fourth/fifith, 3 step is sixth singleton and cluster from 2nd step...
ex.hclust$height #heights
#the height tells you at which distance clusters were joined together

#Artificial example
set.seed(0) #seed of random generator
X = rbind(scale(matrix(rnorm(2 * 20), ncol = 2), cent = c(1, 1), scale = F),
          scale(matrix(rnorm(2 * 30), ncol = 2), cent = -c(1, 1), scale = F)) #N(0,1), 50 2-dimensional points
X = rbind(X, matrix(runif(2 * 10, min(X), max(X)), ncol = 2)) #third group from U(min, max) distribution
d = dist(X) #ED distance
plot(X, pch = 20, col = rep(c(1, 2, 3), c(20, 30, 10)), xlim = c(-4, 4), ylim = c(-4, 4)) #we don't know the groups.
points(x = -1, y = -1, col = 'black', cex = 3, pch = 21, bg = 'black', lwd = 2) #center (1 group)
points(x = 1, y = 1, col = 'black', cex = 3, pch = 21, bg = 'red', lwd = 2) #center (2 group)
points(x = 0, y = 0, col = 'black', cex = 3, pch = 21, bg = 'green', lwd = 2) #center (3 group)
library(ellipse) #for ellipse function
lines(ellipse(matrix(c(1, 0, 0, 1), nrow = 2), centre = c(-1, -1)), type = 'l', col = 'black', lwd = 2)
lines(ellipse(matrix(c(1, 0, 0, 1), nrow = 2), centre = c(1, 1)), type = 'l', col = 'red', lwd = 2)
lines(ellipse(matrix(c(sqrt(64/12), 0, 0, sqrt(64/12)), nrow = 2), centre = c(0, 0)), type = 'l', col = 'green', lwd = 2)

tree.sing = hclust(ED, method = 'single') #hierarchical clustering
tree.comp = hclust(d, method = 'complete')
tree.avg = hclust(d, method = 'average')
tree.cen = hclust(d, method = 'centroid')

par(mfrow = c(2, 2))
plot(tree.sing, labels = F, hang = -1) #dendrogram
rect.hclust(tree.sing, k = 3, border = 2) #horizontal cut of the dendrogram
plot(tree.comp, labels = F, hang = -1)
rect.hclust(tree.comp, k = 3, border = 3)
plot(tree.avg, labels = F, hang = -1)
rect.hclust(tree.avg, k = 3, border = 4)
plot(tree.cen, labels = F, hang = -1)
rect.hclust(tree.cen, k = 3, border = 5)

labs.sing = cutree(tree.sing, k = 3) #a vector, containing assignment of samples into clusters
labs.comp = cutree(tree.comp, k = 3)
labs.avg = cutree(tree.avg, k = 3)
labs.cen = cutree(tree.cen, k = 3)

cols = c('red', 'green', 'blue') #plot of groups 
plot(X, col = cols[labs.sing], pch = 20, cex = 1.5, main = 'Single')
plot(X, col = cols[labs.comp], pch = 20, cex = 1.5, main = 'Complete')
plot(X, col = cols[labs.avg], pch = 20, cex = 1.5, main = 'Average')
plot(X, col = cols[labs.cen], pch = 20, cex = 1.5, main = 'Centroid')

#Compare clustering with 3 and 4 groups
cols = c('red', 'green', 'blue', 'black')
labs.avg2 = cutree(tree.avg, k = 4)
par(mfrow = c(1, 2))
plot(X, col = cols[labs.avg], main = '3 clusters', pch = 20, cex = 1.5)
plot(X, col = cols[labs.avg2], main = '4 clusters', pch = 20, cex = 1.5)
table(labs.avg, labs.avg2) #second group is splitted into two groups
par(mfrow = c(1, 1))
plot(tree.avg, labels = F, hang = -1e-10)
rect.hclust(tree.avg, k = 3, border = 2)
rect.hclust(tree.avg, k = 4, border = 4)

#Vltava example
vltava = read.delim ('http://www.davidzeleny.net/anadat-r/data-download/vltava-spe.txt', row.names = 1)
head(vltava)
library(cluster) #for agnes & diana commands
library(vegan) #for vegdist command (Bray-Curtis distance)
example.data = rbind(c(11, 0, 7, 8, 0), c(24, 37, 5, 18, 1))
vegdist(example.data, method = 'bray') #0.5675676
dis = vegdist(sqrt(vltava), method = 'bray')
tree.flexible = agnes(dis, method = 'flexible', par.method = 0.625)
lab.flexible = cutree(tree.flexible, 5)
plot(tree.flexible, which.plots = 2) #dendorgram
rect.hclust(tree.flexible, k = 5, border = 2, cluster = lab.flexible) #quite strong clustering structure (AC = 0.76)

coef.hclust(ex.hclust) #Agglomerative Coefficient for Example (Single linkage); 0.5883333

cluster.diana = diana(dis) #diana clustering method (divisive)
plot(cluster.diana, which.plots = 2)
rect.hclust(cluster.diana, k = 5, border = 2, cluster = lab.flexible)

#iris example - clustering & PCA
?pairs
pairs(iris[,1:4], pch = 20, col = rep(1:3, each = 50), cex = 1.5)
#we now pretend we don't know the species anymore, 
#and we will see how well the clustering methods recover the species
dist.iris = dist(iris[,1:4]) #ED
cluster.iris = hclust(dist.iris, method = 'complete')
plot(cluster.iris, labels = F)
cutree(cluster.iris, k = 3)
rect.hclust(cluster.iris, k = 3, border = 2)
#We can use the PC scores as the input to cluster as well to get a different look at the data
model.pca = prcomp(iris[,1:4], scale. = T)
cluster.iris.pca = hclust(dist(model.pca$x), method = 'complete') #clustering on PCA's
par(mfrow = c(1, 3))
plot(model.pca$x, pch = 20, col = rep(1:3, each = 50), cex = 1.5) #original groups in PC space
plot(model.pca$x, pch = 20, col = cutree(cluster.iris, k = 3), cex = 1.5) #groups from clustering on orginal data in PCA space
plot(model.pca$x, pch = 20, col = cutree(cluster.iris.pca, k = 3), cex = 1.5) #groups from clustering on PC data in PCA space
library(car) #for recode command
lab.cluster = recode(cutree(cluster.iris, k = 3), "2 = 3; 3 = 2") #exchange 2 & 3
1 - sum(diag(table(iris$Species, lab.cluster))) / nrow(iris) #error of clustering for original data (16%)
1 - sum(diag(table(iris$Species, col = cutree(cluster.iris.pca, k = 3)))) / nrow(iris) #error of clustering for PC data (21.33%)

###############################
### K-means clustering      ###
###############################
model.kmeans = kmeans(iris[,1:4], centers = 3, nstart = 100) #basic algorithm (100 random initializations; 3 groups)
model.kmeans$cluster #clustering vector
model.kmeans$centers #centers of clusters
model.kmeans$totss #total sum of squares
model.kmeans$withinss #W(C_k)
model.kmeans$tot.withinss #sum(W(C_k))
model.kmeans$betweenss #between clusters sum of squares
round(model.kmeans$betweenss / model.kmeans$totss * 100, 2) #percent of between clusters variavility in total vaiability
#higher means better
model.kmeans$size #number of objects in each class
lab.cluster.kmeans = recode(model.kmeans$cluster, "2 = 1; 3 = 2; 1 = 3") #exchange labels
1 - sum(diag(table(iris$Species, lab.cluster.kmeans))) / nrow(iris) #error of clustering for original data (10.67%); better than hierarchical method
plot(model.pca$x, pch = 20, col = lab.cluster.kmeans, cex = 1.5) #groups from clustering (K-means) for original data in PCA space
(pca.centers =  scale(model.kmeans$centers, model.pca$center, model.pca$scale) %*% model.pca$rotation) #centers in the PC space
points(pca.centers, cex = 3, pch = 21, col = 'black', bg = 'yellow')
cluster.means = by(iris[,1:4], lab.cluster, colMeans) #cluster means from hierarchical clustering
model.kmeans.nonrandom = kmeans(iris[,1:4], centers = matrix(unlist(cluster.means), nrow = 3, byrow = T)) #k-means with non-random initialization
#initial centers from hierarchical clustering
1 - sum(diag(table(iris$Species, model.kmeans.nonrandom$cluster))) / nrow(iris) #error of clustering for original data with non-random centers (10.67%)
#the same error as above
library(cluster) #for pam & clara commands
model.pam = pam(iris[,1:4], k = 3) #partitioning around medoids
model.pam$clustering #clustering vector
clusplot(model.pam, color = T, main = 'PAM clustering') #cluster plot (in PC space)
1 - sum(diag(table(iris$Species, model.pam$clustering))) / nrow(iris) #error of clustering for original data for PAM method (10.67%)
model.clara = clara(iris[,1:4], k = 3) #clara method
1 - sum(diag(table(iris$Species, model.clara$clustering))) / nrow(iris) #error of clustering for original data for PAM method (10%)

##################################
### Clustering - # of clusters ###
##################################
#W(K) minimizing
res =  numeric(50)
for (K in 1:50)
{
  res[K] = kmeans(iris[,1:4], centers = K, nstart = 20)$tot.withinss #W(K)
}
plot(res, type = 'b', pch = 20, ylab = 'W(K)', xlab = 'K')
par(mfrow = c(3, 3))
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 1, nstart = 20)$cluster, pch = 20)
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 2, nstart = 20)$cluster, pch = 20)
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 3, nstart = 20)$cluster, pch = 20)
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 5, nstart = 20)$cluster, pch = 20)
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 10, nstart = 20)$cluster, pch = 20)
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 15, nstart = 20)$cluster, pch = 20)
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 20, nstart = 20)$cluster, pch = 20)
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 30, nstart = 20)$cluster, pch = 20)
plot(model.pca$x, col = kmeans(iris[,1:4], centers = 50, nstart = 20)$cluster, pch = 20)
#Within-cluster variation measures how tightly grouped the clusters are. As we increase K, this will always decrease.
#B(K) maximizing
par(mfrow = c(1, 1))
res =  numeric(50)
for (K in 1:50)
{
  res[K] = kmeans(iris[,1:4], centers = K, nstart = 20)$betweenss #W(K)
}
plot(res, type = 'b', pch = 20, ylab = 'B(K)', xlab = 'K')
#CH index
res =  numeric(20)
N = nrow(iris)
for (K in 2:20)
{
  model.kmeans = kmeans(iris[,1:4], centers = K, nstart = 20) #W(K)
  res[K] = (model.kmeans$betweenss / (K - 1)) / (model.kmeans$tot.withinss / (N - K))
}
plot(res, type = 'b', pch = 20, ylab = 'CH(K)', xlab = 'K')
which.max(res) #best K
library(vegan) #for cascadeKM command
model.cascade = cascadeKM(iris[,1:4], 2, 10)
model.cascade$results #CH indexes
plot(model.cascade)

#Silhouette index
library(cluster) #for silhouette command
sil.index = silhouette(model.kmeans$cluster, dist = dist(iris[,1:4], method = 'euclidean'))
summary(sil.index) #mean s(i) for each cluster
plot(sil.index) #no narrow silhouettes - good clustering
sil.index[,3] #s(i)
mean(sil.index[,3]) #mean s(i)
res =  numeric(20)
dist.ED = dist(iris[,1:4], method = 'euclidean')
for (K in 2:20)
{
  model.kmeans = kmeans(iris[,1:4], centers = K, nstart = 20)
  res[K] = mean(silhouette(model.kmeans$cluster, dist = dist.ED)[,3])
}
plot(res, type = 'b', pch = 20, ylab = expression(bar(s)), xlab = 'K')
which.max(res) #best K (2)

##################################
### Clustering - GMM           ###
##################################
#iris example

library(mclust) #for Mclust command
model.gmm = Mclust(iris[,1:4]) #GMM + EM
summary(model.gmm) #Integrated Completed Likelihood (ICL) - a little more robust than BIC to violation of some GMM assumptions
plot(model.gmm, what = 'BIC') #we are looking for the model with highest BIC, VEV with 2 components
plot(model.gmm, what = 'density') #estimated densities
plot(model.gmm, what = 'classification')
model.gmm$classification #clustering vector
model.gmm$BIC #BICs for all models
model.gmm$df #number of estimated parameters for the best model
model.gmm$z #probabilities that observation belongs to the kth class

#faithful example
?faithful
model.gmm = Mclust(faithful)
summary(model.gmm) #EEE, 3
plot(model.gmm, what = 'BIC')
plot(model.gmm, what = 'density')
plot(model.gmm, what = 'classification')
model.gmm$BIC #EEE, 3 & EEE, 4 & VVE, 2 are very similar, but EEE, 3 is the simplest model


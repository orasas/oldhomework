?read.table
flake <- read.csv('StoneFlakes.txt',header=T, sep = ",", na.strings="NA" )
head(flake)

anno <- read.table('stoneanno.txt', header=TRUE, na.strings='?')
head(anno)


stone <- cbind(flake[,-1], anno)
head(stone)

par(mar = c(2,2,2,2))

#biplot
stonec <- stone[complete.cases(stone),]
head(stonec)
pr1 <- princomp(~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD,
                data = stonec,
                cor=TRUE,
                scores=TRUE)
par(mfrow = c(1,1))
?biplot
biplot(pr1,xlabs=stonec$ID, main = "Biplot of Data")

######

X0<- subset(stonec,,c(LBI,RTI,WDI,FLA,PSF,FSF,ZDF1,PROZD))
X <- scale(X0)

rownames(X) <- stonec$ID
d <- dist(X)

# adapted from http://stackoverflow.com/questions/18802519/label-and-color-leaf-dendrogram-in-r
cols <- colorRampPalette(c('purple','blue','seagreen', 'gold'))(4)

labelCol <- function(x) {
  if (is.leaf(x)) {
    label <- attr(x, "label") 
    attr(x, "nodePar") <- list(lab.col=cols[stonec$group[rownames(X)==label]], pch=46)
  }
  return(x)
}
## apply labelCol on all nodes of the dendrogram
par(cex=1)
hc <- hclust(d,method='average')
dd <- dendrapply(as.dendrogram(hc,hang=.1), labelCol)

par(mar=c(3,.1,.5,2))
plot(dd,horiz=F)
legend(x='topright',
       legend=c('Lower Paleolithic, Homo ergaster?, oldest',
                'Late Lower Paleolithic, Levallois tech',
                'Middle Paleolithic, probably Neanderthals',
                'Homo Sapiens, youngest'),
       text.col=cols,
       ncol=1,cex=1)

title(main = "Average")
##

hc <- hclust(d,method='complete')
dd <- dendrapply(as.dendrogram(hc,hang=.1), labelCol)

par(mar=c(3,.1,1,2))
plot(dd,horiz=F, main = "Complete")
legend(x='topright',
       legend=c('Lower Paleolithic, Homo ergaster?, oldest',
                'Late Lower Paleolithic, Levallois tech',
                'Middle Paleolithic, probably Neanderthals',
                'Homo Sapiens, youngest'),
       text.col=cols,
       ncol=1,cex=1)

##

hc <- hclust(d,method='ward.D')
dd <- dendrapply(as.dendrogram(hc,hang=.1), labelCol)

par(mar=c(3,.1,1,2))
plot(dd,horiz=F, main = "Ward linkage")
legend(x='topright',
       legend=c('Lower Paleolithic, Homo ergaster?, oldest',
                'Late Lower Paleolithic, Levallois tech',
                'Middle Paleolithic, probably Neanderthals',
                'Homo Sapiens, youngest'),
       text.col=cols,
       ncol=1,cex=1)

#####
#HC CUT

hc$merge
hc$height

cluster = hclust(d, method = 'ward.D')
cutree(cluster, k = 4)
table(cutree(cluster, k = 4), stonec$group)
rect.hclust(cluster, k = 4, border = 2)

########### PCA

head(stonec)
model.pca <- prcomp(stonec[,1:8], scale. =T)
summary(model.pca)
a<- (abs(model.pca$rotation[,1:2]))
(a)
#kaiser
par(mfrow = c(1,1), mar = c(2,2,2,2))
plot(model.pca, type = 'l')
#use 2 pcs

##########

plot(model.pca$x, type = 'n')
text(model.pca$x, labels = stonec$ID)

biplot(model.pca, xlabs = stonec$ID)
round(cor(stonec[,1:8], model.pca$x) ^ 2, 2) #amount of variation of each of the variables that

cluster.pca = hclust(dist(model.pca$x[,1:2]), method = 'complete') #clustering on PCA's

table(cutree(cluster.pca, k =4), stonec$group)
labs <- (cutree(cluster.pca, k =4))
labs <- recode(labs, "1=2; 2=1; 3=3; 4=4")

par(mfrow = c(1, 2))
plot(model.pca$x, pch = 20, col = stonec$group, cex = 1.5, main = "Original Data PC Space") #groups from clustering on original in PCA space
plot(model.pca$x, pch = 20, col = labs, cex = 1.5) #groups from clustering on PC data in PCA space
title(main = "PCA Space Cluster")

#################


library(car) #for recode command
?recode

lab.cluster = (cutree(cluster, k = 4))
table(lab.cluster, stonec$group)
lab.clust.recode = recode(lab.cluster, "2=1; 4 = 3; 3 = 3; 1=2") #exchange labels
lab.clust.recode
1 - sum(diag(table(stonec$group, lab.clust.recode))) / nrow(stonec) #error of clustering for original data 

lab.pca = cutree(cluster.pca, k=4)
lab.pca
table(lab.pca, stonec$group)
lab.pca.recode = recode(lab.pca, "1=2; 2 = 1; 3= 3; 4=3") #exchange labels (18%)
1 - sum(diag(table(stonec$group, lab.pca.recode))) / nrow(stonec) #error of clustering for PC data (24%)

# model.kmeans = kmeans(stonec[,1:8], centers = 4, nstart = 100) #basic algorithm (100 random initializations; 4 groups)
# round(model.kmeans$betweenss / model.kmeans$totss * 100, 2)
# model.kmeans$size
# 
# table(model.kmeans$cluster, stonec$group)
# 
# lab.cluster.kmeans = recode(model.kmeans$cluster, "1 = 3; 2 = 3; 3 = 1; 4=2") #exchange labels
# 1 - sum(diag(table(stonec$group, lab.cluster.kmeans))) / nrow(stonec) #(18%)
# 
# par(mfrow = c(1,1))
# 
# plot(model.pca$x, pch = 20, col = lab.cluster.kmeans, cex = 1.5) #groups from clustering (K-means) for original data in PCA space
# title(main = "K-means Clustering in PC Space")

# library(cluster) #for pam & clara commands
# model.pam = pam(stonec[,1:8], k = 4) #partitioning around medoids
# model.pam$clustering #clustering vector
# 
# table(model.pam$clustering, stonec$group)
# pam.recode = recode(model.pam$clustering, "1=2; 2=1; 3=3; 4=3")
# model.pam$clustering <- pam.recode
# clusplot(model.pam, color = T, main = 'PAM clustering') #cluster plot (in PC space)
# 1 - sum(diag(table(stonec$group, pam.recode))) / nrow(stonec) #error of clustering for original data for PAM method
# 
# model.clara = clara(stonec[,1:8], k = 4) #clara method
# clusplot(model.clara, color = T, main = "Clara")
# 1 - sum(diag(table(stonec$group, model.clara$clustering))) / nrow(stonec) #
# 
# 
# table(model.clara$clustering, stonec$group)
# clara.recode <- recode(model.clara$clustering, "1=2; 2=1; 3=3; 4=3")
# model.clara$clustering <- clara.recode
# clusplot(model.clara, color = T, main = "Clara Recode")
# 1 - sum(diag(table(stonec$group, model.clara$clustering))) / nrow(stonec) #


#############

stonec$Group <- factor(stonec$group,labels=c("Lower Paleolithic",
                                        "Levallois technique",
                                        "Middle Paleolithic",
                                        "Homo Sapiens"))

library(rpart)
library(tree)
stone.tree <- rpart(Group ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD,
                    data=stonec , minsplit=10)
par(mar = c(1,1,1,1), mfrow =c(1,1))
plot(stone.tree, margin = .1,main = "Classification Tree")
text(stone.tree, use.n = TRUE,cex=.8)

#### Prune
# stone.Gini.tree = tree(Group ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, data = stonec, split = 'gini') #Could be different than form rpart package
# plot(stone.Gini.tree)
# text(stone.Gini.tree)
# summary(stone.Gini.tree)
# 
# (stone.tree.prune = cv.tree(stone.Gini.tree, FUN = prune.misclass)) #prunning; k corresponds to alpha in weakest-link pruning
# plot(stone.tree.prune$size, stone.tree.prune$dev, type = 'b') #size (number of nodes)
# plot(stone.tree.prune$k, stone.tree.prune$dev, type = 'b') # alpha
# (best.size  = stone.tree.prune$size[which.min(stone.tree.prune$dev)]) #optimal size
# pruned.tree = prune.misclass(stone.Gini.tree, best = best.size) #optimum (prunned) tree
# plot(pruned.tree)
# text(pruned.tree)
# summary(pruned.tree)

###### RF bagging

library(randomForest) #for randomForest command

model.bagging = randomForest(Group ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, importance = T, data = stonec, ntree = 10000, mtry = 3) #bagging (all the covariates at each split)
model.bagging #confusion matrix and OOB error rate
importance(model.bagging, type = 1) #Permutation variable importance (higher value mean bigger imprtance)
importance(model.bagging, type = 2) #Mean decrease variable importance (higher value mean bigger imprtance)
varImpPlot(model.bagging, main = "StoneFlakes Bagging Importance") #plot of importances

model.rf = randomForest(Group ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, importance = T, data = stonec, ntree = 10000, mtry = 2) #random forest with 2 covarietes at each split 
model.rf
importance(model.rf, type = 1) #Permutation variable importance (higher value mean bigger imprtance)
importance(model.rf, type = 2) #Mean decrease variable importance (higher value mean bigger imprtance)
varImpPlot(model.rf, main = "StoneFlakes RandomForest Importance") #plot of importances

### LDA

require(MASS)
stone.lda <- lda(Group ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, data=stonec)
(stone.lda)
(classification = predict(stone.lda, data = stonec)) #classification on trainng data set
(contingency.table = table(classification$class, stonec$Group)) # (aka confusion matrix)

classification$class != stonec$Group #errors (8)
print(sum(classification$class != stonec$Group) / nrow(stonec)*100, 3) #error rate (resubstitution) in % 11.3
print((1 - sum(diag(contingency.table) / sum(contingency.table))) * 100, 3) #as above from contingency.table
round(classification$posterior, 3)

pred.lda = c()
length(stonec$Group)
for (i in 1:length(stonec$Group))#for each observation
{
pred.lda[i] = predict(stone.lda, stonec[i,])$class
}
length(pred.lda)
(loo.error.lda = (1 - sum(diag(table(stonec$Group, pred.lda)))/nrow(stonec)) * 100) #LDA LOO error rate; 11.26%

#try classification with pca maybe not tho

# pca.lda <- lda(stonec$Group~model.pca$x)
# 
# pca.class = predict(pca.lda, data = stonec) #classification on trainng data set
# pca.class
# (contingency.table = table(pca.class$class, stonec$Group))
# (contingency.table = table(classification$class, stonec$Group)) # (aka confusion matrix)


# #####
# #REGRESSION
# head(stonec)
# stone.lm <- lm(-age ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, data = stonec)
# summary(stone.lm)
# par(mfrow = c(2,2))
# plot(stone.lm)
# shapiro.test(stone.lm$resid)
# 
# 
# stonec.log = stonec
# stonec.log$age <- log(-stonec$age)
# stone.log <- lm(age~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, data =stonec.log)
# summary(stone.log)
# plot(stone.log)
# shapiro.test(stone.log$residuals)
  
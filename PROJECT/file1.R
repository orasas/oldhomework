StoneFlakes
nrow(StoneFlakes)
ncol(StoneFlakes)
summary(StoneFlakes)

plot(StoneFlakes)
stone = (StoneFlakes)
head(stone)
age <- c(120,200,200,300,300,200,200,200,200,80, 300, 200,120,120,200,NA,80,80,80,80,80,80,80,80,
         120,120,300,200,200,200,200,80,80,80,120,80, 200,200,300,300,200,200,120,200, 80,120,80,80,200,
         80,80,80,80,80,40,40,40, NA, 200,80,80,200,120,120,130,80,400,400,200,120,120,300,80,200,
         300,300,200,200,200)
length(age)

(group <- stoneanno$V2)


stone <- data.frame(age, (group), stone)
head(stone)
class(stone$LBI)
stone$LBI <- as.numeric(as.character(stone$LBI))
head(stone)

stone.c= stone[complete.cases(stone),]
str(stone.c)

sapply(stone.c, class)

model.pca <- prcomp(stone.c, scale. =T)
summary(model.pca)
abs(model.pca$rotation[,1:2])
par(mfrow = c(1,1))
plot(model.pca, type = 'l')

plot(model.pca$x, type = 'n')
text(model.pca$x, labels = (rownames(stone.c)))

biplot(model.pca)
round(cor(stone.c, model.pca$x) ^ 2, 2) #amount of variation of each of the variables that
#use 2 pcs

############

?princomp
pr1 <- princomp(~LBI+ RTI+ WDI+ FLA+ PSF+ ZDF1+ PROZD, data = stone.c, cor = T, scores=T)
biplot(pr1, xlabs = stone.c$X.group.)
legend('topright', border=NULL, legend = c('1 = Lower Paleo.', '2 = Middle Paleo.', '3 = Neanderthalensis', '4 = Sapiens'))
#use this one

############

X1<- subset(stone.c,,c(LBI,RTI,WDI,FLA,PSF,FSF,ZDF1 ,PROZD))
X <- scale(X1)
ED <- dist(X)
par(cex=.7)
hc <- hclust(ED,method='complete')

############

ex.hclust = hclust(ED, method = 'single')
par(mfrow = c(1,1))
plot(ex.hclust, hang = .1)
rect.hclust(ex.hclust, k = 4, border = 2) #horizontal cut of the dendrogram


# adapted from http://stackoverflow.com/questions/18802519/label-and-color-leaf-dendrogram-in-r
?colorRampPalette
cols <- colorRampPalette(c('blue','cyan', 'gold','seagreen'))(4) 

labelCol <- function(x) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label") 
    ## set label color to red for A and B, to blue otherwise
    attr(x, "nodePar") <- list(lab.col=cols[stone.c$X.group.[rownames(stone.c)==label]],
                               pch=46)
  }
  return(x)
}


## apply labelCol on all nodes of the dendrogram
dd <- dendrapply(as.dendrogram(hc,hang=.1), labelCol)
dd2 <- dendrapply(as.dendrogram(ex.hclust,hang=-1), labelCol)

par(mfrow = c(1,1))
plot(dd,horiz=F)
plot(dd2, horiz=F)

legend(x='topright',
       legend=c('Lower Paleolithic, Homo Ergaster, oldest',
                'Middle, Levallois technique',
                'Homo neanderthalensis',
                'Homo Sapiens, youngest'),
       text.col=cols,
       ncol=1,cex=.75)

rect.hclust(hc,k =4, border = 2) #horizontal cut of the dendrogram
rect.hclust(ex.hclust, k = 4, border = 2)

#################
#ClASSIFICATION

stone.c$X.group. <- factor(stone.c$X.group.,labels=c("Lower Paleolithic",
    "Levallois technique", "Middle Paleolithic", "Homo Sapiens"))

library(rpart)
library(tree)
?rpart
stone.tree <- rpart(X.group. ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD,
    data=stone.c , minsplit=10)
plot(stone.tree,margin=.1)
text(stone.tree, use.n = TRUE,cex=.8)

require(MASS)
stone.lda <- lda(X.group. ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, data=stone.c)
stone.lda
(classification = predict(stone.lda, data = stone.c)) #classification on trainng data set
(contingency.table = table(classification$class, stone.c$X.group.)) #on diagonal correctly classified cases (aka confusion matrix)

classification$class != stone.c$X.group. #errors (1)
print(sum(classification$class != stone.c$X.group.) / nrow(stone.c)*100, 3) #error rate (resubstitution) in %
print((1 - sum(diag(contingency.table) / sum(contingency.table))) * 100, 3) #as above from contingency.table
round(classification$posterior, 3)

############
#Regression
head(stoneanno)
dating <- stoneanno$V4


stone2 <- data.frame(stone, dating)
stone.c2 <- stone2[complete.cases(stone2),]
(stone.c2)
nrow(stone.c2)
sapply(stone.c2,class)

stone.c2$WDI
stone.lm <- lm(age ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, data = stone.c2[stone.c2$dating == 'geo',])
stone.lm <- lm(age ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, data = stone.c2)

summary(stone.lm)
### incorrect something not reading correctly?

logage <- log(stone.c2$age)
stone.c3 <- stone.c2
stone.c3$age <- logage

stone.log <- lm(age ~ LBI + RTI + WDI + FLA + PSF + FSF + ZDF1 + PROZD, data = stone.c3[stone.c3$dating == 'geo',])
summary(stone.log)

par(mfrow =c(2,2))
plot(stone.log)



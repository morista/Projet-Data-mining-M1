## ***** TP Data Mining

install.packages("ElemStatLearn")
library(ElemStatLearn)
datazip.train <- as.data.frame(zip.train)
datazip.test <- as.data.frame(zip.test)

## 1)

summary(datazip.train)

summary(datazip.test)

# Taille
 str(datazip.train)

str(datazip.test)

View(datazip.train)

datazip.train$V1 <- as.factor(datazip.train$V1)
datazip.test$V1 <- as.factor(datazip.test$V1)
## Données app
table(datazip.train$V1)
round(prop.table(table(datazip.train$V1)), digits = 4)
# Données Test
table(datazip.test$V1)
round(prop.table(table(datazip.test$V1)), digits = 4)
# 2)

library(graphics)
## Pour chaque classe on fait : 
image.Class <- function(data, k) {
  zz <- subset(data, data$V1 == k)
  
  z <- array(as.numeric(zz[34,-1]), dim = c(16,16))
  z <- z[,16:1]
  im <- image(1:16, 1:16, z)
  
}

# Données app
par(mfrow = c(2,2))
image.Class(datazip.train, 1)
image.Class(datazip.train, 2)
image.Class(datazip.train, 3)
image.Class(datazip.train, 4)

# Données de Test
image.Class(datazip.test, 1)
image.Class(datazip.test, 2)
image.Class(datazip.test, 3)
image.Class(datazip.test, 4)
zz <- subset(datazip.train, datazip.train$V1 == 2)

z <- array(as.numeric(zz[34,-1]), dim = c(16,16))
z <- z[,16:1]
 im <- image(1:16, 1:16, z, main="im")

## 3)

# Function Erreur et MC
error.rate.CM <- function(data.test,pred)
  {

    # Matrice de confusion
    mc <- table(data.test$V1,pred)
    print(mc)
    # Error Rate
    err.rate <- 1-sum(diag(mc))/sum(mc)
    return( err.rate)
  }

# a)
library(MASS)
lda.app1 <- lda(datazip.train$V1 ~., data = datazip.train)

lda_pred <- predict(lda.app1, datazip.test)
# MC et Error 
print(error.rate.CM(datazip.test, lda_pred$class))

# LDA avec Validation Croisé
lda.app2 <- lda(datazip.train$V1~., data=datazip.train, CV=TRUE)
print(error.rate.CM(datazip.train, lda.app2$class))

# LDA avec prior
# 
prior <- c(0.1638, 0.1378, 0.1003, 0.0902, 0.0894, 
           0.0763, 0.0911, 0.0885, 0.0743, 0.0883)
d  <- datazip.train[100:1000,]
lda.prior <- lda(d$V1~., data = d, CV=TRUE)
print(error.rate.CM(d, lda.prior$class))
lda.pred.prior <- predict(lda.prior, datazip.test)
print(error.rate.CM(datazip.test, lda.pred.prior$class))
# b)

 
library(class)
data.knn <- knn(datazip.train[,-1], datazip.test[,-1],
               cl = datazip.train$V1, k=3, prob = FALSE)
print(error.rate.CM(datazip.test, data.knn))

# En fonction de k
err<- rep(0,10)
for (i in 1:10){
  data.knn <- knn(datazip.train[,-1],datazip.test[,-1],
                  cl=datazip.train$V1, k=i, prob=TRUE)
  mc <-table(datazip.test$V1,data.knn)
  err[i] <- 1-sum(diag(mc))/sum(mc)
}
err
plot(err, xlab = "k",main ="Error.rate en fonction de k",
     type="b", col= c("blue","red"), pch=8, lwd = 2)


# c)

library(e1071)

## Noyeau linear

model1 <- svm(datazip.train$V1 ~., data = datazip.train, kernel = "linear")
## Prediction
pred.model1 <- predict(model1, newdata = datazip.test, type="class")
print(error.rate.CM(datazip.test, pred.model1))

## Noyeau radial

model2 <- svm(datazip.train$V1 ~., data = datazip.train, kernel = "radial")
## Prediction
pred.model2 <- predict(model2, newdata = datazip.test, type="class")
print(error.rate.CM(datazip.test, pred.model2))

## Noyeau polynomial 

model3 <- svm(datazip.train$V1 ~., data = datazip.train, kernel = "polynomial")
## Prediction
pred.model3 <- predict(model3, newdata = datazip.test, type="class")
print(error.rate.CM(datazip.test, pred.model3))

g.mod <- tune.svm(datazip.train$V1~., data = datazip.train, gamma = seq(.5, .9, by = .1))
# d) 
library(MASS)
model_logic <- polr(V1~., data = datazip.train)
model_logic
model.pred <- predict(model_logic, datazip.test, type = "class")
print(error.rate.CM(datazip.test, model.pred))
#e)
library(rpart)
install.packages("rattle")
library(rattle)
arb1 <- rpart(V1 ~ .,method="class", data=datazip.train)

# printcp(fit) # display the results 
# plotcp(fit) # visualize cross-validation results 
# summary(fit)

# # plot tree 
# plot(fit, uniform=TRUE, 
#      main="Classification Tree for Kyphosis")
# text(fit, use.n=TRUE, all=TRUE, cex=.8)

pred1<-predict(arb1,datazip.test, type="class")
print(error.rate.CM(datazip.test, pred))


#Arbre elagé
arb2 <- rpart(V1 ~ .,method="class", data=datazip.train, control=sctrl)
fancyRpartPlot(arb1)
pred2 <- predict(arb2, datazip.test, type= "class")
print(error.rate.CM(datazip.test, pred2))

# Pruned tree

arb3 <- prune(arb1, cp = arb1$cptable[which.min(arb1$cptable[,"xerror"]),"CP"])
pred3 <- predict(arb3, datazip.test, type = "class")
print(error.rate.CM(datazip.test, pred3))
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(arb3, uniform=TRUE, main="Pruned Classification Tree")
text(arb3, pretty = 0)
# plot(fit, uniform=TRUE, 
#      main="Classification Tree for Kyphosis")
# text(fit, use.n=TRUE, all=TRUE, cex=.8)


## 4)

# Temps d'exécuton des model :
et <- proc.time()
lda.app1 <- lda(datazip.train$V1 ~., data = datazip.train)
temps.LDA <- proc.time() - et
temps.LDA

et <- proc.time()
data.knn <- knn(datazip.train[,-1], datazip.test[,-1],
                cl = datazip.train$V1, k=3, prob = FALSE)
temps.k_near <- proc.time() - et
temps.k_near

et <- proc.time()
model1 <- svm(datazip.train$V1 ~., data = datazip.train, kernel = "linear")
temps.SVM_Lin <- proc.time() - et
temps.SVM_Lin

et <- proc.time()
model1 <- svm(datazip.train$V1 ~., data = datazip.train, kernel = "radial")
temps.SVM_rad <- proc.time() - et
temps.SVM_rad

et <- proc.time()
model1 <- svm(datazip.train$V1 ~., data = datazip.train, kernel = "polynomial")
temps.SVM_pol <- proc.time() - et
temps.SVM_pol

et <- proc.time()
model_logic <- polr(V1~., data = datazip.train)
temps.logic <- proc.time() - et
temps.logic

et <- proc.time()
arb1 <- rpart(V1 ~ .,method="class", data=datazip.train)
temps.dfault_raprt <- proc.time() - et
temps.dfault_raprt

et <- proc.time()
arb2 <- rpart(V1 ~ .,method="class", data=datazip.train, control=sctrl)
temps.elage <- proc.time() - et
temps.elage

et <- proc.time()
arb3 <- prune(arb1, cp = arb1$cptable[which.min(arb1$cptable[,"xerror"]),"CP"])
temps.prune <- proc.time() - et
temps.prune

library(MASS)
data<- read.csv("train.csv")
IRIS1 <- data[1:86,2:6]
head(IRIS1)

IRIS <- data.frame(length=IRIS1$Petal.Length,width=IRIS1$Petal.Width)
IRIS$class <- ifelse(IRIS1$Species == "setosa", +1, -1)
LDA <- lda(class ~ length + width, data = IRIS)
IRIS$pred <- predict(LDA)$class
confusionMatrix(table(IRIS$pred,IRIS$class))
accuracy()

IRIS$colour <- 2+ifelse(IRIS$pred == IRIS$class, as.numeric(IRIS$class), 0)
color <- c("red","green", "blue","red","blue")
plot(IRIS$length,IRIS$width,col=color[IRIS$colour],pch=2,cex=1)
rpt <- 3000
rpts <- function(v,rpt){runif(rpt,min=v[1],max=v[2])}
grid <- data.frame(apply(apply(IRIS[,1:2],2,range),2,rpts,pts)) 
grid$pred <- 3+as.numeric(predict(LDA,grid)$class)
points(grid$length,grid$width,col=color[grid$pred],cex=0.1) > sum(IRIS$class != IRIS$pred)



library(MASS)

data<- read.csv("train.csv")
IRIS1 <- data[1:86,2:6]
head(mydata)

IRIS1<-IRIS1[IRIS1$Species!="setosa",]

 IRIS <- data.frame(length=IRIS1$Petal.Length,width=IRIS1$Petal.Width)
 IRIS$class <- ifelse(IRIS1$Species == "virginica", +1, -1)
 LDA_2 <- lda(class ~ length + width, data = IRIS)
 
 IRIS$pred <- predict(LDA_2)$class
 
 confusionMatrix(table(IRIS$pred,IRIS$class))
 
 IRIS$colour <- 2+ifelse(IRIS$pred == IRIS$class, as.numeric(IRIS$class), 0)
 color <- c("red","green", "blue","red","blue")
 plot(IRIS$length,IRIS$width,col=color[IRIS$colour],pch=2,cex=1)
 rep <- 3000
 rpts <- function(v,pts){runif(pts,min=v[1],max=v[2])}
 grid <- data.frame(apply(apply(X[,1:2],2,range),2,rpts,rep)) 
 grid$pred <- 3+as.numeric(predict(LDA_2,grid)$class)
 points(grid$length,grid$width,col=color[grid$pred],cex=0.1) > sum(X$class != X$pred)
 
 

 
 
 
 
 test<- read.csv("test.csv")
 dim(test)
 head(test)
 
 
 IRIS1_test <- data.frame(length=test$Petal.Length,width=test$Petal.Width)
 IRIS1_test$class <- ifelse(test$Species == "setosa", +1, -1)
 IRIS1_test$pred <- predict(LDA_2,IRIS1_test)$class
 color <- c("red","green", "blue","red","blue")
 
 IRIS2_test$colour <- 2+ifelse(IRIS2_test$pred == IRIS2_test$class, as.numeric(IRIS2_test$class), 0)
 plot(IRIS2_test$length,IRIS2_test$width,col=color[IRIS2_test$colour],pch=2,cex=1)
 
 
 
 IRIS12_test<-test[test$Species!="setosa",]
 IRIS2_test <- data.frame(length=IRIS12_test$Petal.Length,width=IRIS12_test$Petal.Width)
 IRIS2_test$class <- ifelse(IRIS2_test$Species == "virginica", +1, -1)
 IRIS2_test$pred <- predict(LDA_2,IRIS2_test)$class
 color <- c("red","green", "blue","red","blue")
 
 IRIS2_test$colour <- 2+ifelse(IRIS2_test$pred == IRIS2_test$class, as.numeric(IRIS2_test$class), 0)
 plot(IRIS2_test$length,IRIS2_test$width,col=color[IRIS2_test$colour],pch=2,cex=1)
 
 
 
 
 
 
 data<- read.csv("train.csv")
 IRIS1 <- data[1:86,2:6]
 head(IRIS1)
 
 IRIS <- data.frame(length=IRIS1$Petal.Length,width=IRIS1$Petal.Width,class=IRIS1$Species)
 #IRIS$class <- ifelse(IRIS1$Species == "setosa", +1, -1)
 LDA <- lda(class ~ length + width, data = IRIS)
 
 
 IRIS$pred <- predict(LDA)$class
 
confusionMatrix(table(IRIS$pred,IRIS$class))
 

IRIS1_test <- data.frame(length=test$Petal.Length,width=test$Petal.Width,class=test$Species)
IRIS1_test$pred<-predict(LDA,IRIS1_test)$class
confusionMatrix(table(IRIS1_test$pred,IRIS1_test$class))

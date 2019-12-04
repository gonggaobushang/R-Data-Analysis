library(randomForest)

data(iris)
iris.rf <- randomForest(iris[,-5], iris[,5], prox=TRUE)
iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox) #找到最近的邻居
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
     main="Iris Data with Prototypes")
points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))


rf1 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf2 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf3 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf.all <- combine(rf1, rf2, rf3) #将树组合在一起
print(rf.all)


getTree(randomForest(iris[,-5], iris[,5], ntree=10), 3, labelVar=TRUE) #第三棵树
# status是否是终端 -1是 1不是



iris.rf <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
iris.rf <- grow(iris.rf, 50) #新增加树
print(iris.rf) #增加后误差矩阵等不显示


data(mtcars)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000,
                          keep.forest=FALSE, importance=TRUE)
importance(mtcars.rf) #第一个为OOB，第二个为基尼系数


iris.rf <- randomForest(Species ~ ., iris, keep.forest=FALSE)
plot(margin(iris.rf)) #正的表示正确的分类


iris.rf <- randomForest(Species ~ ., iris, proximity=TRUE,
                        keep.forest=FALSE)
MDSplot(iris.rf, iris$Species) #多维坐标
MDSplot(iris.rf, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))



iris.na <- iris
for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA
iris.roughfix <- na.roughfix(iris.na) #离散用众数填补，连续用中位数填补

iris.rf <- randomForest(iris[,-5], iris[,5], proximity=TRUE)
plot(outlier(iris.rf), type="h", #极端值用减去中位数除MAD（中位数绝对偏差）
     col=c("red", "green", "blue")[as.numeric(iris$Species)])



iris.rf <- randomForest(Species~., iris)
partialPlot(iris.rf, iris, Petal.Width, "versicolor") #变量对目标的边际影响

plot(randomForest(mpg ~ ., mtcars, keep.forest=FALSE, ntree=100), log="y")


ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))
iris.rf <- randomForest(Species ~ ., data=iris[ind == 1,])
iris.pred <- predict(iris.rf, iris[ind == 2,])
table(observed = iris[ind==2, "Species"], predicted = iris.pred)
predict(iris.rf, iris[ind == 2,], predict.all=TRUE)
predict(iris.rf, iris[ind == 2,], mtry=if (!is.null(y) && !is.factor(y)){
        max(floor(ncol(x)/3), 1)}else{ floor(sqrt(ncol(x)))},
        proximity=TRUE)



iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,localImp=TRUE,
                        mtry=2,ntree=212, #两个重要参数
                        proximity=TRUE)
print(iris.rf)
plot(iris.rf)
round(importance(iris.rf), 2)

myiris <- cbind(iris[1:4], matrix(runif(1 * nrow(iris)), nrow(iris), 1))
result <- rfcv(myiris, iris$Species, cv.fold=3) #交叉验证次数
with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))


iris.na <- iris
for (i in 1:4) iris.na[sample(150, sample(20)), i] <- NA
iris.imputed <- rfImpute(Species ~ ., iris.na) #缺失值的重要性


hist(treesize(iris.rf))


fgl.res <- tuneRF(fgl[,-10], fgl[,10], stepFactor=0.5) #寻找最优的mtry参数



mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000, keep.forest=FALSE,
                          importance=TRUE)

varImpPlot(mtcars.rf) #参数重要性


varUsed(randomForest(Species~., iris, ntree=100), by.tree=FALSE, count=TRUE) #变量在随机森林中被用的次数

#2019.12.4
#如果要分类，y需要转成factor形式，不然会被当成回归形式


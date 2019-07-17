library(Boruta)
#特征选择
#具有随机性
iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
boruta.train <- Boruta(Species~.,data=iris.extended,
                       maxRuns=100, #随机森林运行的最大次数
                       doTrace=2) #是否打印
print(boruta.train) #三种结果：确认，暂定，拒绝
plot(boruta.train, xlab = "", xaxt = "n")
#下面这段是为了补充plot的x轴
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)
#蓝色的盒状图对应一个阴影属性的最小、平均和最大分数
#红色、黄色和绿色的盒状图分别代表拒绝、暂定和确认属性的分数
final.boruta <- TentativeRoughFix(boruta.train) #通过比较归类为拒绝或者确认
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F) #确认的列表
boruta.df <- attStats(final.boruta)
print(boruta.df)

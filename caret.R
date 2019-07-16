library(caret)
data(mdrr)

#数据预处理
zerovar=nearZeroVar(mdrrDescr) #降维，删除方差较小的自变量
newdata1=mdrrDescr[,-zerovar]

descrCorr = cor(newdata1)
highCorr = findCorrelation(descrCorr, 0.90) #与其它自变量很强的相关性
newdata2 = newdata1[, -highCorr]
comboInfo = findLinearCombos(newdata2) #多重共线性
newdata2=newdata2[, -comboInfo$remove]

Process = preProcess(newdata2) #数据进行标准化
newdata3 = predict(Process, newdata2) #补足缺失值

inTrain = createDataPartition(mdrrClass, p = 3/4, list = FALSE) #数据划分
trainx = newdata3[inTrain,]
testx = newdata3[-inTrain,]
trainy = mdrrClass[inTrain]
testy = mdrrClass[-inTrain]

featurePlot(trainx[,1:2],trainy,plot='box') #箱线图

subsets = c(20,30,40,50,60,70,80)
ctrl= rfeControl(functions = rfFuncs, 
                 #rfFuncs随机森林 lmFuncs线性回归 nbFuncs朴素贝叶斯 treebagFuncs装袋决策树 caretFuncs自定
                 method = "cv", #交叉检验
                 verbose = FALSE, returnResamp = "final")
Profile = rfe(newdata3, mdrrClass, sizes = subsets, rfeControl = ctrl)
print(Profile)
plot(Profile)

Profile$optVariables

newdata4=newdata3[,Profile$optVariables]
inTrain = createDataPartition(mdrrClass, p = 3/4, list = FALSE)
trainx = newdata4[inTrain,]
testx = newdata4[-inTrain,]
trainy = mdrrClass[inTrain]
testy = mdrrClass[-inTrain]

fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 3,returnResamp = "all")
gbmGrid = expand.grid(.interaction.depth = c(1, 3),.n.trees = c(50, 100, 150, 200, 250, 300),.shrinkage = 0.1,.n.minobsinnode = 10)
gbmFit1 = train(trainx,trainy,method = "gbm",trControl = fitControl,tuneGrid = gbmGrid,verbose = FALSE)
plot(gbmFit1)

predict(gbmFit1, newdata = testx)[1:5]
gbmFit2= train(trainx, trainy,method = "treebag",trControl = fitControl)
models = list(gbmFit1, gbmFit2)
predValues = extractPrediction(models,testX = testx, testY = testy)
head(predValues)
testValues = subset(predValues, dataType == "Test")
probValues = extractProb(models,testX = testx, testY = testy)
testProbs = subset(probValues, dataType == "Test")
Pred1 = subset(testValues, model == "gbm")
Pred2 = subset(testValues, model == "treebag")
confusionMatrix(Pred1$pred, Pred1$obs)
confusionMatrix(Pred2$pred, Pred2$obs)

prob1 = subset(testProbs, model == "gbm")
prob2 = subset(testProbs, model == "treebag")
library(ROCR)
prob1$lable=ifelse(prob1$obs=="Active",yes=1,0)
pred1 = prediction(prob1$Active,prob1$lable)
perf1 = performance(pred1, measure="tpr", x.measure="fpr" )
plot(perf1 )

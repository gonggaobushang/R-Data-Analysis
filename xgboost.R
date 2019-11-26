#xgboost模型的说明
#可视化

# bst是一个xgboost模型
model <-xgb.dump(bst, with.stats = T)
model[1:10] #打印出前几个分类特征
importance_matrix <- xgb.importance(bst$feature_names,
                                    model = bst)
xgb.plot.importance(importance_matrix[1:10,]) #重要性
xgb.plot.tree(model = bst)
xgb.plot.tree(model = bst, trees = 0, show_node_id = TRUE) #只画第一颗树
library(DiagrammeR)
gr <- xgb.plot.tree(model=bst, trees=0:1, render=FALSE)
#需要安装DiagrammeRsvg、rsvg包
export_graph(gr, 'C:\\Users\\Administrator.000\\Desktop\\tree.pdf', width=1500, height=1900) 
export_graph(gr, 'C:\\Users\\Administrator.000\\Desktop\\tree.png', width=1500, height=1900)

#2019.11.26学习笔记
require(xgboost)
require(methods)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
bst <- xgboost(data = train$data,
               label = train$label,
               max_depth = 2, #树的最大深度
               eta = 1, #0-1的值，每棵树对当前估计的权重，越低nrounds越大，防止拟合越好但是速度慢
               nrounds = 2, #boosting 次数
               nthread = 2, #线程数
               objective = "binary:logistic")
bst <- xgboost(data = as.matrix(train$data), 
               label = train$label, 
               max_depth = 2, 
               eta = 1,
               nrounds = 2,
               nthread = 2, 
               objective = "binary:logistic", 
               verbose = 1) #打印训练误差
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bst <- xgboost(data = dtrain, 
               max_depth = 2, 
               eta = 1, 
               nrounds = 2, 
               nthread = 2, 
               objective = "binary:logistic", 
               verbose = 0)
#上面三种data的input形式不同，但不影响运算结果

#保存
xgb.DMatrix.save(dtrain, "dtrain.buffer")
dtrain2 <- xgb.DMatrix("dtrain.buffer")
xgb.save(bst, "xgboost.model")
bst2 <- xgb.load("xgboost.model")
raw = xgb.save.raw(bst)
bst3 <- xgb.load(raw)
dump_path = file.path(tempdir(), 'dump.raw.txt')
xgb.dump(bst, dump_path, with_stats = T)

dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain,
                 max_depth=2, 
                 eta=1, 
                 nrounds=2, 
                 watchlist=watchlist,
                 nthread = 2,
                 objective = "binary:logistic")
bst <- xgb.train(data=dtrain, 
                 max_depth=2, 
                 eta=1,
                 nrounds=2, 
                 watchlist=watchlist,
                 eval_metric = "error", #可以选择多个给定误差
                 eval_metric = "logloss", #自定误差只能一个
                 eval_metric = "rmse",
                 #eval_metric = "mlogloss ",
                 #eval_metric = "merror ",
                 eval_metric = "auc",
                 eval_metric = "aucpr",
                 eval_metric = "ndcg",
                 nthread = 2, 
                 objective = "binary:logistic")

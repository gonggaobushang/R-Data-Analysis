#EIX包用于探索XGBoost和lightGBM模型的结构
library("EIX")
library("Matrix")
library("xgboost")

# weight 某特征(节点)在树图中出现的次数
# total_cover 某特征对例子进行分类的个数(总和)
# cover total_cover/weight
# total_gain 在所有树中某特征每次分类节点时带来的总收益
# gain total_gain/weight

# sumGain  所有节点中的增益值之和
# sumCover 所有节点中Cover值的总和
# mean5Gain gain最高的给定变量出现的5次平均增益，
# meanGain 所有节点中的平均增益值
# meanCover 表示所有节点中的覆盖值
# freqency 给定变量的节点中出现的次数
# meanDepth 通过gain加权的平均深度
# numberOfRoots”  根目录中出现的次数
# weightedRoot”  根中的平均出现次数，按增益加权。

sm <- sparse.model.matrix(left ~ . - 1, data = HR_data)
param <- list(objective = "binary:logistic", max_depth = 2)
xgb_model <- xgboost(sm, params = param, label = HR_data[, left] == 1, nrounds = 25, verbose=0)
imp <- importance(xgb_model, sm, option = "both")
imp
plot(imp, top = 10)
imp <- importance(xgb_model, sm, option = "variables")
imp
plot(imp, top = nrow(imp))
imp <- importance(xgb_model, sm, option = "interactions")
imp
plot(imp, top = nrow(imp))
imp <- importance(xgb_model, sm, option = "variables")
imp
plot(imp, top = NULL, radar = FALSE, xmeasure = "sumCover", ymeasure = "sumGain")

inter <- interactions(xgb_model, sm, option = "interactions")
inter
plot(inter)
inter <- interactions(xgb_model, sm, option = "pairs") #子变量的gain总会比父节点高
#所以pairs的high gain可能是子节点带来的
inter
plot(inter)

lolli <- lollipop(xgb_model, sm)
plot(lolli, labels = "topAll", log_scale = TRUE)

data <- HR_data[9,-7]
new_observation <- sm[9,]
wf <- waterfall(xgb_model, new_observation, data, option = "interactions")
wf
plot(wf)

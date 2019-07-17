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
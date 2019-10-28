#EFA因子分析
# https://blog.csdn.net/woooooood/article/details/81610967
library(psych)
#主成分分析中，主成分可表示为原始变量的线性组合，原始变量也可表示为因子的线性组合
#而在因子分析中，原始变量是因子的线性组合，因子却不能表示为主成分的线性组合
fa(r, nfactors=, n.obs=, rotate=, scores=, fm=) 
#r:相关系数矩阵或原始数据矩阵， 
#nfactors:设定主提取的因子数（默认为1） 
#n.obs:观测数（输入相关系数矩阵时需要填写） 
#rotate:设定旋转的方法（默认互变异数最小法） 
#scores:设定是否需要计算因子得分（默认不需要） 
#fm:设定因子化方法（默认极小残差法）
#提取公因子的方法（fm），方法包括： ml：最大似然法 pa：主轴迭代法 
#wls：加权最小二乘法 gls：广义加权最小二乘法 minres：最小残差法 

# 因子分析
fa.parallel(iris[,c(1:4)], n.obs = 30, fa = "both", n.iter = 100)
fa_model1 <- fa(iris[,c(1:4)], nfactors = 1, rotate = "none", fm = "ml")

# 因子旋转：正交旋转法
fa_model2 <- fa(iris[,c(1:4)], nfactors = 2, rotate = "varimax", fm = "ml")
fa_model2$weights  #因子与原始变量之间线性回归方程的系数
factor.plot(fa_model2)
fa.diagram(fa_model2, simple = FALSE)






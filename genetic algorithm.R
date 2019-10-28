# 解决 x1/(x2+x3)+x2/(x1+x3)+x3/(x1+x2)=4

library(mcga)
library(GA)
library(foreach)
library(iterators)

f<-function(x){
  return (abs(x[1]/(x[2]+x[3])+x[2]/(x[1]+x[3])+x[3]/(x[1]+x[2])-4))
}
m <- mcga( popsize=2000, #个体数量，即染色体数目
                      chsize=3, #基因数量，限参数的数量
                      minval=0.0, #随机生成初始种群的下边界值
                       maxval=999999999, #随机生成初始种群的上边界值
                       maxiter=25000, #繁殖次数，即循环次数，默认为10
                      crossprob=1, #交配概率，默认为1.0
                      mutateprob=0.01, #突变概率，默认为0.01
                       evalFunc=f)#适应度函数，用于给个体进行评价
# elitism:精英数量，直接复制到下一代的染色体数目，默认为1

print(m$population[1,])
cat("Cost: ",m$costs[1],"\n")



library(genalg)

f<-function(x){
  return (abs(x[1]/(x[2]+x[3])+x[2]/(x[1]+x[3])+x[3]/(x[1]+x[2])-4))
}
m2 = rbga(stringMin=c(0,0,0), #每个基因的最小值
          stringMax=c(999999999,999999999,999999999), #每个基因的最大值
          suggestions=NULL, #染色体的可选列表
             popSize=2000, #个体数量，即染色体数目
             iters=25000,  #迭代次数
             evalFunc=f,  #适应度函数，用于给个体进行评价
             mutationChance=0.01, #突变机会，默认为1/(size+1)，
          #它影响收敛速度和搜索空间的探测，低机率导致更快收敛，高机率增加了搜索空间的跨度。
             verbose=F  #打印算法运行日志
             #monitorFunc=monitor  #监控函数，每产生一代后运行
          #elitism 精英数量，默认为20%，直接复制到下一代的染色体数目
                     )

print(m2$population[1,])
plot(m2)

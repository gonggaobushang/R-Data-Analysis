# https://mp.weixin.qq.com/s/ZyOj-9gTqbkK5yQKLaIeJw
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

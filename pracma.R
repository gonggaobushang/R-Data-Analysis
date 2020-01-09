library(pracma)
library(ggplot2)

#拟合曲线
set.seed(1)
x <- seq(0, pi, length.out = 25)
y <- sin(x) + 0.05 * runif(length(x), -2, 2)
p1 <- polyfit(x, y, 6) # 拟合6阶多项式,返回长度为7的向量
p2 <- polyfix(x, y, 6, xfix = c(1, 3), yfix = c(0.75, 0.05)) #与上面相比，保证拟合曲线一定经过这些点
p3 <- polyval(p1, x) # 用上面拟合出来的参数，根据x坐标计算y值

data1 <- data.frame(x = x, y = p3, y_fix = polyval(p2, x), y_point = y, stringsAsFactors = F)
g1 <- ggplot(data1) + geom_line(aes(x, y), color = "pink", size = 1) + 
  geom_line(aes(x,y_fix), color = "blue", size = 1) + geom_point(aes(x, y_point), color = "grey",size = 3)
g1


#拟合曲线
set.seed(21)
u <- seq(0, pi, length.out = 50)
x <- cos(u) + 0.005 * sample(-5:5, 50, replace = T)
y <- sin(u) + 0.005 * sample(-5:5, 50, replace = T)

myfit <- curvefit(u, x, y, n = 5) # u 表示拟合曲线要经过点的x坐标，即返回该点的y坐标
str(myfit) # 4个元素的列表，分别是拟合曲线的x,y坐标xp,yp; 和拟合多项式系数,px,py

xfit <- unlist(myfit$xp)
yfit <- unlist(myfit$yp)
#x[length(xfit)] <- NA
#y[length(xfit)] <- NA
df_fit <- data.frame(x = x, y = y, xfit = xfit, yfit = yfit)
ggplot(df_fit) + geom_point(aes(x = x, y = y), shape = 21, size = 3, fill = "cyan") + 
  geom_line(aes(x = xfit, y = yfit), color = "pink", size = 1)


#三次样条差值
x <- seq(-55, 65, by = 10)
y <- c(-3.25, -3.37, -3.35, -3.20, -3.12, -3.02, -3.02, -3.07, -3.17, -3.32, -3.30, -3.22, -3.10)
xs <- seq(-60, 70, by = 1) 

ys1 <- cubicspline(x, y, xi=xs, endp2nd = TRUE) # xi 表示样条曲线要经过的点的坐标，函数即返回在该点y坐标
ys2 <- ys <- cubicspline(x, y, xs) # 不指定边界条件

x[length(xs)] <- NA; y[length(xs)] <- NA

df_splines <- data.frame(x = x, y = y, xs = xs, ys1 = ys1, ys2 = ys2)
ggplot(df_splines) + 
  geom_point(aes(x, y), shape = 21, size = 3, fill = "#fdc086") + 
  geom_line(aes(x=xs, y=ys1), color = "#7fc97f", size = 0.8) + # 默认边界条件，端点处导数为0
  geom_line(aes(x=xs, y=ys2), color = "#fdc086", size = 1) # 无边界条件

#天气数据
library(worldmet)
#找到离这个坐标点最近的机场位置
#并呈现leaflet交互式地图
#缺点是速度非常慢
getMeta(lon = 120.173569,lat=30.251173)
#提取数据
importNOAA(code="584570-99999",year = seq(1956,2019))
#小时级别数据

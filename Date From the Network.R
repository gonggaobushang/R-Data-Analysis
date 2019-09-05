#数据的网络获取

#直接从网上读取
url=paste0("https://research.stlouisfed.org/fred2/series/","DEXCHUS","/downloaddata/","DEXCHUS",".csv")
read.csv(url)

#Wind数据接口下载
library(WindR)
w.start(showmenu=FALSE)

#低频时间序列
w.wsd("603096.SH","open,high,close,low","2019-9-1",Sys.Date()-1,"Period=D") #D日W周M月Q季Y年

#历史截面数据
w.wss("603096.SH,600000.SH","net_profit_is","unit=1","rptDate=20171231","rptType=1") #年底的净利润数据

#分钟数据
w.wsi("600000.SH","open,high,close,low","2018-9-3 9:00:00","2018-9-3 10:00:00","BarSize=3")

#日内tick数据
w.wst("IF00.CFE","last,volume,amt,bid1,bsize1,ask1,asize1",Sys.Date()-1,Sys.Date()) #沪深300股指期货

#板块指数等成分数据
w.wset("indexconstituent","date=2018-01-05;windcode=000300.SH") #沪深300指数成分股权重

#宏观经济数据
w.edb("M5567876","1978-01-01")

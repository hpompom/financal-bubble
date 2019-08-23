library(readxl)
library(ggplot2)
library(exuber)
library(MultipleBubbles)
library(forecast)
library(fBasics)
library(VineCopula)
library(rugarch)
library(FinTS)
library(urca)
library(TSA)
library(xts)
library(pastecs)
library(tseries)
library(FinTS)
library(car)
library(lmtest)
library(PerformanceAnalytics)#加载包
par(family='STKaiti') # 改字体, 否则不显示中文
chart.Correlation(re, histogram=TRUE)
chart.Correlation(adf, histogram=TRUE)

####
set.seed(1000)##设定种子
####
logre<-function(a){
  re<-c()##对数收益率
  for(i in 1:(length(a)-1)){
    re<-c(re,log(a[i+1]/a[i]))
  }
  return(re)
}
####

tj<-function(a){
  mean1<-mean(a)
  sd1<-sd(a)
  kur1<- kurtosis(a)
  ske1<- skewness(a)
  sha1 <- shapiro.test(a)$p.value
  gg <- data.frame(mean1,sd1,kur1,ske1,sha1)
  return(gg)
}

#####
#####
jmall <- read_excel("~/Desktop/桌面/建模/数据/亚太指数.xlsx", 
                                 sheet = "剔除后", col_types = c("date", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric"))#载入数据
jmall<-as.data.frame(jmall)
jmallnotime<-jmall[,-1]#去除日期列
###################################
rbre<-logre(jmall[,2]);hgre<-logre(jmall[,3]);xgre<-logre(jmall[,4]);ydre<-logre(jmall[,5]);shre<-logre(jmall[,6]);szre<-logre(jmall[,7])
mlre<-logre(jmall[,8]);flbre<-logre(jmall[,9]);tgre <- logre(jmall[,10]);ynnre<-logre(jmall[,11]);xjpre <- logre(jmall[,12]);ynre <- logre(jmall[,13])
###################################
jmadf<-radf(jmallnotime,lag=1)#bsadf
jmcv<-mc_cv(length(jmallnotime[,1]),minw = 64)#关键值
autoplot(jmadf,cv=jmcv,select = TRUE)#画图
saveadf<-data.frame(jmadf$bsadf,jmcv$bsadf_cv[(1:872),2],jmall[(66:length(jmall[,1])),1])
write.table(saveadf,file = "~/desktop/saveadf.csv",sep=",")
##描述性统计
re<-data.frame(rbre,hgre,xgre,ydre,szre,shre,mlre,flbre,tgre,ynnre,xjpre,ynre)
boxre<-data.frame(收益率=c(rbre,hgre,xgre,ydre,shre,szre,mlre,flbre,tgre,ynnre,xjpre,ynre),class=c(rep("日经225",times=length(rbre))
                  ,rep("韩国综指",times=length(rbre)),rep("恒生指数",times=length(rbre)),rep("孟买30",times=length(rbre)),rep("深圳成指",times=length(rbre))
                  ,rep("上证指数",times=length(rbre)),rep("吉隆坡指数",times=length(rbre)),rep("马尼拉指数",times=length(rbre)),rep("泰国指数",times=length(rbre))
                  ,rep("雅加达指数",times=length(rbre)),rep("新加坡STI",times=length(rbre)),rep("胡志明指数",times=length(rbre))))
adf<-data.frame(rbadf,hgadf,xgadf,ydadf,shadf,szadf,mladf,flbadf,tgadf,ynnadf,xjpadf,ynadf)
colnames(adf)<-c("日经225bsadf","韩国综指bsadf","恒生指数bsadf","孟买30bsadf","深证成指bsadf"
                ,"上证指数bsadf","吉隆坡指数bsadf","马尼拉指数bsadf","泰国综指bsadf"
                ,"雅加达指数bsadf","新加坡STIbsadf","胡志明指数bsadf")
#箱线图
names(boxre)<-c("收益率","地区")
boxre$地区<-factor(boxre$地区)
ggplot(data=boxre,aes(x=地区,y=收益率))+
 theme(text = element_text(family = 'STKaiti'))+
 geom_boxplot()

###提取数据
rbadf<-jmadf$bsadf[,1]
hgadf<-jmadf$bsadf[,2]
xgadf<-jmadf$bsadf[,3]
ydadf<-jmadf$bsadf[,4]
shadf<-jmadf$bsadf[,5]
szadf<-jmadf$bsadf[,6]
mladf<-jmadf$bsadf[,7]
flbadf<-jmadf$bsadf[,8]
tgadf<-jmadf$bsadf[,9]
ynnadf<-jmadf$bsadf[,10]
xjpadf<-jmadf$bsadf[,11]
ynadf<-jmadf$bsadf[,12]

boxadf<-data.frame(收益率=c(rbadf,hgadf,xgadf,ydadf,shadf,szadf,mladf,flbadf,tgadf,ynnadf,xjpadf,ynadf),class=c(rep("日经225",times=length(rbadf))
                                                                                                ,rep("韩国综指",times=length(rbadf)),rep("恒生指数",times=length(rbadf)),rep("孟买30",times=length(rbadf)),rep("深圳成指",times=length(rbadf))
                                                                                                ,rep("上证指数",times=length(rbadf)),rep("吉隆坡指数",times=length(rbadf)),rep("马尼拉指数",times=length(rbadf)),rep("泰国指数",times=length(rbadf))
                                                                                                ,rep("雅加达指数",times=length(rbadf)),rep("新加坡STI",times=length(rbadf)),rep("胡志明指数",times=length(rbadf))))

adf<-data.frame(rbadf,hgadf,xgadf,ydadf,shadf,szadf,mladf,flbadf,tgadf,ynnadf,xjpadf,ynadf)
colnames(re)<-c("日经225","韩国综指","恒生指数","孟买30","深圳成指","上证指数","吉隆坡指数","马尼拉指数","泰国指数","雅加达指数","新加坡STI","胡志明指数"
)
colnames(adf)<-c("日经225bsadf","韩国综指bsadf","恒生指数bsadf","孟买30bsadf","上证指数bsadf"
                 ,"深圳成指bsadf","吉隆坡指数bsadf","马尼拉指数bsadf","泰国综指bsadf"
                 ,"雅加达指数bsadf","新加坡STIbsadf","胡志明指数bsadf")
#箱线图
names(boxadf)<-c("指数BSADF","地区")
boxadf$地区<-factor(boxadf$地区)
ggplot(data=boxadf,aes(x=地区,y=指数BSADF))+
  theme(text = element_text(family = 'STKaiti'))+
  geom_boxplot()

##
names(boxadf)<-c("指数BSADF","地区")
boxadf$地区<-factor(boxadf$地区)
ggplot(data=boxadf,aes(x=地区,y=指数BSADF))+
  theme(text = element_text(family = 'STKaiti'))+
  geom_boxplot()

#ARCH效应检验
#BSADF
ArchTest(rbadf)
ArchTest(hgadf)
ArchTest(xgadf)
ArchTest(ydadf)
ArchTest(shadf)
ArchTest(szadf)
###
#收益率平稳性检验
summary(ur.df(rbre,type = c("trend")))
summary(ur.df(hgre,type = c("trend")))
summary(ur.df(ydre,type = c("trend")))
summary(ur.df(xgre,type = c("trend")))
summary(ur.df(shre,type = c("trend")))
summary(ur.df(szre,type = c("trend")))
summary(ur.df(mlre,type = c("trend")))
summary(ur.df(flbre,type = c("trend")))
summary(ur.df(tgre,type = c("trend")))
summary(ur.df(ynnre,type = c("trend")))
summary(ur.df(ynre,type = c("trend")))
summary(ur.df(xjpre,type = c("trend")))

###收益率acf,pacf
acf(rbre)
pacf(rbre)
acf(hgre)
pacf(hgre)
acf(xgre)
pacf(xgre)
acf(ydre)
pacf(ydre)
acf(shre)
pacf(shre)
acf(szre)
pacf(szre)
acf(shre)
pacf(shre)
acf(mlre)
pacf(mlre)
acf(mlre)
pacf(mlre)
acf(flbre)
pacf(flbre)
acf(tgre)
pacf(tgre)
acf(ynnre)
pacf(ynnre)
acf(ynre)
pacf(ynre)
acf(xjpre)
pacf(xjpre)

###拟合均值-方差
########################均值-方差-分布
gjr_sstd0_1.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,1)),
                              distribution.model = "sstd" ) 
gjr_sstd0_3.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,3)),
                              distribution.model = "sstd" ) 
gjr_sstd2_2.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(2,2)),
                              distribution.model = "sstd" ) 
gjr_sstd3_1.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(3,1)),
                              distribution.model = "sstd" )
gjr_sstd1_0.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,0)),
                              distribution.model = "sstd" )
gjr_sstd0_2.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,2)),
                              distribution.model = "sstd" )
gjr_sstd1_2.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,2)),
                              distribution.model = "sstd" )
gjr_sstd1_1.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1)),
                              distribution.model = "sstd" )
gjr_sstd6_0.spec = ugarchspec(variance.model = list(model="gjrGARCH",  
                                                    garchOrder=c(1,1)), mean.model = list(armaOrder=c(6,0)),
                              distribution.model = "sstd" )
##均值-波动建模
##################################################################
arma_gjr_rbre<-ugarchfit(spec=gjr_sstd3_1.spec,data = rbre)#日本
arma_gjr_hgre<-ugarchfit(spec=gjr_sstd0_1.spec,data = hgre)#韩国
arma_gjr_xgre<-ugarchfit(spec=gjr_sstd0_1.spec,data = xgre)#香港
arma_gjr_ydre<-ugarchfit(spec=gjr_sstd0_3.spec,data = ydre)#印度
arma_gjr_shre<-ugarchfit(spec=gjr_sstd0_1.spec,data = shre)#上海
arma_gjr_szre<-ugarchfit(spec=gjr_sstd2_2.spec,data = szre)#深圳
arma_gjr_mlre<-ugarchfit(spec=gjr_sstd1_0.spec,data = mlre)#马来西亚
arma_gjr_flbre<-ugarchfit(spec=gjr_sstd0_2.spec,data = flbre)#菲律宾
arma_gjr_tgre<-ugarchfit(spec=gjr_sstd2_2.spec,data = tgre)#泰国
arma_gjr_ynnre<-ugarchfit(spec=gjr_sstd1_2.spec,data = ynnre)#印度尼西亚
arma_gjr_ynre<-ugarchfit(spec=gjr_sstd2_2.spec,data = ynre)#越南
arma_gjr_xjpre<-ugarchfit(spec=gjr_sstd6_0.spec,data = xjpre)#新加坡
##################################################################
residualall<-data.frame(residual_rbre,residual_hgre,
                     residual_xgre,residual_ydre,
                     residual_shre,residual_szre,
                     residual_mlre,residual_flbre,
                     residual_tgre,residual_ynnre,
                     residual_ynre,residual_xjpre)
#########################arch效应检验########################################
apply(residualall, 2, ArchTest)
###################################################################

#标准化残差
residual_rbre<-residuals(arma_gjr_rbre,standardize=TRUE)
residual_hgre<-residuals(arma_gjr_hgre,standardize=TRUE)
residual_xgre<-residuals(arma_gjr_xgre,standardize=TRUE)
residual_ydre<-residuals(arma_gjr_ydre,standardize=TRUE)
residual_shre<-residuals(arma_gjr_shre,standardize=TRUE)
residual_szre<-residuals(arma_gjr_szre,standardize=TRUE)
residual_mlre<-residuals(arma_gjr_mlre,standardize=TRUE)
residual_flbre<-residuals(arma_gjr_flbre,standardize=TRUE)
residual_tgre<-residuals(arma_gjr_tgre,standardize=TRUE)
residual_ynnre<-residuals(arma_gjr_ynnre,standardize=TRUE)
residual_ynre<-residuals(arma_gjr_ynre,standardize=TRUE)
residual_xjpre<-residuals(arma_gjr_xjpre,standardize=TRUE)
##数据处理
residual_rbre<-data.frame(residual_rbre)[,1]
residual_hgre<-data.frame(residual_hgre)[,1]
residual_xgre<-data.frame(residual_xgre)[,1]
residual_ydre<-data.frame(residual_ydre)[,1]
residual_shre<-data.frame(residual_shre)[,1]
residual_szre<-data.frame(residual_szre)[,1]
residual_mlre<-data.frame(residual_mlre)[,1]
residual_flbre<-data.frame(residual_flbre)[,1]
residual_ynnre<-data.frame(residual_ynnre)[,1]
residual_ynre<-data.frame(residual_ynre)[,1]
residual_xjpre<-data.frame(residual_xjpre)[,1]
residual_tgre<-data.frame(residual_tgre)[,1]
#####残差序列与0-1分布的检测
ks.test(residual_rbre,runif(1000))
ks.test(residual_hgre,runif(1000))
ks.test(residual_xgre,runif(1000))
ks.test(residual_ydre,runif(1000))
ks.test(residual_shre,runif(1000))
ks.test(residual_szre,runif(1000))
ks.test(residual_mlre,runif(1000))
ks.test(residual_flbre,runif(1000))
ks.test(residual_ynnre,runif(1000))
ks.test(residual_ynre,runif(1000))
ks.test(residual_xjpre,runif(1000))
######概率积分变换后的残差序列；0-1分布的检测
####概率积分变化
residual_rbre_01<-pobs(residual_rbre)#1日本
residual_hgre_01<-pobs(residual_hgre)#2韩国
residual_xgre_01<-pobs(residual_xgre)#3香港
residual_ydre_01<-pobs(residual_ydre)#4印度
residual_shre_01<-pobs(residual_shre)#5上海
residual_szre_01<-pobs(residual_szre)#6深圳
residual_mlre_01<-pobs(residual_mlre)#7马来西亚
residual_flbre_01<-pobs(residual_flbre)#8菲律宾
residual_tgre_01<-pobs(residual_tgre)#9泰国
residual_ynnre_01<-pobs(residual_ynnre)#10印尼
residual_ynre_01<-pobs(residual_ynre)#11越南
residual_xjpre_01<-pobs(residual_xjpre)#12新加坡
########检验0-1分布

ks.test(residual_rbre_01,runif(1000))
ks.test(residual_hgre_01,runif(1000))
ks.test(residual_xgre_01,runif(1000))
ks.test(residual_ydre_01,runif(1000))
ks.test(residual_shre_01,runif(1000))
ks.test(residual_szre_01,runif(1000))
ks.test(residual_mlre_01,runif(1000))
ks.test(residual_flbre_01,runif(1000))
ks.test(residual_tgre_01,runif(1000))
ks.test(residual_ynnre_01,runif(1000))
ks.test(residual_ynre_01,runif(1000))
ks.test(residual_xjpre_01,runif(1000))
######收益率copula拟合
recopula<-data.frame(residual_rbre_01,residual_hgre_01,
                     residual_xgre_01,residual_ydre_01,
                     residual_shre_01,residual_szre_01,
                     residual_mlre_01,residual_flbre_01,
                     residual_tgre_01,residual_ynnre_01,
                     residual_ynre_01,residual_xjpre_01)
RVM_re<-RVineStructureSelect(recopula,familyset = NA,type=0,progress = TRUE)
plot(RVM_re)


######bsadf copula拟合
######
acf(rbadf)
pacf(rbadf)
acf(hgadf)
pacf(hgadf)
acf(xgadf)
pacf(xgadf)
acf(ydadf)
pacf(ydadf)
acf(shadf)
pacf(shadf)
acf(szadf)
pacf(szadf)
acf(mladf)
pacf(mladf)
acf(flbadf)
pacf(flbadf)
acf(tgadf)
pacf(tgadf)
acf(ynnadf)
pacf(ynnadf)
acf(ynadf)
pacf(ynadf)
acf(xjpadf)
pacf(xjpadf)
####ar(1)
arma_rbadf<-arma(rbadf,order = c(1,0))
arma_hgadf<-arma(hgadf,order = c(1,0))
arma_xgadf<-arma(xgadf,order = c(1,0))
arma_ydadf<-arma(ydadf,order = c(1,0))
arma_shadf<-arma(shadf,order = c(1,0))
arma_szadf<-arma(szadf,order = c(1,0))
arma_mladf<-arma(mladf,order = c(1,0))
arma_xjpadf<-arma(xjpadf,order = c(1,0))
arma_ynnadf<-arma(ynnadf,order = c(1,0))
arma_ynadf<-arma(ynadf,order = c(1,0))
arma_tgadf<-arma(tgadf,order = c(1,0))
arma_flbadf<-arma(flbadf,order = c(1,0))
#####
##ARCH效应检验
ArchTest(residuals(arma_rbadf))
ArchTest(residuals(arma_hgadf))
ArchTest(residuals(arma_xgadf))
ArchTest(residuals(arma_ydadf))
ArchTest(residuals(arma_shadf))
ArchTest(residuals(arma_szadf))
ArchTest(residuals(arma_mladf))
ArchTest(residuals(arma_ynnadf))
ArchTest(residuals(arma_ynadf))
ArchTest(residuals(arma_tgadf))
ArchTest(residuals(arma_xjpadf))
ArchTest(residuals(arma_flbadf))
####存在ARCH效应，即存在异方差
#由于gjr-garch在gamma=0时会退化至garch，所以我们选择gjr-garch建模
gjr_t.spec<-ugarchspec(variance.model = list(model="gjrGARCH",  
                                        garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,0)),
                                             distribution.model = "std" ) 
###分别使用了norm，std，ged后选择t
ar_gjr_rbadf<-ugarchfit(spec=gjr_t.spec,data=rbadf)
ar_gjr_hgadf<-ugarchfit(spec=gjr_t.spec,data=hgadf)
ar_gjr_xgadf<-ugarchfit(spec=gjr_t.spec,data=xgadf)
ar_gjr_ydadf<-ugarchfit(spec=gjr_t.spec,data=ydadf)
ar_gjr_shadf<-ugarchfit(spec=gjr_t.spec,data=shadf)
ar_gjr_szadf<-ugarchfit(spec=gjr_t.spec,data=szadf)
ar_gjr_mladf<-ugarchfit(spec=gjr_t.spec,data=mladf)
ar_gjr_tgadf<-ugarchfit(spec=gjr_t.spec,data=tgadf)
ar_gjr_ynnadf<-ugarchfit(spec=gjr_t.spec,data=ynnadf)
ar_gjr_ynadf<-ugarchfit(spec=gjr_t.spec,data=ynadf)
ar_gjr_xjpadf<-ugarchfit(spec=gjr_t.spec,data=xjpadf)
ar_gjr_flbadf<-ugarchfit(spec=gjr_t.spec,data=flbadf)
####标准残差
#标准化残差
residual_rbadf<-residuals(ar_gjr_rbadf,standardize=TRUE)
residual_hgadf<-residuals(ar_gjr_hgadf,standardize=TRUE)
residual_xgadf<-residuals(ar_gjr_xgadf,standardize=TRUE)
residual_ydadf<-residuals(ar_gjr_ydadf,standardize=TRUE)
residual_shadf<-residuals(ar_gjr_shadf,standardize=TRUE)
residual_szadf<-residuals(ar_gjr_szadf,standardize=TRUE)
residual_mladf<-residuals(ar_gjr_mladf,standardize=TRUE)
residual_tgadf<-residuals(ar_gjr_tgadf,standardize=TRUE)
residual_ynnadf<-residuals(ar_gjr_ynnadf,standardize=TRUE)
residual_ynadf<-residuals(ar_gjr_ynadf,standardize=TRUE)
residual_xjpadf<-residuals(ar_gjr_xjpadf,standardize=TRUE)
residual_flbadf<-residuals(ar_gjr_flbadf,standardize=TRUE)

##数据处理
residual_rbadf<-data.frame(residual_rbadf)[,1]
residual_hgadf<-data.frame(residual_hgadf)[,1]
residual_xgadf<-data.frame(residual_xgadf)[,1]
residual_ydadf<-data.frame(residual_ydadf)[,1]
residual_shadf<-data.frame(residual_shadf)[,1]
residual_szadf<-data.frame(residual_szadf)[,1]
residual_tgadf<-data.frame(residual_tgadf)[,1]
residual_xjpadf<-data.frame(residual_xjpadf)[,1]
residual_ynadf<-data.frame(residual_ynadf)[,1]
residual_ynnadf<-data.frame(residual_ynnadf)[,1]
residual_mladf<-data.frame(residual_mladf)[,1]
residual_flbadf<-data.frame(residual_flbadf)[,1]
############
install.packages("mFilter")
####概率积分变换
rbadf_01<-pobs(residual_rbadf)#1日本
hgadf_01<-pobs(residual_hgadf)#2韩国
xgadf_01<-pobs(residual_xgadf)#3香港
ydadf_01<-pobs(residual_ydadf)#4印度
shadf_01<-pobs(residual_shadf)#5上海
szadf_01<-pobs(residual_szadf)#6深圳
mladf_01<-pobs(residual_mladf)#7马来西亚
flbadf_01<-pobs(residual_flbadf)#8菲律宾
tgadf_01<-pobs(residual_tgadf)#9泰国
ynnadf_01<-pobs(residual_ynnadf)#10印尼
ynadf_01<-pobs(residual_ynadf)#11越南
xjpadf_01<-pobs(residual_xjpadf)#12新加坡
##k-s检验
ks.test(rbadf_01,runif(1000))
ks.test(hgadf_01,runif(1000))
ks.test(xgadf_01,runif(1000))
ks.test(ydadf_01,runif(1000))
ks.test(shadf_01,runif(1000))
ks.test(szadf_01,runif(1000))
ks.test(mladf_01,runif(1000))
ks.test(flbadf_01,runif(1000))
ks.test(tgadf_01,runif(1000))
ks.test(ynnadf_01,runif(1000))
ks.test(ynadf_01,runif(1000))
ks.test(xjpadf_01,runif(1000))
######adf-copula拟合
adfcopula<-data.frame(rbadf_01,hgadf_01,
                     xgadf_01,ydadf_01,
                     shadf_01,szadf_01,
                     mladf_01,flbadf_01,
                     tgadf_01,ynnadf_01,
                     ynadf_01,xjpadf_01)
adf<-data.frame(rbadf,hgadf,xgadf,ydadf,shadf,szadf,mladf,flbadf,tgadf,ynnadf,ynadf,xjpadf)

adf0_1<-apply(adf,2,pobs)

ks<-function(a){
  ks.test(a,runif(10000))
}
apply(adf0_1, 2, ks)
###

RVM_adf<-RVineStructureSelect(adf0_1,familyset = NA,type=0,progress = TRUE)



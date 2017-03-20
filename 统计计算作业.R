setwd("/Users/Lisiyan/Documents/作业/统计计算作业/")

library(ggplot2)
library(showtext)
library("latex2exp")
library(MASS)
library(fBasics)
showtext.auto(enable=T)
par(family="STSong")

load("data.rda")
dat2=dat2[dat2$volume>0,]
dat2$pct_chg[dat2$pct_chg>10]=10
dat2$pct_chg[dat2$pct_chg< -10]=-10
sam.time=unique(dat2$datetime)
dat2$pct_chg2=ifelse(dat2$pct_chg>=0,"涨","跌")

d=dat2[dat2$datetime==sam.time[2],]
pht=ggplot(data=d,aes(x=volume))+
  geom_histogram(aes(y=..density..,fill=pct_chg2),
                 alpha=1,bins=20,position="dodge")+
  scale_fill_manual(values=c("green","red"))+
  scale_x_continuous(limits=c(0,5e7),breaks=c(0,1e7,2e7,3e7,4e7,5e7),
                     labels=c(TeX("$ \\0.0 \\times 10^{7}$"),
                              TeX("$ \\1.0 \\times 10^{7}$"),
                              TeX("$ \\2.0 \\times 10^{7}$"),
                              TeX("$ \\3.0 \\times 10^{7}$"),
                              TeX("$ \\4.0 \\times 10^{7}$"),
                              TeX("$ \\5.0 \\times 10^{7}$")))+
  scale_y_continuous(breaks=c(0,5e-8,1e-7,1.5e-7,2e-7),
                     labels=c(TeX("$ \\0.0 \\times 10^{-8}$"),
                              TeX("$ \\5.0 \\times 10^{-8}$"),
                              TeX("$ \\1.0 \\times 10^{-7}$"),
                              TeX("$ \\1.5 \\times 10^{-7}$"),
                              TeX("$ \\2.0 \\times 10^{-7}$")))+
  labs(x="交易量",y="密度",fill="涨跌情况")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text.x=element_text(angle=0,hjust=0),
        legend.position=c(0.7,0.7),
        legend.key.width=unit(1.1,"cm"),
        legend.key=element_rect(colour='white',
                                fill='white',
                                size=1))
print(pht)
ggsave("直方图1.pdf",width=8,height=5)

qqnorm(d$volume,ylim=c(0,5e7),main="")
qqline(d$volume)

d$volume2=(d$volume^(-0.1)-1)/(-0.1)
pht2=ggplot(data=d,aes(x=volume2))+
  geom_histogram(aes(y=..density..,fill=pct_chg2),
                 alpha=1,bins=20,position="dodge")+
  scale_fill_manual(values=c("green","red"))+
  labs(x="Box-Cox变换后的交易量",y="密度",fill="涨跌情况")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text.x=element_text(angle=0,hjust=0),
        legend.position=c(0.3,0.7),
        legend.key.width=unit(1.1,"cm"),
        legend.key=element_rect(colour='white',
                                fill='white',
                                size=1))
print(pht2)
ggsave("直方图2.pdf",width=8,height=5)

qqnorm(d$volume2,ylim=c(7,8.5),main="")
qqline(d$volume2)



#正态性检验
tex1=data.frame(x=3.5e7,y=rep(1.25e-7,9),sk=sapply(sam.time,function(x)skewness(dat2$volume[dat2$datetime==x])),datetime=sam.time)
tex2=data.frame(x=3.5e7,y=rep(1e-7,9),ku=sapply(sam.time,function(x)kurtosis(dat2$volume[dat2$datetime==x])),datetime=sam.time)
ph1=ggplot(data=dat2,aes(x=volume))+
  geom_histogram(aes(y=..density..,fill=pct_chg2),
               alpha=1,bins=20,position="dodge")+
  scale_fill_manual(values=c("green","red"))+
  scale_x_continuous(limits=c(0,5e7),breaks=c(0,1e7,2e7,3e7,4e7,5e7),
                     labels=c(TeX("$ \\0.0 \\times 10^{7}$"),
                              TeX("$ \\1.0 \\times 10^{7}$"),
                              TeX("$ \\2.0 \\times 10^{7}$"),
                              TeX("$ \\3.0 \\times 10^{7}$"),
                              TeX("$ \\4.0 \\times 10^{7}$"),
                              TeX("$ \\5.0 \\times 10^{7}$")))+
  scale_y_continuous(breaks=c(0,5e-8,1e-7,1.5e-7,2e-7),
                     labels=c(TeX("$ \\0.0 \\times 10^{-8}$"),
                              TeX("$ \\5.0 \\times 10^{-8}$"),
                              TeX("$ \\1.0 \\times 10^{-7}$"),
                              TeX("$ \\1.5 \\times 10^{-7}$"),
                              TeX("$ \\2.0 \\times 10^{-7}$")))+
  geom_text(data=tex1,aes(x=x,y=y,label=paste("偏度=",round(sk,2),sep="")))+
  geom_text(data=tex2,aes(x=x,y=y,label=paste("峰度=",round(ku,2),sep="")))+
  labs(x="交易量",y="密度",fill="涨跌情况")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.position="right",
        legend.key.width=unit(1.1,"cm"),
        legend.key=element_rect(colour='white',
                                fill='white',
                                size=1))+
  facet_wrap(~datetime,ncol=3)
print(ph1)
ggsave("成交量直方图.pdf",width=8,height=5)

#box-cox变换
mod1=aov(dat2$volume~dat2$pct_chg2)
summary(mod1)
bc=boxcox(mod1,lambda=seq(-2,2,0.1))
dat2$volume2=(dat2$volume^(-0.1)-1)/(-0.1)
tex1=data.frame(x=6.5,y=rep(2,9),sk=sapply(sam.time,function(x)skewness(dat2$volume2[dat2$datetime==x])),datetime=sam.time)
tex2=data.frame(x=6.5,y=rep(1.5,9),ku=sapply(sam.time,function(x)kurtosis(dat2$volume2[dat2$datetime==x])),datetime=sam.time)
ph2=ggplot(data=dat2,aes(x=volume2))+
  geom_histogram(aes(y=..density..,fill=pct_chg2),
                 alpha=1,position="dodge",bins=20)+
  geom_text(data=tex1,aes(x=x,y=y,label=paste("偏度=",round(sk,2),sep="")))+
  geom_text(data=tex2,aes(x=x,y=y,label=paste("峰度=",round(ku,2),sep="")))+
  scale_fill_manual(values=c("green","red"))+
  labs(x="box-cox变换后的交易量",y="密度",fill="涨跌情况")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position="right",
        legend.key.width=unit(1.1,"cm"),
        legend.key=element_rect(colour='white',
                                fill='white',
                                size=1))+
  facet_wrap(~datetime,ncol=3)
print(ph2)
ggsave("成交量直方图2.pdf",width=8,height=5)

#等方差检验
fun1=function(x){
  return(c(time=x[1],state=x[2],sigma=round(sd(dat2$volume2[dat2$datetime==x[1] & dat2$pct_chg2==x[2]]),2)))
}
tmp=data.frame(t(apply(cbind(rep(as.character(sam.time),each=2),c("涨","跌")),1,fun1)))
write.table(tmp,file="方差检验.txt",sep="&",row.names=F)

for(i in sam.time){
  d=dat2[dat2$datetime==i,]
  print(summary(aov(volume2~pct_chg2,data=d)))
}

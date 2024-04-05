require(data.table)
require(lubridate)
require(forecast)
require(skimr)
require(repr)
require(openxlsx)
require(ggplot2)
require(data.table)
require(skimr)
require(GGally)
require(ggcorrplot)


require(forecast)

options(repr.plot.width=12, repr.plot.height=8)

data_path="/Users/serhatekli/Desktop/23-24\ spring/IE360/EVDS-28.xlsx"
data=read.xlsx(data_path)
head(data)

data_path="/Users/serhatekli/Desktop/23-24\ spring/IE360/EVDS-29.xlsx"
konut_satis=read.xlsx(data_path)
head(konut_satis)
str(konut_satis)
konut_satis$Tarih <- ym(konut_satis$Tarih)
konut_satis$TP.DK.EUR.C.YTL <- as.numeric(konut_satis$TP.DK.EUR.C.YTL)
konut_satis$TP.KTF12 <- as.numeric(konut_satis$TP.KTF12)
str(konut_satis)

data_path="/Users/serhatekli/Desktop/23-24\ spring/IE360/EVDS-30.xlsx"
araba_alma_beklentisi=read.xlsx(data_path)
head(araba_alma_beklentisi)
str(araba_alma_beklentisi)
araba_alma_beklentisi$Tarih <- ym(araba_alma_beklentisi$Tarih)
araba_alma_beklentisi$TP.BRENTPETROL.EUBP <- as.numeric(araba_alma_beklentisi$TP.BRENTPETROL.EUBP)
araba_alma_beklentisi$TP.KTF11 <- as.numeric(araba_alma_beklentisi$TP.KTF11)
str(araba_alma_beklentisi)

data_path="/Users/serhatekli/Desktop/23-24\ spring/IE360/EVDS-31.xlsx"
issizlik_orani=read.xlsx(data_path)
head(issizlik_orani)
str(issizlik_orani)
issizlik_orani$Tarih <- ym(issizlik_orani$Tarih)
issizlik_orani$TP.DK.USD.C.YTL <- as.numeric(issizlik_orani$TP.DK.USD.C.YTL)
issizlik_orani$TP.KAP2.TOP.A <- as.numeric(issizlik_orani$TP.KAP2.TOP.A)
str(issizlik_orani)

str(data)
data$Tarih <- ym(data$Tarih)
data$TP.YISGUCU2.G8 <- as.numeric(data$TP.YISGUCU2.G8)
data$TP.AKONUTSAT1.TOPLAM <- as.numeric(data$TP.AKONUTSAT1.TOPLAM)
data$TP.TG2.Y17 <- as.numeric(data$TP.TG2.Y17)
str(data)

summary_data = skim(data)
print(summary_data)

require(GGally)
ggpairs(data)

issizlik_orani_trend = "/Users/serhatekli/Desktop/23-24\ spring/IE360/is_ilanlari.csv"
araba_alma_beklentisi_trend = "/Users/serhatekli/Desktop/23-24\ spring/IE360/araba_fiyatlari.csv"
konut_satis_trend = "/Users/serhatekli/Desktop/23-24\ spring/IE360/konut_kredisi.csv"

issizlik_orani_trend = read.csv(issizlik_orani_trend,header = TRUE,stringsAsFactors = FALSE, sep = ",")
araba_alma_beklentisi_trend = read.csv(araba_alma_beklentisi_trend,header = TRUE,stringsAsFactors=FALSE, sep = ",")
konut_satis_trend = read.csv(konut_satis_trend,header = TRUE,stringsAsFactors=FALSE, sep = ",")

str(issizlik_orani_trend)
str(araba_alma_beklentisi_trend)
str(konut_satis_trend)

issizlik_orani_trend$Ay <- ym(issizlik_orani_trend$Ay)
araba_alma_beklentisi_trend$Ay <- ym(araba_alma_beklentisi_trend$Ay)
konut_satis_trend$Ay <- ym(konut_satis_trend$Ay)

ggplot(data, aes(x=Tarih,y=TP.YISGUCU2.G8)) + geom_line()
ggplot(issizlik_orani_trend, aes(x=Ay,y=iş.ilanları)) + geom_line()

ggplot(data ,aes(x=Tarih,y=data$TP.AKONUTSAT1.TOPLAM)) + geom_line()
ggplot(konut_satis_trend, aes(x=Ay,y=konut.kredisi)) + geom_line()

ggplot(data ,aes(x=Tarih,y=data$TP.TG2.Y17)) + geom_line()
ggplot(araba_alma_beklentisi_trend, aes(x=Ay,y=araba.fiyatları)) + geom_line()

require(tidyverse)

colnames(issizlik_orani_trend)[colnames(issizlik_orani_trend)=='Ay'] <- 'Tarih'
set <- list(data,issizlik_orani_trend,issizlik_orani)
issizlik_orani_data <- set %>% reduce(inner_join, by='Tarih')
head(issizlik_orani_data)

ggpairs(issizlik_orani_data)

colnames(konut_satis_trend)[colnames(konut_satis_trend)=='Ay'] <- 'Tarih'
set <- list(data,konut_satis_trend,konut_satis)
konut_satis_data <- set %>% reduce(inner_join, by='Tarih')
head(konut_satis_data)

ggpairs(konut_satis_data)

colnames(araba_alma_beklentisi_trend)[colnames(araba_alma_beklentisi_trend)=='Ay'] <- 'Tarih'
set <- list(data,araba_alma_beklentisi_trend,araba_alma_beklentisi)
araba_alma_beklentisi_data <- set %>% reduce(inner_join, by='Tarih')
head(araba_alma_beklentisi_data)

ggpairs(araba_alma_beklentisi_data)

ggplot(data ,aes(x=Tarih,y=data$TP.YISGUCU2.G8)) + geom_point()+geom_smooth(method='lm')

ggplot(data ,aes(x=Tarih,y=data$TP.AKONUTSAT1.TOPLAM)) + geom_point()+geom_smooth(method='lm')

ggplot(data ,aes(x=Tarih,y=data$TP.TG2.Y17)) + geom_point()+geom_smooth(method='lm')

issizlik_orani_data <- as.data.table(issizlik_orani_data)
issizlik_orani_data[,trnd:=1:.N]
issizlik_orani_data[,yil:=as.character(month(Tarih,label=T))]
issizlik_orani_data[,ay:=as.character(month(Tarih,label=T))]
head(issizlik_orani_data)

ggplot(issizlik_orani_data,aes(x=TP.YISGUCU2.G8,y=TP.DK.USD.C.YTL)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

ggplot(issizlik_orani_data,aes(x=TP.YISGUCU2.G8,y=TP.KAP2.TOP.A)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

ggplot(issizlik_orani_data,aes(x=TP.YISGUCU2.G8,y=issizlik_orani_data$iş.ilanları)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

lm_base=lm(TP.YISGUCU2.G8~trnd+ay+yil+issizlik_orani_data$iş.ilanları+TP.DK.USD.C.YTL+TP.KAP2.TOP.A,issizlik_orani_data)
summary(lm_base)

checkresiduals(lm_base$residuals)

tmp=copy(issizlik_orani_data)
tmp[,actual:=TP.YISGUCU2.G8]
tmp[,predicted_trend:=predict(lm_base,tmp)]
tmp[,residual_trend:=actual-predicted_trend]
ggplot(tmp ,aes(x=Tarih)) +
  geom_line(aes(y=actual,color='real')) + 
  geom_line(aes(y=predicted_trend,color='predicted'))



araba_alma_beklentisi_data <- as.data.table(araba_alma_beklentisi_data)
araba_alma_beklentisi_data[,trnd:=1:.N]
araba_alma_beklentisi_data[,yil:=as.character(month(Tarih,label=T))]
araba_alma_beklentisi_data[,ay:=as.character(month(Tarih,label=T))]
head(araba_alma_beklentisi_data)

ggplot(araba_alma_beklentisi_data,aes(x=TP.TG2.Y17,y=TP.BRENTPETROL.EUBP)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

ggplot(araba_alma_beklentisi_data,aes(x=TP.TG2.Y17,y=TP.KTF11)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

ggplot(araba_alma_beklentisi_data,aes(x=TP.TG2.Y17,y=araba_alma_beklentisi_data$araba.fiyatları)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

lm_base=lm(TP.TG2.Y17~trnd+ay+yil+araba_alma_beklentisi_data$araba.fiyatları+TP.BRENTPETROL.EUBP+TP.KTF11,araba_alma_beklentisi_data)
summary(lm_base)

checkresiduals(lm_base$residuals)

tmp=copy(araba_alma_beklentisi_data)
tmp[,actual:=TP.TG2.Y17]
tmp[,predicted_trend:=predict(lm_base,tmp)]
tmp[,residual_trend:=actual-predicted_trend]
ggplot(tmp ,aes(x=Tarih)) +
  geom_line(aes(y=actual,color='real')) + 
  geom_line(aes(y=predicted_trend,color='predicted'))



konut_satis_data <- as.data.table(konut_satis_data)
konut_satis_data[,trnd:=1:.N]
konut_satis_data[,yil:=as.character(month(Tarih,label=T))]
konut_satis_data[,ay:=as.character(month(Tarih,label=T))]
head(konut_satis_data)

ggplot(konut_satis_data,aes(x=TP.AKONUTSAT1.TOPLAM,y=TP.DK.EUR.C.YTL)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

ggplot(konut_satis_data,aes(x=TP.AKONUTSAT1.TOPLAM,y=TP.KTF12)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

ggplot(konut_satis_data,aes(x=TP.AKONUTSAT1.TOPLAM,y=konut_satis_data$konut.kredisi)) +
  geom_point() + geom_smooth(method=lm,linewidth=3) + facet_wrap(~ay)

lm_base=lm(TP.AKONUTSAT1.TOPLAM~trnd+ay+yil+konut_satis_data$konut.kredisi+TP.DK.EUR.C.YTL+TP.KTF12,konut_satis_data)
summary(lm_base)

checkresiduals(lm_base$residuals)

tmp=copy(konut_satis_data)
tmp[,actual:=TP.AKONUTSAT1.TOPLAM]
tmp[,predicted_trend:=predict(lm_base,tmp)]
tmp[,residual_trend:=actual-predicted_trend]
ggplot(tmp ,aes(x=Tarih)) +
  geom_line(aes(y=actual,color='real')) + 
  geom_line(aes(y=predicted_trend,color='predicted'))




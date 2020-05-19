#packages
library(stringr)
library(dplyr)
library(forecast)
library(ggplot2)
library(RCurl)
library(xts)
#

# download dataset
# https://covid.ourworldindata.org/data/owid-covid-data.csv
fileurl = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
localD = "G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/owid-covid-data.csv"
download.file(fileurl, localD, method = "curl")

# reference link:https://ourworldindata.org/coronavirus-source-data
setwd("G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/")

df_covid = read.csv("owid-covid-data.csv")
head(df_covid)

# organize time series

df = df_covid %>% filter(location == "World")
tail(df)
tc = df[,c(3,4)]
tc

scatter.smooth(x=tc$date, tc$total_cases, main="Date ~ Total Cases")

ts = ts(tc$total_cases)
ts
time = as.integer(length(ts)*0.7)
f_time = length(ts)
# test better model
train = window(ts, start= 1, end= time)
test = window(ts, start=time, end=f_time)
f_time - time
DFor =  f_time - time

#naive
naive = naive(train, h=DFor)

#mean
meanf = meanf(train, h=DFor)

#drift
rwf = rwf(train, h=DFor, drift = T)

#holt - pesos
holt = holt(train, h=DFor)

#arima
arima = auto.arima(train)
arima = forecast(arima, h=DFor)

#linear
tslm = tslm(train ~ trend, data=train)
tslm = forecast(tslm, h=DFor)

#rede neural
nnetar = nnetar(train)
nnetar = forecast(nnetar, h=DFor)
nnetar

# acurracy
anaive = accuracy(test,naive$mean)
ameanf = accuracy(test,meanf$mean)
arwf = accuracy(test,rwf$mean)
aholt = accuracy(test,holt$mean)
aarima = accuracy(test,arima$mean)
atslm = accuracy(test,tslm$mean)
annetar = accuracy(test,nnetar$mean)

acurracy = rbind(anaive,ameanf,arwf,aholt,aarima,atslm,annetar)
Names = c("Naive","Mean","Drift","Holt","Arima","Linear Regression", "Neural Net")
acurracy = cbind(Names,acurracy)
acurracy
# plot
plot(ts, main = "Forecast Benchmark")
lines(naive$mean, type="l", pch=22, lty=6,  col="red", lwd=2)
lines(meanf$mean, type="l", pch=22, lty=5,  col="blue", lwd=2)
lines(rwf$mean,  type="l", pch=22, lty=4, col="green",lwd=2)
lines(holt$mean, type="l", pch=22, lty=3,  col="orange", lwd=2)
lines(arima$mean, type="l", pch=22, lty=5,  col="maroon", lwd=2)
lines(tslm$mean, type="l", pch=22, lty=4,  col="cyan",lwd=2)
lines(nnetar$mean,  type="l", pch=22, lty=3, col="yellow",lwd=2)
legend("bottomleft",legend=c("Naive","Mean","Dri.","Hol.","Ari.","LR","NN"),
       col = c("red","blue","green","orange","maroon","cyan","yellow"),
       lty=1:2, cex=0.8, ncol = 2,lwd=4)

# Forecast
# best models
# 1°
FHolt = holt(ts, h=5)
FHolt$mean
# 2°
FArima = auto.arima(ts)
FArima = forecast(FArima, h = 5)
FArima$mean

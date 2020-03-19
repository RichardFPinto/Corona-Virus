#bibliotecas
library(stringr)
library(dplyr)
library(forecast)
library(ggplot2)

# fazendo o download do dataset
arquivourl = "https://covid.ourworldindata.org/data/full_data.csv"
destino = "G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/full_data.csv"
download.file(arquivourl, destino, method = "curl")

# carregando os dados, eles foram retirados do link:https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset
setwd("G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/")

df_corona = read.csv("full_data.csv")
head(df_corona)
# Criando uma curva de regressão que mais se adapta a curva de crescimento das infecções do corona virus

df_brasil = df_corona %>% filter(location == "Brazil")
df_brasil
tc_brasil = df_brasil[,c(1,5)]
tc_brasil

df_italia = df_corona %>% filter(location == "Italy")
df_italia
tc_italia = df_italia[,c(1,5)]
tc_italia

df_china = df_corona %>% filter(location == "China")
df_china
tc_china = df_china[,c(1,5)]
tc_china

plot(tc_china$date,tc_china$total_cases,type="l")
lines(tc_brasil$date,tc_brasil$total_cases)
lines(tc_italia$date,tc_italia$total_cases)

scatter.smooth(x=tc_china$date, tc_china$total_cases, main="Data ~ Infectados - China")
scatter.smooth(x=tc_italia$date, tc_italia$total_cases, main="Data ~ Infectado - Italia")
scatter.smooth(x=tc_brasil$date, tc_brasil$total_cases, main="Data ~ Infectado - Brasil")

time_br = 1:nrow(tc_brasil)
time_br
# modelo simples
mbr = lm(total_cases ~ time_br, data=tc_brasil)
plot(total_cases ~ time_br , data=tc_brasil)
abline(mbr)
anova(mbr)
print(mbr)
summary(mbr)


# modelo com logaritmo
mbr2 = lm(log(total_cases) ~ time_br, data=tc_brasil)
plot(log(total_cases) ~ time_br , data=tc_brasil)
abline(mbr2)
anova(mbr2)
print(mbr2)
summary(mbr2)

# fazendo a predição
x = predict(mbr2,data.frame(time_br = (nrow(tc_brasil)+1)))
print(exp(x))

# fazer para a italia, mas não farei para a china pois já está controlado lá

time_ita = 1:nrow(tc_italia)
time_ita
# modelo simples
mita = lm(total_cases ~ time_ita, data=tc_italia)
plot(total_cases ~ time_ita , data=tc_italia)
abline(mita)
anova(mita)
print(mita)
summary(mita)


# modelo com logaritmo
mita2 = lm(log(total_cases) ~ time_ita, data=tc_italia)
plot(log(total_cases) ~ time_ita , data=tc_italia)
abline(mita2)
anova(mita2)
print(mita2)
summary(mita2)

# fazendo a predição
x = predict(mita2,data.frame(time_ita = (nrow(tc_italia)+1)))
print(exp(x))

# agora vamos fazer para o mundo inteiro

df_mundo = df_corona %>% filter(location == "World")
df_mundo
tc_mundo = df_mundo[,c(1,5)]
tc_mundo
# plotando grafico
scatter.smooth(x=tc_mundo$date, tc_mundo$total_cases, main="Data ~ Infectado - No Mundo")
# fazendo uma variavel com os dias
time_m = 1:nrow(tc_mundo)
time_m
# fazendo a regressão simples dos dados
mm = lm(total_cases ~ time_m, data=tc_mundo)
plot(total_cases ~ time_m , data=tc_mundo)
abline(mm)
anova(mm)
print(mm)
summary(mm)
x = predict(mm,data.frame(time_m = (nrow(tc_mundo)+1)))
print(x)
# fazendo a regressão logaritmica
tc_mundo$total_cases
mm2 = lm(log(total_cases) ~ time_m, data=tc_mundo)
plot(log(total_cases) ~ time_m , data=tc_mundo)
abline(mm2)
anova(mm2)
print(mm2)
summary(mm2)

x = predict(mm2,data.frame(time_m = (nrow(tc_mundo)+1)))
print(exp(x))



# tentando fazer com a previsão usando media moveis
# ordem 5
mediamundo1 = ma(tc_mundo$total_cases, order = 5 )
autoplot(mediamundo1)
mm5mundo= lm(mediamundo1 ~ time_m)
plot(mediamundo1 ~ time_m , data=tc_mundo)
abline(mm5mundo)


mediamundo2 = ma(tc_mundo$total_cases, order = 8 )
autoplot(mediamundo2)
mm8mundo= lm(mediamundo2 ~ time_m)
plot(mediamundo2 ~ time_m , data=tc_mundo)
abline(mm8mundo)


mediamundo3 = ma(tc_mundo$total_cases, order = 10 )
autoplot(mediamundo3)
mm10mundo= lm(mediamundo3 ~ time_m)
plot(mediamundo3 ~ time_m , data=tc_mundo)
abline(mm10mundo)

plot(tc_mundo)
lines(mediamundo1, col = "blue")
lines(mediamundo2, col = "red")
lines(mediamundo3, col = "green")
legend("topleft",legend=c("Orig.","MM5","MM8","MM10"), col = c("black","blue","red","green"), lty=1:2, cex=0.8,)

# usando o autoarima para prever
ts_mundo = ts(tc_mundo$total_cases)
ts_mundo

modelo = auto.arima(ts_mundo, trace = T,stepwise = F, approximation = F )
print(modelo)
previsão = forecast(modelo,h=4)
plot(previsão)
lines(previsão$mean, col = "red")

print(previsão$mean)

# vamos usar o auto-arima para prever do brasil e italia
# brasil
ts_brasil = ts(tc_brasil$total_cases)
ts_brasil

modelobr = auto.arima(ts_brasil, trace = T,stepwise = F, approximation = F )
print(modelobr)
previsãobr = forecast(modelobr,h=4)
plot(previsãobr)
lines(previsãobr$mean, col = "red")

print(previsãobr)
print(previsãobr$mean)

# italia
ts_italia = ts(tc_italia$total_cases)
ts_italia

modeloita = auto.arima(ts_italia, trace = T,stepwise = F, approximation = F )
print(modeloita)
previsãoita = forecast(modeloita,h=4)
plot(previsãoita)
lines(previsãoita$mean, col = "red")

print(previsãoita$mean)

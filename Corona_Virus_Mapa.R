
library(stringr)
library(dplyr)
library(leaflet)
library(mapview)
# fazendo o download do dataset
arquivourl = "https://covid.ourworldindata.org/data/full_data.csv"
nomearquivo = str_c(Sys.Date(),"full_data.csv")
destino = str_c("G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/",nomearquivo)
download.file(arquivourl, destino, method = "curl")

# carregando os dados, eles foram retirados do link:https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset
setwd("G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/")
# organizando o dataset
df_corona = read.csv(nomearquivo)
head(df_corona)
data = as.character(Sys.Date())
df_mapa = df_corona %>% filter(date == data)
head(df_mapa)
worldwide = df_mapa %>% filter(location == "Worldwide")
International = df_mapa %>% filter(location == "International")
Outros = rbind(worldwide,International)
df_mapa = df_mapa %>% anti_join(Outros, by = "location")
df_mapa$location = as.character(df_mapa$location)
localizacao = read.csv("countries.csv")
head(localizacao[,-1])
paises = localizacao[,-1]
paises = paises %>% rename(location = Country)
head(paises,50)
df_mapa = df_mapa %>% left_join(paises, by = "location")

# para imprimir que não tiver com latitude 
#x = which(is.na(teste$Latitude))
#for (i in 1: length(x)){
#  print(teste[x[i],2])
#}
head(df_mapa)
diretorio = str_c("G:/Github/Projetos Prontos/20200303 Corona Virus/2 - Dados Preparados/",Sys.Date()," Df_corona.csv" )
write.csv(df_mapa,diretorio, row.names = FALSE)

# criando o mapa
# colocando cores nos marcadores baseado na quantidade de pessoas
cores = c()
for(i in 1:nrow(df_mapa)){
  if(df_mapa$total_cases[i]>= 20000){cores[i] = "black"}
  if(df_mapa$total_cases[i]>= 5000 && df_mapa$total_cases[i]< 20000){cores[i] = "red"}
  if(df_mapa$total_cases[i]>= 1000 && df_mapa$total_cases[i]< 5000){cores[i] = "orange"}
  if(df_mapa$total_cases[i]>= 50 &&df_mapa$total_cases[i]< 1000){cores[i] = "green"}
  if(df_mapa$total_cases[i]< 50){cores[i] = "blue"}
  
}

icones = awesomeIcons(icon="ios-close",library="ion",iconColor="Black",markerColor= cores)
comentarios = paste(sep = "<br/>",
                    "<b> Cidade/Provincia:</b>",
                    as.character(df_mapa$location),
                    "<b> Confirmados:</b>",
                    as.character(df_mapa$total_cases),
                    "<b> Mortos:</b>",
                    as.character(df_mapa$total_deaths)
)
paleta = c("blue", "green", "orange","red","black")
valores = c("0-49 pessoas", "50-999 pessoas", "1000 - 4999 pessoas", "5000 - 19999 pessoas", "Mais de 20000 pessoas:")
mapa = leaflet(
) %>% addProviderTiles(providers$Esri.NatGeoWorldMap
)%>% addAwesomeMarkers(df_mapa$Longitude,df_mapa$Latitude ,icon=icones,popup = comentarios, label = as.character(df_mapa$location)
)%>% addLegend("bottomright", labels = valores, title = "Pessoas infectadas com Corona Virus",opacity = 1, colors = paleta, labFormat = labelFormat(prefix = "$"))
mapa
# criando um .hmtl para poder visualizar o mapa
urlname = str_c("G:/Github/Projetos Prontos/20200303 Corona Virus/5 - Insights/",Sys.Date()," CoronaMapa.html" )
# exportando o mapa como html
mapshot(mapa,url = paste0(urlname))

# packages
library(stringr)
library(dplyr)
library(leaflet)
library(mapview)

# download dataset
# https://covid.ourworldindata.org/data/owid-covid-data.csv
fileurl = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
localD = "G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/owid-covid-data.csv"
download.file(fileurl, localD, method = "curl")

# reference link:https://ourworldindata.org/coronavirus-source-data
setwd("G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/")

df_covid = read.csv("owid-covid-data.csv")
localizacao = read.csv("countries.csv")

# organizando o dataset
head(df_corona)
table(df_corona$location)
df_covid %>% filter(location == "Brazil")
datesys = as.character(Sys.Date())
df_map = df_covid %>% filter(date == datesys)
if(nrow(df_mapa)< 2){
  datesys = as.character(Sys.Date()-1)
  df_map = df_covid %>% filter(date == datesys)
}
  
head(df_mapa)

worldwide = df_map %>% filter(location == "World")
International = df_map %>% filter(location == "International")
others = rbind(worldwide,International)
Others
df_map = df_map %>% anti_join(Outros, by = "location")
df_mapa$location = as.character(df_mapa$location)

country = localizacao[,-1]
country = country %>% rename(location = Country)
df_map = df_map %>% left_join(paises, by = "location")

# para imprimir que não tiver com latitude 
x = which(is.na(df_map$Latitude))
for (i in 1: length(x)){
  print(x)
  print(df_map[x[i],2])
}
for (j in 3: ncol(df_map)){
  y = which(is.na(df_map[,j]))
  if(length(y)>= 1){
      for (i in 1: length(y)){
        df_map[y[i],j] = 0
}}}
head(df_map)
localSave = "G:/Github/Projetos Prontos/20200303 Corona Virus/2 - Dados Preparados/Df_corona.csv"
write.csv(df_map,localsave, row.names = FALSE)

# criando o mapa
# colocando cores nos marcadores baseado na quantidade de pessoas
cores = c()
for(i in 1:nrow(df_map)){
  if(df_map$total_cases[i]>= 20000){cores[i] = "black"}
  if(df_map$total_cases[i]>= 5000 && df_map$total_cases[i]< 20000){cores[i] = "red"}
  if(df_map$total_cases[i]>= 1000 && df_map$total_cases[i]< 5000){cores[i] = "orange"}
  if(df_map$total_cases[i]>= 50 &&df_map$total_cases[i]< 1000){cores[i] = "green"}
  if(df_map$total_cases[i]< 50){cores[i] = "blue"}
  
}

icones = awesomeIcons(icon="ios-close",library="ion",iconColor="Black",markerColor= cores)
comentarios = paste(sep = "<br/>",
                    "<b> City:</b>",
                    as.character(df_map$location),
                    "<b> Total cases:</b>",
                    as.character(df_map$total_cases),
                    "<b> New cases:</b>",
                    as.character(df_map$new_cases),
                    "<b> Total deaths:</b>",
                    as.character(df_map$total_deaths),
                    "<b> New deaths:</b>",
                    as.character(df_map$new_deaths)
)
paleta = c("blue", "green", "orange","red","black")
valores = c("0-49 people", "50-999 people", "1000 - 4999 people", "5000 - 19999 people", "Mais de 20000 people:")
map = leaflet(
) %>% addProviderTiles(providers$Esri.NatGeoWorldMap
)%>% addAwesomeMarkers(df_map$Longitude,df_map$Latitude ,icon=icones,popup = comentarios, label = as.character(df_map$location)
)%>% addLegend("bottomright", labels = valores, title = "People with COVID",opacity = 1, colors = paleta, labFormat = labelFormat(prefix = "$"))
map

directory = "G:/Github/Projetos Prontos/20200303 Corona Virus/5 - Insights/CovidMap.html" 
# export map
mapshot(map,url = paste0(directory))

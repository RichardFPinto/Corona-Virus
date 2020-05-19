# packages
library(shiny)
library(stringr)
library(dplyr)
library(forecast)
library(ggplot2)

# download dataset
# https://covid.ourworldindata.org/data/owid-covid-data.csv
fileurl = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
localD = "G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/owid-covid-data.csv"
download.file(fileurl, localD, method = "curl")

# reference link:https://ourworldindata.org/coronavirus-source-data
setwd("G:/Github/Projetos Prontos/20200303 Corona Virus/1 - Dados Originais/")

df_covid = read.csv("owid-covid-data.csv") 

local = unique(df_corona$location)

ui <- fluidPage(
    titlePanel("Forecast COVID-19"),
    #inputs
    fluidRow(
        column(4,
               selectInput("local","Local:", choices = local, selected = "World")
        ),
        column(4,
               numericInput("ForecastTime", "How many days you want to forecast:", 4, min = 1, max = 12)
        
        ),
        column(4,
               radioButtons("Opcao", label = "Which you want to forecast:",
                     choices = list("Total Cases" = 1, "New Cases" = 2, "Total Deaths" = 3, "New Deaths" = 4), 
                     selected = 1),
               actionButton("Process","Process")
        )
    ),
    #outputs
    fluidRow(
        column(12,
               
               plotOutput("Graf")
        )),
    fluidRow(
        column(4,
                   
               h2(textOutput("Lpreve")),
               tableOutput("lower")
        ),
        column(4,
               h2(textOutput("Mpreve")),
               tableOutput("mean")
        ),
        column(4,
               h2(textOutput("Upreve")),
               tableOutput("upper")
        )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$Process,
                 {
    # construindo o modelo
    location = as.character(input$local)
    data = df_covid %>% filter(location == location)
    
    tipo = input$Opcao
    if (tipo==1){ 
        x = data$total_cases
        tit = "Total cases" 
    }
    if (tipo==2){ 
        x = dados$new_cases
        tit = "New cases"
        
    }
    if (tipo==3) { 
        x = dados$total_deaths  
        tit = "Total deaths"
    }
    if (tipo==4) { 
        x = dados$new_deaths
        tit = "New deaths"
    }
    
    TSdata  = ts(x)
    Time = input$ForecastTime
    Fore = holt(TSdata, h = Time)
    
    output$Graf <- renderPlot({
        plot(Fore, main = tit, ylab = " People", xlab = "Days")
        lines(Fore$mean, col = "red")
        
    })
    output$lower <- renderTable({Fore$lower})
    output$mean <- renderTable({Fore$mean})
    output$upper <- renderTable({Fore$upper})
    
    output$Lpreve = renderText({"Forecast lower" })   
    output$Mpreve = renderText({"Forecast mean" })
    output$Upreve = renderText({"Forecast upper" }) 
    
}
)}
# Run the application 
shinyApp(ui = ui, server = server)

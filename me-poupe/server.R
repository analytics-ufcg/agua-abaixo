library(shiny)
library(plotly)
source("R/olhonagua-lib.R")

# TODO: como atualizar esse arquivo automaticamente?
sumario = read_csv("olhonagua/periodos_de_consumo.csv") 
consumos = sumario %>% get_consumos()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$exemplo1Plot <- renderPlotly({
    p = ver_consumo(12172, sumario)
    ggplotly(p)
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
  })
  
  output$plotEscolhido <- renderPlotly({
      p = ver_consumo(input$reservatorio, sumario)
      ggplotly(p)
  })
})

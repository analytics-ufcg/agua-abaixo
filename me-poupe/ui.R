library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)

source("R/Populacao reservatorios conhecidos.R")
info_cidades_res = cidade_municipio("olhonagua/abastecimento_municipio.csv", 
                                    "snis/informacoes_gerais.csv")
reservatorios = info_cidades_res %>% 
    select(COD_ANA, RESERVATORIO) %>% 
    dplyr::distinct() %>% 
    group_by(COD_ANA) %>% 
    summarise(RESERVATORIO = first(RESERVATORIO))

res_options = list()
for (i in reservatorios$COD_ANA) {
    label = reservatorios[reservatorios$COD_ANA == i,]$RESERVATORIO
    res_options[[label]] <- i
}

shinyUI(
    fluidPage(
        theme = shinytheme("cerulean"),
        titlePanel("Me Poupe!"),
        p("Lorem Impsum. Lorem Impsum. Lorem Impsum. Lorem Impsum. Lorem Impsum."),
        fluidRow(
            column(6, 
                   h3("um exemplo"),
                   p(em("blablabla")),
                   fluidRow(
                       plotlyOutput("exemplo1Plot")
                   ),
                   offset = 0
            )
        ),
        fluidRow(
            column(6, 
                   h3("procure você"),
                   p(em("blablabla")),
                   wellPanel(
                       selectInput(
                           'reservatorio',
                           'Escolha um reservatório',
                           choices = res_options,
                           selected = res_options[runif(1, 0, 39)],
                           selectize = TRUE
                       ) 
                   ),
                   fluidRow(
                       plotlyOutput("plotEscolhido")
                   ),
                   offset = 0
            )
        )
    )
)
    
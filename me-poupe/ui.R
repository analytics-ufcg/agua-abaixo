library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)

source("R/Populacao reservatorios conhecidos.R")

# Ler dados
info_cidades_res = cidade_municipio("olhonagua/abastecimento_municipio.csv", 
                                    "snis/informacoes_gerais.csv")
reservatorios = info_cidades_res %>% 
    select(COD_ANA, RESERVATORIO) %>% 
    dplyr::distinct() %>% 
    group_by(COD_ANA) %>% 
    summarise(RESERVATORIO = first(RESERVATORIO))

# Opções para o combobox dos reservatórios
res_options = list()
for (i in reservatorios$COD_ANA) {
    label = reservatorios[reservatorios$COD_ANA == i,]$RESERVATORIO
    res_options[[label]] <- i
}

shinyUI(
    fluidPage(
        theme = shinytheme("cerulean"),
        tags$head(tags$link(rel="shortcut icon", href="poupeme-favicon.png")),
        fluidRow(
            column(8, 
                   img(src="poupeme-logo.png", style = "margin-left: auto; margin-right: auto; display: block; margin-top: 15px; margin-bottom: 50px"),
                   p("Estamos em uma crise hídrica. Mas estamos poupando água? Quanto?"),
                   p("Usamos dados de reservatórios no semiárido para avaliar como diferentes regiões consumiram água desde o início da estiagem."),
                   offset = 2
            )
        ),
        fluidRow(
            column(8, 
                   h3("Um exemplo: Campina Grande e arredores"),
                   p("De 2012 a 2015 as cidades que utilizam o açude de Boqueirão diminuíram bastante o consumo de água. Daí em diante o consumo continou reduzindo, mas cada vez mais lentamente. De 2016 em diante, o consumo praticamente estabilizou, sem reduções consideráveis."),
                   offset = 2
            )
        ),
        fluidRow(
            column(4, 
                   plotlyOutput("exemplo1Plot"), 
                   offset = 2
            ),
            column(4, 
                   p(em("Para o consumo, cada ponto é a estimativa de litros/pessoa usados por dia na região. Não separamos consumo para diferentes fins: indústria, agronomia, pecuária, etc. A linha azul clara é uma estimativa da tendência do consumo ao longo do tempo. ")), 
                   style = "display: flex;align-items: center;"
            ), 
            style = "display: flex;"
        ), 
        fluidRow(
            column(4, 
                plotlyOutput("exemplo1Volume"),
                offset = 2
            ),
            column(4, 
                p(em("Aqui vemos quanto o açude tinha de água ao longo do tempo, em percentual.")), 
                style = "display: flex;align-items: center;"
            ), 
            style = "display: flex;"
        ), 
        fluidRow(
            column(8, 
                   h3("Explore os reservatórios"),
                   p("Selecione um reservatório para visualizar como os municípios que o utilizam consumiram água desde o início da estiagem."),
                   p(em("Um detalhe importante: conseguimos apenas estimar consumo para alguns reservatórios (só quando as cidades da região utilizam um só reservatório).")),
                   wellPanel(
                       selectInput(
                           'reservatorio',
                           'Escolha um reservatório',
                           choices = res_options,
                           selected = res_options[runif(1, 0, 39)],
                           selectize = TRUE
                       ) 
                   ),
                   textOutput('populacaoAtendida', inline = TRUE),
                   br(),
                   textOutput('municipiosAtendidos', inline = TRUE),
                   br(),
                   column(6,
                       plotlyOutput("plotEscolhido")
                   ),
                   column(6, 
                       plotlyOutput("plotVolume")
                   ),
                   offset = 2
            )
        )
    )
)
    
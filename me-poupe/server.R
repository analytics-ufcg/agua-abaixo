library(shiny)
library(plotly)
library(readr)
library(lubridate)
source("R/olhonagua-lib.R")

#
# LER, LIMPAR
#
# TODO: como atualizar esses arquivos automaticamente?
historicos = read_csv("olhonagua/historicos_reservatorios.csv") %>% 
    mutate(Data = ymd(Data))
sumario = read_csv("olhonagua/periodos_de_consumo.csv") 
consumos = sumario %>% get_consumos()

info_cidades_res = cidade_municipio("olhonagua/abastecimento_municipio.csv", 
                                    "snis/informacoes_gerais.csv")

populacoes = habitantes_por_reservatorio("olhonagua/abastecimento_municipio.csv", 
                                         "snis/informacoes_gerais.csv")

#
# O SERVIDOR
#
shinyServer(function(input, output) {
    reservatorio_atual = reactive ({
        input$reservatorio
    })
    
    output$exemplo1Plot <- renderPlotly({
        p = ver_consumo(12172, sumario) +
            ggtitle("Consumo diário (estimado)")
        ggplotly(p)
    })
    
    output$exemplo1Volume <- renderPlotly({
        p = historicos %>% 
            filter(id == 12172, Ano >= 2012) %>% 
            ggplot(aes(x = Data, y = VolumePercentual)) + 
            ylab("Volume percentual") + 
            geom_area(fill = "#2b8cbe") + 
            ggtitle("Volume (%) do reservatório")
            xlim(ymd("2012-01-01", today()))
        ggplotly(p)
    })
    
    
    output$populacaoAtendida <- renderText({
        populacao_reservatorio = populacoes[populacoes$COD_ANA == reservatorio_atual(),]$n_habitantes[1]
        sprintf("População atendida: %s", 
                populacao_reservatorio)
    })
    
    output$municipiosAtendidos <- renderText({
        cidades_atendidas = info_cidades_res[info_cidades_res$COD_ANA == reservatorio_atual(),]$Municipio
        sprintf("Municípios atendidos: %s", 
                paste(cidades_atendidas, collapse = ", "))
    })
    
    output$plotEscolhido <- renderPlotly({
        populacao_reservatorio = populacoes[populacoes$COD_ANA == reservatorio_atual(),]$n_habitantes[1]
        p = ver_consumo(reservatorio_atual(), sumario, populacao = populacao_reservatorio) + 
            ggplot2::xlim(ymd("2012-01-01", today()))
        ggplotly(p)
    })
    
    output$plotVolume <- renderPlotly({
        p = historicos %>% 
            filter(id == reservatorio_atual(), Ano >= 2012) %>% 
            ggplot(aes(x = Data, y = VolumePercentual)) + 
            ylab("Volume percentual") + 
            geom_area(fill = "#2b8cbe") + 
            xlim(ymd("2012-01-01", today()))
        ggplotly(p)
    })
})

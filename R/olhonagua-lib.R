
load_abastecimento <- function(arquivo = "olhonagua/abastecimento_municipio.csv"){
    library(readr)
    read_csv(arquivo, 
                     col_types = cols(.default = col_character())) %>% 
        filter(COD_ANA != "Não_monitorado" & !is.na(COD_ANA)) %>% 
        return()
}

get_repositorio <- function(id){
    library(jsonlite)
    library(lubridate)
    json = fromJSON(paste0("https://wwws-cloud.lsd.ufcg.edu.br:42160/api/reservatorios/", 
                           id, 
                           "/monitoramento"))$volumes 
    if(length(json) == 0){ # Não havia dados
        return(NULL)
    }
    serie = json %>% 
        mutate(Data = dmy(DataInformacao), 
               Ano = year(Data), 
               Mes = month(Data)) %>% 
        group_by(Ano, Mes) %>% 
        filter(day(Data) == max(day(Data))) %>% 
        ungroup() %>%
        distinct() %>% 
        select(-DataInformacao)
    
    serie$id = id 
    
    return(serie)
}

get_repositorios <- function(ids){
    repositorios = NULL
    for(id in ids){
        print(id)
        serie = get_repositorio(id)
        if(is.null(repositorios) & !is.null(serie)){
            repositorios = serie
        } else if(!is.null(serie)){
            repositorios = rbind(repositorios, serie) 
        } else { 
            print(paste("Sem dados para id=", id))
        }
    }
    return(repositorios)
}

limpa_e_add_sofrendo_delta <- function(historicos){
    historicos %>% 
        filter(Ano >= 2012, complete.cases(historicos)) %>% 
        group_by(id) %>% 
        arrange(Data) %>% 
        mutate(sofrendo = !any(Ano >= 2015 & VolumePercentual > 60), 
               Volume = as.numeric(Volume),
               delta = c(0, diff(Volume))) %>% 
        return()
}

filtra_decidas = function(historicos){
    d = historicos %>% 
        group_by(id) %>% 
        arrange(Data) %>% 
        filter(delta < 0 | 
                   lead(delta) < 0 | 
                   (lead(delta) == 0 & delta < 0) | 
                   (delta == 0 & lag(delta) <0)) 
    
     d %>% 
        group_by(id) %>% 
        arrange(Data) %>% 
        mutate(inicio = (Data == first(Data)) | 
                   delta > 0 |  
                   as.period(Data - lag(Data)) > days(60), 
               fim = (Data == last(Data)) | lead(delta) > 0)  %>% 
        ungroup() %>% 
        arrange(id, Data) %>% 
        mutate(sequencia = cumsum(inicio)) %>% 
        filter(abs(delta) < 100) %>% 
        return()
}

cria_seqs_inicio_fim <- function(decidas){
    decidas %>% group_by(sequencia) %>% 
        #filter(Data == first(Data) | Data == last(Data)) %>% 
        filter(n() >1) %>% 
        return()
}

get_consumo <- function(reservatorio, sumario){
    library(zoo)
    r = sumario %>% 
        filter(id == reservatorio) %>% 
        arrange(Data) %>% 
        group_by(sequencia) %>% 
        mutate(delta = c(diff(Volume)[1], diff(Volume)), 
               dias = c(30, diff(Data)),
               consumo = abs(delta)/dias * 1e9 # converte para litros
               #,consumo.r = rollmeanr(x = consumo, 2, na.pad = T, align = "right")
               )
    return(r)
}

get_consumos <- function(sumario){
    sumario %>% 
        arrange(id, sequencia, Data) %>% 
        group_by(sequencia) %>% 
        mutate(delta = c(diff(Volume)[1], diff(Volume)), 
               dias = c(30, diff(Data)),
               consumo = abs(delta)/dias * 1e9) %>%
    return()
}


# snis %>% filter(municipio == "Campina Grande", ano >= 2012) %>% mutate(consumo.pc = consumo * 1e6 / 365 / populacao_total_ibge) %>% View()

ver_consumo <- function(reservatorio, sumario, populacao = 5e5){
    get_consumo(reservatorio, sumario) %>% 
        mutate(`Consumo pessoa/dia` = abs(consumo) / populacao) %>% 
        ggplot(aes(x = Data, y = `Consumo pessoa/dia`)) + 
        geom_smooth(method = "loess", se = F, colour = "#abd9e9", size =2 ) + 
        geom_point(colour = "#d7191c", size = 1.5, alpha = 1) + 
        labs(x = "Mês", y = "Consumo estimado (litros por pessoa por dia)") %>% 
        return()
} # 12262



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

get_todos_repositorios <- function(id_vec){
    repositorios = NULL
    for(id in as.numeric(ids)){
        print(id)
        serie = get_repositorio(id)
        if(is.null(repositorios)){
            repositorios = serie
        }else{
            repositorios = rbind(repositorios, serie) 
        }
    }
    return(repositorios)
}
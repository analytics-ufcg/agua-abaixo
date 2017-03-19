library(dplyr)

cidade_municipio <- function(file_path_abastecimento_municipio, file_path_informacoes_gerais) {
  source("Abastecimento e Habitantes.R", encoding = "utf-8")
  abastecimento_municipio <- read.csv(file_path_abastecimento_municipio, fileEncoding = "utf-8")
  
  abastecimento_municipio$GEOCODIGO1 <- as.numeric(substr(abastecimento_municipio$GEOCODIGO,0,6))
  abastecimento_populacao <-merge(abastecimento_municipio, 
                        populacao(file_path_abastecimento_municipio, file_path_informacoes_gerais), 
                                  by.x ="GEOCODIGO1", by.y = "Geocodigo", all = FALSE)
  
  cidade_municipio <- abastecimento_populacao %>%
    group_by(GEOCODIGO1,COD_ANA,Pop_total,Pop_urbana_total, RESERVATORIO, Municipio) %>%
    summarise(n_monitorados = sum(COD_ANA !="Não_monitorado"), 
              n_nao_monitorados = sum(COD_ANA =="Não_monitorado" && COD_ANA =="")) %>%
    filter(n_monitorados > 0, n_nao_monitorados == 0, COD_ANA!="", !is.na(COD_ANA),
           GEOCODIGO1!="", !is.na(GEOCODIGO1)) %>%
    group_by(GEOCODIGO1) %>% 
    mutate(quantidade_reservatorios_municipio = n()) %>% 
    filter(quantidade_reservatorios_municipio==1) %>%
    select(GEOCODIGO1,COD_ANA,Pop_total,Pop_urbana_total, RESERVATORIO, Municipio)
  return(cidade_municipio)
}

habitantes_por_reservatorio <- function(file_path_abastecimento_municipio, 
                                        file_path_informacoes_gerais) {
  habitantes_por_reservatorio <- cidade_municipio(file_path_abastecimento_municipio, 
                                                  file_path_informacoes_gerais) %>%
    group_by(COD_ANA) %>%
    summarise(n_habitantes = sum(Pop_total), 
              n_habitantes_urbanos = sum(Pop_urbana_total))
  return(habitantes_por_reservatorio)
}
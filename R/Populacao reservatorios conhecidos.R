
populacao <- function(file_path_abastecimento_municipio, file_path_informacoes_gerais){
    library(dplyr, warn.conflicts = FALSE)
    abastecimento = read.csv(file_path_abastecimento_municipio, fileEncoding = "utf-8", stringsAsFactors = F)
    info_gerais = read.csv(file_path_informacoes_gerais, fileEncoding = "utf-8", stringsAsFactors = F)
    
    info_gerais_ultimas = filter(info_gerais, Ano.de.Referência == max(Ano.de.Referência))
    abastecimento$GEOCODIGO1 <- as.numeric(substr(abastecimento$GEOCODIGO,0,6))
    comparacao = right_join(info_gerais_ultimas, abastecimento, by = c("Código.do.Município" = "GEOCODIGO1"))
    k = rename(comparacao,
               Geocodigo = Código.do.Município,
               Municipio = Município,
               Estado = Estado,
               Pop_total = POP_TOT...População.total.do.município..Fonte..IBGE....Habitantes.,
               Pop_urbana_total = POP_URB...População.urbana.do.município..Fonte..IBGE...Habitantes.
    )
    
    result = distinct(k, Geocodigo, Municipio, Estado, Pop_total, Pop_urbana_total)
    return (result)
}


cidade_municipio <- function(file_path_abastecimento_municipio, file_path_informacoes_gerais) {
    library(dplyr, warn.conflicts = FALSE)
    abastecimento_municipio <- read.csv(file_path_abastecimento_municipio, fileEncoding = "utf-8", stringsAsFactors = F)
    
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
        select(GEOCODIGO1,COD_ANA,Pop_total,Pop_urbana_total, RESERVATORIO, Municipio) %>% 
        ungroup()
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